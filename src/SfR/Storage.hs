{-|
Module     : SfR.Storage
Description: Web application's storage
Copyright  : (c) Petr Schmied, 2018
License    : MIT
Maintainer : peter9209@gmail.com
Stability  : stable
Portability: portable

Module defines data types and methods for web application's storage management.

'migrateAll' generates database migration for both entities below.

= Data types of database entities

['User']: Application user.

    ['UserId']: User's ID.
    ['userUsername']: User's username.
    ['userSessionKey']: User's session key (see 'getOrCreateUser').
    ['UniqueUser']: Unique constraint on user's username.
    ['UniqueSessionKey']: Unique constraint on users' session keys.

['SavedItem']: Saved item ( [Reddit](https://www.reddit.com) post or comment)
               stored by application.

    ['SavedItemId']: Saved item's ID.
    ['savedItemIdentifier']: Saved item's [Reddit](https://www.reddit.com)
                             identifier.
    ['savedItemAuthor']: Saved item's author.
    ['savedItemParentAuthor']: Saved comment's parent post author.
    ['savedItemThumbnail']: Saved post's thumbnail URL.
    ['savedItemTitle']: Saved post's or comment's parent post's title.
    ['savedItemLink']: Saved item's link.
    ['savedItemPermalink']: Saved item's permalink.
    ['savedItemBody']: Saved item's body as raw HTML.
    ['savedItemSubreddit']: Saved item's subreddit.
    ['savedItemScore']: Saved item's score (upvotes &#x2212; downvotes).
    ['savedItemCreatedUtc']: Saved item's created date and time as UNIX
                             timestamp.
    ['savedItemUserId']: 'UserId' of 'User' to whom the saved item belongs.
    ['UniqueSavedItem']: Unique constraint on saved item's 'UserId' and
                         'savedItemIdentifier', i. e. no duplicate saved
                         items stored for the same user.
-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module SfR.Storage where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.ByteString as BS hiding (map)
import Data.ByteString.Base64 as B64
import Data.Int (Int64)
import Data.Text as T hiding (map)
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import GHC.Generics

import SfR.Config (db_file, sfrConfig)
import SfR.Reddit.Types.Comment as TC
import SfR.Reddit.Types.Post as TP

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
  username String
  sessionKey String
  accessToken String Maybe
  UniqueUser username
  UniqueSessionKey sessionKey
  deriving Show
SavedItem
  identifier String
  author String
  parentAuthor String Maybe
  thumbnail String Maybe
  title String
  link String
  permalink String
  body Text Maybe
  subreddit String
  score Int
  createdUtc Int64
  userId UserId
  UniqueSavedItem userId identifier
  deriving Show Generic Eq
|]

-- | Export 'SavedItem' to JSON.
instance ToJSON SavedItem

-- | Gets or creates application's user.
--
-- Runs 'migrateAll' database migration.
--
-- (1) Generates new session key (UUID) for user.
-- (2) Creates and returns user if (s)he doesn't exist. Updates (session key
--     and access token) and returns user otherwise. Also returns user ID.
getOrCreateUser :: String -- ^ [Reddit](https://www.reddit.com) username.
                -> String -- ^ [Reddit](https://www.reddit.com) access token.
                -> IO (Int64, User) -- ^ User ID and 'User' record.
getOrCreateUser username accessToken = do
  db_file <- db_file <$> sfrConfig
  runSqlite (T.pack db_file) $ do
    runMigration migrateAll
    sessionKey <- liftIO $ toString <$> nextRandom
    maybeUser <- getBy $ UniqueUser username
    case maybeUser of
      Nothing -> do
        userId <- insert $ User username sessionKey (Just accessToken)
        Just user <- get userId
        return (fromSqlKey userId, user)
      Just (Entity userId user) -> do
        update userId [UserSessionKey =. sessionKey,
                       UserAccessToken =. Just accessToken]
        Just user <- get userId
        return (fromSqlKey userId, user)

-- | Gets user from the application's session.
--
-- Runs 'migrateAll' database migration.
--
-- Uses session key (UUID) to lookup corresponding 'User'.
getUserFromSession :: String -- ^ Session key (UUID).
                   -> IO (Maybe (Entity User)) -- ^ User if exists.
getUserFromSession session_key = do
  db_file <- db_file <$> sfrConfig
  runSqlite (T.pack db_file) $ do
    runMigration migrateAll
    getBy $ UniqueSessionKey session_key

-- | Gets user's saved items.
--
-- Runs 'migrateAll' database migration.
--
-- (1) Gets user by username.
-- (2) If user doesn't exist, returns empty list.
-- (3) Otherwise returns her/his saved items sorted by created date and time
--     (descending).
getUserSaved :: String -- ^ Application user's username.
             -> IO [SavedItem] -- ^ User's saved items.
getUserSaved username = do
  db_file <- db_file <$> sfrConfig
  runSqlite (T.pack db_file) $ do
    runMigration migrateAll
    maybeUser <- getBy $ UniqueUser username
    case maybeUser of
      Nothing                   -> return []
      Just (Entity userId user) ->
        map (\(Entity savedItemId savedItem) -> savedItem) <$>
          selectList [SavedItemUserId ==. userId] [Desc SavedItemCreatedUtc]

-- | Normalizes saved posts and comments.
--
-- Also assigns them to application's user by her/his user ID.
normalizeSaved :: Int64 -- ^ Application user's ID.
               -> [SavedPostData] -- ^ User's saved posts.
               -> [SavedCommentData] -- ^ User's saved comments.
               -> [SavedItem] -- ^ Normalized user's saved items.
normalizeSaved userId posts comments =
  map normalize_post posts ++ map normalize_comment comments
  where normalize_post post =
          SavedItem { savedItemIdentifier = TP.name post
                    , savedItemAuthor = TP.author post
                    , savedItemParentAuthor = Nothing
                    , savedItemThumbnail = let thumbnail = TP.thumbnail post
                                           in case thumbnail of
                                                "self"    -> Nothing
                                                "default" -> Nothing
                                                "image"   -> Nothing
                                                "nsfw"    -> Nothing
                                                "spoiler" -> Nothing
                                                _         -> Just thumbnail
                    , savedItemTitle = TP.title post
                    , savedItemLink = TP.url post
                    , savedItemPermalink = (normalize_link . TP.permalink) post
                    , savedItemBody = TP.selftext_html post
                    , savedItemSubreddit = TP.subreddit post
                    , savedItemScore = TP.score post
                    , savedItemCreatedUtc = TP.created_utc post
                    , savedItemUserId = toSqlKey userId
                    }
        normalize_comment comment =
          SavedItem { savedItemIdentifier = TC.name comment
                    , savedItemAuthor = TC.author comment
                    , savedItemParentAuthor = Just (TC.link_author comment)
                    , savedItemThumbnail = Nothing
                    , savedItemTitle = TC.link_title comment
                    , savedItemLink = TC.link_permalink comment
                    , savedItemPermalink = (normalize_link . TC.permalink) comment
                    , savedItemBody = Just (TC.body_html comment)
                    , savedItemSubreddit = TC.subreddit comment
                    , savedItemScore = TC.score comment
                    , savedItemCreatedUtc = TC.created_utc comment
                    , savedItemUserId = toSqlKey userId
                    }
        normalize_link = (++) "https://www.reddit.com"

-- | Updates saved items in application's database.
--
-- Runs 'migrateAll' database migration.
--
-- Inserts only unique items, see 'UniqueSavedItem'.
updateSaved :: [SavedItem] -- ^ Saved items.
            -> IO ()
updateSaved saved_items = do
  db_file <- db_file <$> sfrConfig
  runSqlite (T.pack db_file) $ do
    runMigration migrateAll
    forM_ saved_items (\item -> do
        maybeItem <- getBy $
          UniqueSavedItem (savedItemUserId item) (savedItemIdentifier item)
        case maybeItem of
          Nothing   -> insert item
          Just (Entity itemId item) -> return itemId
      )
