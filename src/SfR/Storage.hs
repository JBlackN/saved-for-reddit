{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
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

instance ToJSON SavedItem

getOrCreateUser :: String -> IO (Int64, User)
getOrCreateUser username = do
  db_file <- db_file <$> sfrConfig
  runSqlite (T.pack db_file) $ do
    runMigration migrateAll
    sessionKey <- liftIO $ toString <$> nextRandom
    maybeUser <- getBy $ UniqueUser username
    case maybeUser of
      Nothing -> do
        userId <- insert $ User username sessionKey
        Just user <- get userId
        return (fromSqlKey userId, user)
      Just (Entity userId user) -> do
        update userId [UserSessionKey =. sessionKey]
        Just user <- get userId
        return (fromSqlKey userId, user)

getUserFromSession :: String -> IO (Maybe (Entity User))
getUserFromSession session_key = do
  db_file <- db_file <$> sfrConfig
  runSqlite (T.pack db_file) $ do
    runMigration migrateAll
    getBy $ UniqueSessionKey session_key

getUserSaved :: String -> IO [SavedItem]
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

normalizeSaved :: Int64 -> [SavedPostData] -> [SavedCommentData] -> [SavedItem]
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

updateSaved :: [SavedItem] -> IO ()
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
