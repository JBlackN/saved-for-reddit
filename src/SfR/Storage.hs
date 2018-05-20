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

import Control.Monad (forM_, liftM)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString as BS hiding (map)
import Data.ByteString.Base64 as B64
import Data.Int (Int64)
import Data.Text as T hiding (map)
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH

import SfR.Config (db_file, sfr_config)
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
  body String Maybe
  subreddit String
  score Int
  createdUtc Double
  userId UserId
  UniqueSavedItem userId identifier
  deriving Show
|]

get_or_create_user :: String -> IO (Int64, User)
get_or_create_user username = do
  db_file <- liftM db_file sfr_config
  runSqlite (T.pack db_file) $ do
    runMigration migrateAll
    sessionKey <- liftIO $ liftM toString nextRandom
    maybeUser <- getBy $ UniqueUser username
    case maybeUser of
      Nothing -> do
        userId <- insert $ User username sessionKey
        Just user <- get userId
        return $ (fromSqlKey userId, user)
      Just (Entity userId user) -> do
        update userId [UserSessionKey =. sessionKey]
        Just user <- get userId
        return $ (fromSqlKey userId, user)

get_user_from_session :: String -> IO (Maybe (Entity User))
get_user_from_session session_key = do
  db_file <- liftM db_file sfr_config
  runSqlite (T.pack db_file) $ do
    runMigration migrateAll
    getBy $ UniqueSessionKey session_key

get_user_saved :: String -> IO [SavedItem]
get_user_saved username = do
  db_file <- liftM db_file sfr_config
  runSqlite (T.pack db_file) $ do
    runMigration migrateAll
    maybeUser <- getBy $ UniqueUser username
    case maybeUser of
      Nothing                   -> return []
      Just (Entity userId user) -> do
        entities <- selectList [SavedItemUserId ==. userId] [Desc SavedItemCreatedUtc]
        return $ map (\(Entity savedItemId savedItem) -> savedItem) entities

normalize_saved :: Int64 -> [SavedPostData] -> [SavedCommentData] -> [SavedItem]
normalize_saved userId posts comments =
  (map normalize_post posts) ++ (map normalize_comment comments)
  where normalize_post post =
          SavedItem { savedItemIdentifier = TP.name post
                    , savedItemAuthor = TP.author post
                    , savedItemParentAuthor = Nothing
                    , savedItemThumbnail = let thumbnail = TP.thumbnail post
                                           in case thumbnail of
                                                "self"    -> Nothing
                                                "default" -> Nothing
                                                otherwise -> Just thumbnail
                    , savedItemTitle = TP.title post
                    , savedItemLink = TP.url post
                    , savedItemPermalink = TP.permalink post
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
                    , savedItemLink = TC.link_url comment
                    , savedItemPermalink = TC.link_permalink comment
                    , savedItemBody = Just (TC.body_html comment)
                    , savedItemSubreddit = TC.subreddit comment
                    , savedItemScore = TC.score comment
                    , savedItemCreatedUtc = TC.created_utc comment
                    , savedItemUserId = toSqlKey userId
                    }

update_saved :: [SavedItem] -> IO ()
update_saved saved_items = do
  db_file <- liftM db_file sfr_config
  runSqlite (T.pack db_file) $ do
    runMigration migrateAll
    forM_ saved_items (\item -> do
        maybeItem <- getBy $
          UniqueSavedItem (savedItemUserId item) (savedItemIdentifier item)
        case maybeItem of
          Nothing   -> insert $ item
          Just (Entity itemId item) -> return itemId
      )
