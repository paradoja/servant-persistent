{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE DeriveGeneric              #-}

module Models where

import Data.Aeson                  (ToJSON, FromJSON, toJSON, genericToJSON, defaultOptions)
import GHC.Generics                (Generic)
import Control.Monad.Reader        (ReaderT, asks, liftIO)
import Database.Persist.Sql        (SqlPersistT)
import Database.Persist.Postgresql (SqlBackend(..), runMigration,
                                    runSqlPool)
import Database.Persist.TH         (share, mkPersist, sqlSettings,
                                    mkMigrate, persistLowerCase)
import Control.Monad.IO.Class      (MonadIO)
import Control.Monad.Reader.Class  (MonadReader)
import Config

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User
    name String
    email String
    deriving Show
|]

doMigrations :: ReaderT SqlBackend IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
    pool <- asks getPool
    liftIO $ runSqlPool query pool

data Person = Person
    { name :: String
    , email :: String
    } deriving (Eq, Show, Generic)

instance ToJSON Person where
    -- needed due to error on Aeson that creates warning
    toJSON = genericToJSON defaultOptions
instance FromJSON Person

userToPerson :: User -> Person
userToPerson User{..} = Person { name = userName, email = userEmail }
