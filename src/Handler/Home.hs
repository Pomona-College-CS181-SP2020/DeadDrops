{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Yesod.Form.Jquery
import Text.Julius (RawJS (..))
import Control.Monad              (join)
import Control.Monad.Catch        (throwM)
import Control.Monad.CryptoRandom
import Control.Monad.Error
import Crypto.Random (SystemRandom, newGenIO)
import Data.ByteString.Base16 (encode)
import Data.Text                                           (unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Encoding         (decodeUtf8)
import UnliftIO.Exception         (catch)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import Data.Time
import Data.ByteString.Lazy.UTF8 as BLU (toString)
import Data.List ((!!))
import Data.Char (chr)

-- Define our data that will be used for creating the form.
data FileForm = FileForm
    { fileInfo :: FileInfo
    , downloadFrom :: Day
    , downloadFromTime :: TimeOfDay
    , downloadTo :: Day
    , downloadEndTime :: TimeOfDay
    , fileDescription :: Text
    }

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe FileForm
        handlerName = "getHomeR" :: Text
        nonce = undefined
    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

getRetrieveR :: String -> String -> Handler ()
getRetrieveR nonce filename = do
  master <- getYesod
  sendFile "text/html" ((Data.Text.unpack (appFileUploadDirectory $ appSettings master)) ++ "/" ++ nonce ++ "/data/"++ filename)

instance MonadError GenError Handler where
            throwError = throwM
            catchError = catch
instance MonadCRandom GenError Handler where
            getCRandom = wrap crandom
            {-# INLINE getCRandom #-}
            getBytes i = wrap (genBytes i)
            {-# INLINE getBytes #-}
            getBytesWithEntropy i e = wrap (genBytesWithEntropy i e)
            {-# INLINE getBytesWithEntropy #-}
            doReseed bs = do
                genRef <- fmap randGen getYesod
                join $ liftIO $ atomicModifyIORef genRef $ \gen ->
                    case reseed bs gen of
                        Left e -> (gen, throwM e)
                        Right gen' -> (gen', return ())
            {-# INLINE doReseed #-}

wrap :: (SystemRandom -> Either GenError (a, SystemRandom)) -> Handler a
wrap f = do
      genRef <- fmap randGen getYesod
      join $ liftIO $ atomicModifyIORef genRef $ \gen ->
          case f gen of
                Left e -> (gen, throwM e)
                Right (x, gen') -> (gen', return x)

postHomeR :: Handler Html
postHomeR = do
    master <- getYesod
    ((result, formWidget), formEnctype) <- runFormPost sampleForm

    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing
    randomBS <- getBytes 16
    case submission of
          Just fileForm -> do
            let prefix = (Data.Text.unpack $ (appFileUploadDirectory $ appSettings master) ++ "/" ++ (Data.Text.Encoding.decodeUtf8 $ encode randomBS))
            saveMeas (fileInfo fileForm)  (prefix ++ "/data")
            makeMetadataFile fileForm prefix
    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
            nonce = randomBS
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

saveMeas :: FileInfo -> FilePath -> HandlerT App IO (FilePath)
saveMeas file dest = do
    let filename = Import.unpack $ fileName file
        dest' = dest </> filename
    liftIO $ createDirectoryIfMissing True dest
    liftIO $ fileMove file dest'
    return filename

makeMetadataFile :: FileForm -> FilePath -> HandlerT App IO (FilePath)
makeMetadataFile fileForm filePath = do
    let startTime = downloadFromTime fileForm
        endTime = downloadEndTime fileForm
        startDate = downloadFrom fileForm
        endDate = downloadTo fileForm
        filename = Import.unpack $ fileName (fileInfo fileForm)
        dest' = filePath </> "meta.meta"
        utctimeStart = todToUTCTime startTime startDate
        utctimeEnd = todToUTCTime endTime endDate
        fileStartDate = (show utctimeStart) ++ "\n"
        fileEndDate = (show utctimeEnd) ++ "\n"
    currTime <- liftIO getCurrentTimeZone
    -- utcStart <- localToUTCTimeOfDay (currTime) startTime
    -- utcEnd <- localToUTCTimeOfDay currTime endTime
    liftIO $ writeFile dest' ((Import.fromString filename ++ "\n") ++ (Import.fromString fileStartDate) ++ (Import.fromString fileEndDate))
    return filename

todToUTCTime :: TimeOfDay -> Day -> UTCTime
todToUTCTime tod day = UTCTime day (timeOfDayToTime tod)

bsToStr :: ByteString -> String
bsToStr = map (chr . fromEnum) . Import.unpack

getDownloadR :: String -> Handler Html
getDownloadR nonce = do
    master <- getYesod
    directoryExists <- liftIO $ doesDirectoryExist ((Data.Text.unpack $ (appFileUploadDirectory $ appSettings master)) ++ "/" ++ nonce)
    if directoryExists
        then
           do
                fileContents <- liftIO $ readFile ("/var/yesod-upload/" ++ nonce ++ "/meta.meta")
                let arr = (Import.lines (bsToStr fileContents))
                    fileName =  (arr!!0)

                let submission = Nothing :: Maybe FileForm
                    handlerName = "getHomeR" :: Text
                    downloadUrl = "/files/" ++ nonce ++"/data/" ++ fileName :: String
                defaultLayout $ do
                    setTitle "Welcome To Yesod!"
                    $(widgetFile "downloadpage")

    else
        do
                defaultLayout $ do
                    let (commentFormId, commentTextareaId, commentListId) = commentIds
                    aDomId <- newIdent
                    setTitle "Welcome To Yesod!"
                    $(widgetFile "errorpage")




    -- (formWidget, formEnctype) <- generateFormPost sampleForm
    -- let submission = Nothing :: Maybe FileForm
    --     handlerName = "getHomeR" :: Text
    -- defaultLayout $ do
    --     let (commentFormId, commentTextareaId, commentListId) = commentIds
    --     aDomId <- newIdent
    --     setTitle "Welcome To Yesod!"
    --     $(widgetFile "downloadpage")


sampleForm :: Form FileForm
sampleForm = renderBootstrap3 BootstrapBasicForm $ FileForm
    <$> fileAFormReq "Upload a file"
    -- <*> areq (jqueryDayField def
    --    { jdsChangeYear = True -- give a year dropdown
    --    , jdsYearRange = "2020:+20" -- 2020 till 20 years from now
    --     }) "Download Window Start Date " Nothing
    <*> areq (jqueryDayField def
       { jdsChangeYear = True -- give a year dropdown
        , jdsYearRange = "2020:+20" -- 2020 till 20 years from now
       }) "Download Window Start Date " Nothing
    <*> areq timeFieldTypeTime startTimeSettings Nothing
    <*> areq (jqueryDayField def
       { jdsChangeYear = True -- give a year dropdown
        , jdsYearRange = "2020:+20" -- 2020 till 20 years from now
       }) "Download Window End Date " Nothing
    <*> areq timeFieldTypeTime endTimeSettings Nothing
    <*> areq textField textSettings Nothing
    -- Add attributes like the placeholder and CSS classes.
    where textSettings = FieldSettings
            { fsLabel = "What's on the file?"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control")
                , ("placeholder", "File description")
                ]
            }
          startTimeSettings = FieldSettings
            { fsLabel = "Start time"
              , fsTooltip = Nothing
              , fsId = Nothing
              , fsName = Nothing
              , fsAttrs =
                [ ("class", "form-control")
                , ("current time", "This is a test!")
                ]
            }
          endTimeSettings = FieldSettings
            { fsLabel = "End time"
              , fsTooltip = Nothing
              , fsId = Nothing
              , fsName = Nothing
              , fsAttrs =
                [ ("class", "form-control")
                , ("current time", "This is a test!")
                ]
            }

commentIds :: (Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")
