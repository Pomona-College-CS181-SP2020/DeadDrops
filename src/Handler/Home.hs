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
import System.Directory (createDirectoryIfMissing)

-- Define our data that will be used for creating the form.
data FileForm = FileForm
    { fileInfo :: FileInfo
    , downloadFrom :: Day
    , downloadTo :: Day
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

uploadDirectory :: FilePath
uploadDirectory = "temp"

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
          Just fileInfo1 -> do
            saveMeas (fileInfo fileInfo1) ( (Data.Text.unpack  $ (appFileUploadDirectory $ appSettings master) ++ "/" ++ (Data.Text.Encoding.decodeUtf8 $ encode randomBS)) ++ "/data")
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

getDownloadR :: Handler Html
getDownloadR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe FileForm
        handlerName = "getHomeR" :: Text
    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "downloadpage")

postDownloadR :: Handler Html
postDownloadR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "downloadpage")

sampleForm :: Form FileForm
sampleForm = renderBootstrap3 BootstrapBasicForm $ FileForm
    <$> fileAFormReq "Upload a file"
    <*> areq (jqueryDayField def
       { jdsChangeYear = True -- give a year dropdown
       , jdsYearRange = "2020:+20" -- 2020 till 20 years from now
        }) "Download Window Start Date " Nothing
    <*> areq (jqueryDayField def
       { jdsChangeYear = True -- give a year dropdown
        , jdsYearRange = "2020:+20" -- 2020 till 20 years from now
       }) "Download Window End Date " Nothing
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

commentIds :: (Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")
