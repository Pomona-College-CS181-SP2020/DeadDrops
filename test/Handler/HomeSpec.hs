{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.HomeSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

    describe "Homepage" $ do
      it "loads the index and checks it looks right" $ do
          get HomeR
          statusIs 200
          htmlAnyContain "h1" "Welcome"

          request $ do
              setMethod "POST"
              setUrl HomeR
              addToken
              fileByLabelContain "a file" "test/Spec.hs" "text/plain" -- talk about self-reference
              byLabelExact "Upload a file" "localhost"

          -- more debugging printBody
          htmlAllContain "upload-response" "text/plain"
          htmlAllContain "upload-response" "available"
