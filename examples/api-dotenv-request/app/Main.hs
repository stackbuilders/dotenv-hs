{-# LANGUAGE OverloadedStrings #-}

import Configuration.Dotenv (loadFile, defaultConfig)
import System.Environment (lookupEnv)
import Network.HTTP.Req
import qualified Data.Text as T
import Data.Aeson

main :: IO ()
main = do
    loadFile defaultConfig
    
    --  Loading API URI from dotenv 
    maybeApiEndpoint <- lookupEnv "API_URI"

    case maybeApiEndpoint of
        Just apiEndpoint -> do
            -- Making the request
            -- For this example the api used is: jsonplaceholder.typicode.com
            let apiEndpointURL = https (T.pack apiEndpoint) /: T.pack "posts" /: T.pack "1"
            
            --  Fake user agent
            let userAgent = "Haskell Network.HTTP.Req"
            let request :: Req (JsonResponse Value)
                request = req GET apiEndpointURL NoReqBody jsonResponse $ header ("User-Agent") (userAgent)


            -- Obtaining the response
            response <- runReq defaultHttpConfig $ do
                responseStatusCode <$> request

            print response
        Nothing -> putStrLn "API_ENDPOINT environment variable not set."