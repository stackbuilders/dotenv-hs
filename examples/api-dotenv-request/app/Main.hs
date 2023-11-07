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
            let apiEndpointURL = https (T.pack apiEndpoint) /: T.pack "posts" /: T.pack "1"
            
            --  Fake user agent
            let userAgent = "Mozilla/5.0 (Linux; Android 5.0.1; Nokia 1100 LTE Build/GRK39F) AppleWebKit/536.14 (KHTML, like Gecko)  Chrome/55.0.3442.377 Mobile Safari/600.3"
            let request :: Req (JsonResponse Value)
                request = req GET apiEndpointURL NoReqBody jsonResponse $ header ("User-Agent") (userAgent)


            -- Obtaining the response
            response <- runReq defaultHttpConfig $ do
                responseStatusCode <$> request

            print response
        Nothing -> putStrLn "API_ENDPOINT environment variable not set."