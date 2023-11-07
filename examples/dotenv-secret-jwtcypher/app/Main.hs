import Jose.Jws (hmacEncode)
import Jose.Jwa (JwsAlg(HS256))
import qualified Data.ByteString.Char8 as B
import Configuration.Dotenv (loadFile, defaultConfig)
import System.Environment (lookupEnv)

main :: IO ()
main = do
    loadFile defaultConfig

    --  Loading secret from .env
    maybeSecret <- lookupEnv "SECRET"

    case maybeSecret of
        Just secret -> do
            -- Encoding based on .env secret
            let eitherEncode = hmacEncode HS256 (B.pack secret) (B.pack "my JSON message")

            case eitherEncode of
                Right jt -> putStrLn $ "Encoded JWT: " <> show jt
                Left _ -> putStrLn "Nothing to show :("

        Nothing -> putStrLn "SECRET environment variable not set."
    

