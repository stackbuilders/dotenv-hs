import Test.Hspec

import qualified Configuration.DotenvSpec as Dotenv
import qualified Configuration.Dotenv.ParseSpec as DotenvParse

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Dotenv" Dotenv.spec
    describe "DotenvParse" DotenvParse.spec
