{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where
import Web.Scotty 
import Data.Aeson 
import Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Vector as V 
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K 
import qualified Data.ByteString as BS



main :: IO ()
main = do 
    putStrLn "Hello shitty world"
    scotty 8000 $ do 
        get "/" $ do 
            sendHomePage 



sendHomePage :: ActionM ()
sendHomePage = do 
    indexDotHtml <- liftIO $ readFile "frontend/index.html"
    html (Builder.toLazyText $ Builder.fromText $ T.pack $ indexDotHtml)



              






        
