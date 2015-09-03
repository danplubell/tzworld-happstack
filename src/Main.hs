module Main where
import Happstack.Server (dir,
                         nullConf,
                         simpleHTTP,
                         toResponse,
                         toResponseBS,
                         ok,
                         badRequest,
                         Response,
                         ServerPart,
                         lookBS)
import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BS
import Data.TZworld.Api
import Data.Aeson

main::IO()
main = simpleHTTP nullConf tzworldApp


tzworldApp::ServerPart Response
tzworldApp = msum
             [ dir "location" locationParams,
               noQueryParams               
             ]

noQueryParams::ServerPart Response
noQueryParams = badRequest ( toResponse "No location provided")

locationParams::ServerPart Response
locationParams = do
  mLat <- optional $ lookBS "lat"      
  mLon <- optional $ lookBS "lon" 
  case  (mLat,mLon) of
      (Nothing, Nothing)   -> sendBadRequest "No longitude or latitude given"
      (Nothing, _)         -> sendBadRequest "No latitude given"
      (_, Nothing)         -> sendBadRequest "No longitude given"
      (Just lat, Just lon) -> do 
         tz <- liftIO $ handleLocation (BL.toStrict lat) (BL.toStrict lon)
         case tz of
            Left str  -> sendBadRequest str
            Right tzm -> ok $ toResponseBS (BS.pack "application/json") ( encode tzm)  

sendBadRequest::String -> ServerPart Response
sendBadRequest s  = badRequest (toResponseBS (BS.pack "application/json") (encode $ Message s))

sendOK::String -> ServerPart Response
sendOK s = ok (toResponse s) 
