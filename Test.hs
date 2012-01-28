    {-# LANGUAGE OverloadedStrings #-}

    import Happstack.Lite 
    import qualified Data.ByteString.Lazy.Char8 as L

    main :: IO ()
    main = do
        serve Nothing hello 

    hello :: ServerPart Response
    hello = ok $ toResponse ("Hello" :: L.ByteString)
