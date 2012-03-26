{-# LANGUAGE OverloadedStrings #-}

main :: IO ()
main = do
    putStrLn "HI"
    print mbe


-- This example will escape!
-- because of the definition of >>?
mbe :: Maybe String
mbe = do
    one <- Nothing
    two <- Just "two"
    three <- Just "three"
    return two


either :: Either Bool String
either = do
    one <- Right "one"
    two <- Right "two"
    three <- Right "three"
    return two

--test :: Either Bool String
--test = do

class Response where
    response :: (ToJSON b) => a -> (Status, b)

instance (ToJSON a) => Response a where
    response a = (status200, a)

instance (Response a, Response b) => (Either a b) where
    response (Left a) = response a
    response (Right b) = response b