# haskellTips
```haskell
module Main where

main :: IO ()
main = do
  putStrLn $ (Person "david" 30) `getter` personNameLens
  print $ do
    (Person "david" 30 `setter` personNameLens) $ "lol"

data Lens object field
  = Lens
  { get :: object -> field
  , set :: object -> field -> object
  }

data Person
  = Person
  { name :: String
  , age :: Int
  } deriving (Show)

personNameLens :: Lens Person String
personNameLens = Lens name updateName
   where
     updateName :: Person -> String -> Person
     updateName p newName = p { name = newName }

personAgeLens :: Lens Person Int
personAgeLens = Lens age updateAge
   where
       updateAge :: Person -> Int -> Person
       updateAge p newAge = p { age = newAge }

setter :: object -> Lens object field -> field -> object
setter object (Lens _ setter') newField = setter' object newField

getter :: object -> Lens object field -> field
getter object (Lens getter' _) = getter' object
```
