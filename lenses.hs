{-# LANGUAGE ScopedTypeVariables #-}
module Main where



main :: IO ()
main = do
  putStrLn $ getter (Person "david" 30 (Location "city" "state")) personNameLens
  print $ do
    setter (Person "david" 30 (Location "city" "state")) personNameLens $ "lol"
  print $ do
    setter (Person "Peter" 32 (Location "Sandby" "Sealand")) personCityLens $ "Copenhagen"

data Lens object field
  = Lens
  { get :: object -> field
  , set :: object -> field -> object
  }

data Person
  = Person
  { name :: String
  , age :: Int
  , location :: Location
  } deriving (Show)

data Location
  = Location
    { city :: String
    , state :: String
    } deriving Show

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

addressCityLens :: Lens Location String
addressCityLens = Lens city updateCity
  where
    updateCity :: Location -> String -> Location
    updateCity l newCity = l { city = newCity }

addressStateLens :: Lens Location String
addressStateLens = Lens state updateState
  where
    updateState :: Location -> String -> Location
    updateState l newState = l { state = newState }

personLocationLens :: Lens Person Location
personLocationLens = Lens location updateLocation
  where
    updateLocation :: Person -> Location -> Person
    updateLocation p newLocation = p { location = newLocation }

composeLenses :: forall a b c . Lens b c -> Lens a b -> Lens a c
composeLenses lens2 lens1 = lens3
  where
      getLens2 :: b -> c
      getLens2 = get lens2

      setLens2 :: b -> c -> b
      setLens2 = set lens2

      getLens1 :: a -> b
      getLens1 = get lens1

      setLens1 :: a -> b -> a
      setLens1 = set lens1

      lens3 = Lens { get = \(o :: a) -> getLens2 . getLens1 $ o
                   , set = \(o :: a) -> \(f :: c) -> setLens1 o (setLens2 (getLens1 o) f)
                   }

personCityLens :: Lens Person String
personCityLens = composeLenses addressCityLens personLocationLens

setter :: object -> Lens object field -> field -> object
setter object (Lens _ setter') newField = setter' object newField

getter :: object -> Lens object field -> field
getter object (Lens getter' _) = getter' object

_1 :: Lens (a, b) a
_1 = Lens { get = \(a, b) -> a
          , set = \(a, b) -> \newA -> (newA, b)
          }

_2 :: Lens (a, b) b
_2 = Lens { get = \(a, b) -> b
          , set = \(a, b) -> \newB -> (a, newB)
          }

