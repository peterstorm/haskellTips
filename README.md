## haskellTips
HaskellTips is a place where I will write down the tips I learn, througout my journey in Haskell.

Conceptually typeclasses are like java interfaces, if you’ve ever used Java. It allows objects to make instances of a class, and then all members of the class become usable on any object.
A small example:
```haskell
data StopLight = Red | Yellow | Green

class ShowMe a where
  showMe :: a -> String

instance ShowMe StopLight where
  showMe Red = "red"
  showMe Yellow = "yellow"
  showMe Green = "green"

main :: IO ()
main = putStrLn (showMe Red)
```

This is a trivial example, but illustrates the point
In Haskell, all typeclasses should have laws associated with them, even if they are very trivial
So going back to our Lens example
Lenses form a Category, and we can use a typeclass (with associated laws) to capture this fact. We can even prove the correctness of our Lens Category instance using equational reasoning.
This is what `Category` looks like

```class Category cat where
  id :: cat a a
  (<<<) :: cat b c -> cat a b -> cat a c

-- (right identity)
-- f . id  =  f

-- (left identity)
-- id . f  =  f

-- (associativity)
-- f . (g . h)  =  (f . g) . h
```
Lots of algebraic structures can be used as typeclasses in Haskell
Monad, Monoid, Functor, etc.
Algebraic structures (specifically Monad), can be used to model computation, this particular fact was discovered by Eugenio Moggi, https://core.ac.uk/download/pdf/21173011.pdf
> We take category theory as a general theory of functions and develop on top a categorical semantics of computations based on monads
So you’ll see a lot of the motivating types, `Either`, `Except`, `State`, `IO`, `[]` (List), were derived from this fact (among others)

So now, going back to `Category`

It’s important to note that *anything* can be considered a `Category`, so long as it obeys the category laws. Similar to how `String` and `Product` (like an Int used for multiplication), are `Monoids`, but are completely different things, but both obey the `Monoid` laws, so it’s good.
You might be thinking, “But I don’t know how to write my own Monads?“, that’s ok, most of the time when you program in Haskell you’re taking advantage of other peoples discoveries, the algebraic structures they’ve found, the laws associated with them, and you can equationally reason about the proofs. So don’t get too hard on yourself if you’re not writing your own data types and instances. Learn what’s there first. Oleg didn’t become Oleg over night.
@Peter Storm so now that you’ve seen the `Category` class, the `ShowMe` class, and how to make an instance of the `ShowMe` class, using what you’ve defined already `composeLens`, can you make an instance of the `Category` class for `Lens` (edited)
well, before we do that
it might be beneficial for you to write the identity lens

### Lenses
```haskell
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

main :: IO ()
main = do
  putStrLn $ getter (Person "david" 30 (Location "city" "state")) personNameLens
  print $ do
    setter (Person "david" 30 (Location "city" "state")) personNameLens $ "lol"
  print $ do
    setter (Person "Peter" 32 (Location "Sandby" "Sealand")) personCityLens $ "Copenhagen"

class Category cat where
  identity :: cat a a
  (<<<) :: cat b c -> cat a b -> cat a c

-- (right identity)
-- f . id  =  f

-- (left identity)
-- id . f  =  f

-- (associativity)
-- f . (g . h)  =  (f . g) . h

instance Category Lens where
  identity = Lens { get = \(o :: a) -> id o
                    , set = \(o :: a) -> \(f :: a) -> const o f
                    }

  (<<<) lens2 lens1 = lens3
    where
      getLens2 = get lens2
      setLens2 = set lens2
      getLens1 = get lens1
      setLens1 = set lens1
      lens3 = Lens { get = \(o :: a) -> getLens2 . getLens1 $ o
                   , set = \(o :: a) -> \(f :: c) -> setLens1 o (setLens2 (getLens1 o) f)
                   }

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

personCityLens :: Lens Person String
personCityLens = composeLenses addressCityLens personLocationLens

personStateLens :: Lens Person String
personStateLens = composeLenses addressStateLens personLocationLens

identityLens :: Lens a a
identityLens = Lens { get = \(o :: a) -> id o
                    , set = \(o :: a) -> \(f :: a) -> const o f
                    }

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
```
