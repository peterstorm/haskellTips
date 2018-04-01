## haskellTips
HaskellTips is a place where I will write down the tips I learn, througout my journey in Haskell.

conceptually typeclasses are like java interfaces, if you’ve ever used Java. It allows objects to make instances of a class, and then all members of the class become usable on any object.
A small example
```data StopLight = Red | Yellow | Green

class ShowMe a where
  showMe :: a -> String

instance ShowMe StopLight where
  showMe Red = "red"
  showMe Yellow = "yellow"
  showMe Green = "green"

main :: IO ()
main = putStrLn (showMe Red)```
(edited)
this is a trivial example, but illustrates the point
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
-- f . (g . h)  =  (f . g) . h```
