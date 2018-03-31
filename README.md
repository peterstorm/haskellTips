# haskellTips
```haskell
data Lens object field 
  = Lens 
  { get :: object -> field
  , set :: object -> field -> object 
  }
```
```haskell
set :: object -> Lens object field -> field -> object
set object (Lens _ setter) newField = setter object newField
```
```haskell
set :: objectType -> Lens objectType fieldType -> fieldType -> objectType
set object (Lens _ setter) newField = 
  (setter :: objectType -> fieldType -> objectType) object newField
```
```haskell
get :: object -> Lens object field -> field 
get object (Lens getter _) = getter object
```
```haskell
personNameLens :: Lens Person String
personNameLens = Lens name updateName
   where
     updateName :: Person -> String -> Person
     updateName p newName = p { name = newName } 
```
```haskell
personAgeLens :: Lens Person Int
personAgeLens = Lens age updateAge
   where
       updateAge :: Person -> Int -> Person
       updateAge p newAge = p { age = newAge }
```       
```haskell
Person "david" 30 `set` personNameLens $ "lol"
> "david"
```
```haskell
Person "david" 30 `get` personNameLens 
```
