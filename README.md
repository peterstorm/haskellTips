# haskellTips
```
data Lens object field 
  = Lens 
  { get :: object -> field
  , set :: object -> field -> object 
  }
```
```
set :: object -> Lens object field -> field -> object
set object (Lens _ setter) newField = setter object newField
```
```
get :: object -> Lens object field -> field 
get object (Lens getter _) = getter object
```
```
personNameLens :: Lens Person String
personNameLens = Lens name updateName
   where
     updateName :: Person -> String -> Person
     updateName p newName = p { name = newName } 
```
```
personAgeLens :: Lens Person Int
personAgeLens = Lens age updateAge
   where
       updateAge :: Person -> Int -> Person
       updateAge p newAge = p { age = newAge }
```       
