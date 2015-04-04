A Haskell library for expressing and deriving stack prisms.

A stack prism is a bidirectional isomorphism that is partial in the backward
direction. These prisms are compatible with the
[lens](http://hackage.haskell.org/package/lens) library.

Stack prisms can express constructor-deconstructor pairs. For example:

```haskell
nil :: StackPrism t ([a] :- t)
nil = stackPrism f g
  where
    f t = [] :- t
    g ([] :- t) = Just t
    g _ = Nothing

cons :: StackPrism (a :- [a] :- t) ([a] :- t)
cons = stackPrism f g
  where
    f (x :- xs :- t) = (x : xs) :- t
    g ((x : xs) :- t) = Just (x :- xs :- t)
    g _ = Nothing
```

Here `:-` can be read as 'cons', forming a stack of values. For example, `nil`
pushes `[]` onto the stack; or, in the backward direction, tries to remove `[]`
from the stack. `cons` takes a head `x` and tail `xs` from the stack and pushes
`x : xs` onto the stack, or, in the backward direction, takes `x : xs` from the
stack and replaces it with its two individual components.

Every constructor has its own stack prism version. You don't have to write them
by hand; you can automatically generate them, either using Template Haskell
(see module [`Data.StackPrism.TH`](http://hackage.haskell.org/package/stack-
prism/docs/Data-StackPrism-TH.html)) or using GHC generic programming (see
module [`Data.StackPrism.Generic`](http://hackage.haskell.org/package/stack-
prism/docs/Data-StackPrism-Generic.html)).

Representing constructor-destructor pairs as stack manipulators allows them to be composed more easily.
