{-| 
 Module: Pipes
 Description: Infix reversed-function-application operator. '$|' is analogous to shell pipes.
-}
module Pipe where

infixl 0 $|
a $| b = b a
