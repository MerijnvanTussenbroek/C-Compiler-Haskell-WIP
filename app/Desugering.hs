module Desugering where

import AbstractSyntax
import Algebra

-- this here is for desugering the datastructure before we start folding over it
-- this way, I don't have to add semantics for all possible operators

{--
desugerar = cFolder desugeringAlgebra ()

desugeringAlgebra :: CAlgebra Program Members Statements Expression Variable ()
desugeringAlgebra = CAlgebra

--}