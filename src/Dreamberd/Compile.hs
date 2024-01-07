module Dreamberd.Compile (
    compileAst,
) where

import Dreamberd.Types (AstNode (..))
import Dreamberd.Vm (Insts (..), Value (..))

compileAst :: [AstNode] -> Either String [Insts]
compileAst _ = Right [Push (Dreamberd.Vm.String "cramptarea will be back"), Ret]
