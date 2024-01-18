module Dreamberd.Compilation.Postprocessing (
    executePostprocessing,
) where

import Dreamberd.Vm (Builtin (..), Call (..), Insts (..), Value (..))

executePostprocessing :: [Insts] -> [Insts]
executePostprocessing = map postprocessBuiltins

postprocessBuiltins :: Insts -> Insts
postprocessBuiltins (PushEnv "input") = Push (Symbol (Builtin Input))
postprocessBuiltins (PushEnv "print") = Push (Symbol (Builtin Print))
postprocessBuiltins (PushEnv "error") = Push (Symbol (Builtin Error))
postprocessBuiltins (DefineEnv name redef (Just (Lambda args insts))) = DefineEnv name redef (Just (Lambda args (map postprocessBuiltins insts)))
postprocessBuiltins inst = inst
