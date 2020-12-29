module CodeGenerator where

import Ast
import CodeGeneratorUtils (CodeGenEnv, SelectResultType)

cExprWrapper        :: Expr        -> CodeGenEnv
cSelectWrapper      :: Select      -> SelectResultType -> CodeGenEnv
cIndexActionWrapper :: IndexAction -> CodeGenEnv
cTableActionWrapper :: TableActon  -> CodeGenEnv
cDeleteWrapper      :: Delete      -> CodeGenEnv
cInsertWrapper      :: Insert      -> CodeGenEnv
cUpdateWrapper      :: Update      -> CodeGenEnv