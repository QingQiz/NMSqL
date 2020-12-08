module CodeGenerator where

import Ast (Expr, Select)
import CodeGeneratorUtils (CodeGenEnv, SelectResultType)

cExprWrapper :: Expr -> CodeGenEnv
cSelectWrapper :: Select -> SelectResultType -> CodeGenEnv