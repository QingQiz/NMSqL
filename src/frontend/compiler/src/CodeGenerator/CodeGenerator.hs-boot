module CodeGenerator where

import Ast ( Expr )
import CodeGeneratorUtils ( CodeGenEnv )

cExprWrapper :: Expr -> CodeGenEnv