module Library.AssemblyLanguageAST where


data Registers =    PointerIndex
                    | Reg1

type Code = String

prettyPrinter :: Code -> String
prettyPrinter x = "undefined"