module Library.VMAssemblyLangAST where


data Registers =    TempReg
                    | Var1
                    | Var2
                    | Result
                    | IP
                    | SP

type Ident = String
type Value = Int

data Opcodes =  DEFINE Ident
                | SET Ident
                | LOAD Ident
                | PUSH Value
                | READ
                | POP
                | ADD
                | SUB
                | MUL
                | DIV
                | READ_REG Value
                | LOAD_REG Value
                | LABEL Ident [Ident]
                | JUMP1 Ident
                | JUMP2 Value
                | RET
                | BEGIN
                | HALT


type Code = Opcodes

prettyPrinter :: Code -> String
prettyPrinter (DEFINE id) = "DEFINE " ++ show id
prettyPrinter (SET id) = "SET " ++ show id
prettyPrinter (LOAD id) = "LOAD " ++ show id
prettyPrinter (PUSH val) = "PUSH " ++ show val
prettyPrinter READ = "READ"
prettyPrinter POP = "POP"
prettyPrinter ADD = "ADD"
prettyPrinter SUB = "SUB"
prettyPrinter MUL = "MUL"
prettyPrinter DIV = "DIV"
prettyPrinter (READ_REG val) = "READ_REG " ++ show val
prettyPrinter (LOAD_REG val) = "LOAD_REG" ++ show val
prettyPrinter (LABEL id ids) = "LABEL " ++ show id ++ foldr (\x -> (++ " " ++ show x)) "" ids
prettyPrinter (JUMP1 id) = "JUMP " ++ show id
prettyPrinter (JUMP2 val) = "JUMP " ++ show val
prettyPrinter BEGIN = "BEGIN"
prettyPrinter HALT = "HALT"