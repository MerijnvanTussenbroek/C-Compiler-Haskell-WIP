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
                | LABEL Ident
                | JUMP1 Ident
                | JUMP2 Value
                | RET
                | BEGIN
                | NOTHING
                | HALT


type Code = [Opcodes]

prettyPrinter :: Code -> String
prettyPrinter ((DEFINE id):xs) = "DEFINE " ++ id ++ "\n" ++ prettyPrinter xs
prettyPrinter ((SET id):xs) = "SET " ++ id ++ "\n" ++ prettyPrinter xs
prettyPrinter ((LOAD id):xs) = "LOAD " ++ id ++ "\n" ++ prettyPrinter xs
prettyPrinter ((PUSH val):xs) = "PUSH " ++ show val ++ "\n" ++ prettyPrinter xs
prettyPrinter (READ:xs) = "READ" ++ "\n" ++ prettyPrinter xs
prettyPrinter (POP:xs) = "POP" ++ "\n" ++ prettyPrinter xs
prettyPrinter (ADD:xs) = "ADD" ++ "\n" ++ prettyPrinter xs
prettyPrinter (SUB:xs) = "SUB" ++ "\n" ++ prettyPrinter xs
prettyPrinter (MUL:xs) = "MUL" ++ "\n" ++ prettyPrinter xs
prettyPrinter (DIV:xs) = "DIV" ++ "\n" ++ prettyPrinter xs
prettyPrinter ((READ_REG val):xs) = "READ_REG " ++ show val ++ "\n" ++ prettyPrinter xs
prettyPrinter ((LOAD_REG val):xs) = "LOAD_REG" ++ show val ++ "\n" ++ prettyPrinter xs
prettyPrinter ((LABEL id):xs) = "LABEL " ++ id ++ "\n" ++ prettyPrinter xs
prettyPrinter ((JUMP1 id):xs) = "JUMP " ++ id ++ "\n" ++ prettyPrinter xs
prettyPrinter ((JUMP2 val):xs) = "JUMP " ++ show val ++ "\n" ++ prettyPrinter xs
prettyPrinter (RET:xs) = "RET" ++ "\n" ++ prettyPrinter xs
prettyPrinter (BEGIN:xs) = "BEGIN" ++ "\n" ++ prettyPrinter xs
prettyPrinter (HALT:xs) = "HALT" ++ "\n" ++ prettyPrinter xs
prettyPrinter [] = ""