module CodeGen where

import Algebra
import Library.VMAssemblyLangAST

codeGen :: CAlgebra p me s e v env en st typ
codeGen = CAlgebra
    dProgram

    dMemberBlock
    dMemberDecl
    dMemberStat
    dMemberFunc
    dMemberEnum
    dMemberStruct
    dMemberInc
    dMemberTypedef

    dEnum

    dStruct

    dDef11
    dDef12
    dDef2
    dDef31
    dDef32

    dStatBlock
    dStatDecl
    dStatExp
    dStatRet
    dStatIfEl
    dStatWhi

    dBinExp
    dUnaExp1
    dUnaExp2
    dFuncCall
    dLitInt
    dLitChar
    dLitDouble
    dLitVar
    dLitPointer
    dLitArray

    dVar
    dArrayVar 
    dUnumVar 
    dStructVar 

dProgram = undefined
dMemberBlock = undefined
dMemberDecl = undefined
dMemberStat = undefined
dMemberFunc = undefined
dMemberEnum = undefined
dMemberStruct = undefined
dMemberInc = undefined
dMemberTypedef = undefined

dEnum = undefined

dStruct = undefined

dDef11 = undefined
dDef12 = undefined
dDef2 = undefined
dDef31 = undefined
dDef32 = undefined

dStatBlock = undefined
dStatDecl = undefined
dStatExp = undefined
dStatRet = undefined
dStatIfEl = undefined
dStatWhi = undefined

dBinExp = undefined
dUnaExp1 = undefined
dUnaExp2 = undefined
dFuncCall = undefined
dLitInt = undefined
dLitChar = undefined
dLitDouble = undefined
dLitVar = undefined
dLitPointer = undefined
dLitArray = undefined

dVar = undefined
dArrayVar  = undefined
dUnumVar  = undefined
dStructVar  = undefined