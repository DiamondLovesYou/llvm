//===-- ExpandUtils.cpp - Helper functions for expansion passes -----------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Constants.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/NaCl.h"

using namespace llvm;

Instruction *llvm::PhiSafeInsertPt(Use *U) {
  Instruction *InsertPt = cast<Instruction>(U->getUser());
  if (PHINode *PN = dyn_cast<PHINode>(InsertPt)) {
    // We cannot insert instructions before a PHI node, so insert
    // before the incoming block's terminator.  This could be
    // suboptimal if the terminator is a conditional.
    InsertPt = PN->getIncomingBlock(*U)->getFirstInsertionPt();
  }
  return InsertPt;
}

void llvm::PhiSafeReplaceUses(Use *U, Value *NewVal) {
  if (PHINode *PN = dyn_cast<PHINode>(U->getUser())) {
    // A PHI node can have multiple incoming edges from the same
    // block, in which case all these edges must have the same
    // incoming value.
    BasicBlock *BB = PN->getIncomingBlock(*U);
    for (unsigned I = 0; I < PN->getNumIncomingValues(); ++I) {
      if (PN->getIncomingBlock(I) == BB)
        PN->setIncomingValue(I, NewVal);
    }
  } else {
    U->getUser()->replaceUsesOfWith(U->get(), NewVal);
  }
}

Function *llvm::RecreateFunction(Function *Func, FunctionType *NewType) {
  Function *NewFunc = Function::Create(NewType, Func->getLinkage());
  NewFunc->copyAttributesFrom(Func);
  Func->getParent()->getFunctionList().insert(Func, NewFunc);
  NewFunc->takeName(Func);
  NewFunc->getBasicBlockList().splice(NewFunc->begin(),
                                      Func->getBasicBlockList());
  Func->replaceAllUsesWith(
      ConstantExpr::getBitCast(NewFunc,
                               Func->getFunctionType()->getPointerTo()));
  return NewFunc;
}

void llvm::setGlobalVariableValue(Module &M, const char *Name,
                                  Constant *Value) {
  GlobalVariable *Var = M.getNamedGlobal(Name);
  if (Var != nullptr) {
    if (Var->hasInitializer()) {
      report_fatal_error(std::string("Variable ") + Name +
                         " already has an initializer");
    }
    Var->replaceAllUsesWith(ConstantExpr::getBitCast(Value, Var->getType()));
    Var->eraseFromParent();
  }
}
void llvm::CopyInstMD(Instruction *Dest, Instruction *Src) {
  SmallVector<std::pair<unsigned, MDNode*>, 4> MDs;
  Src->getAllMetadata(MDs);
  for(auto MD : MDs) {
    Dest->setMetadata(MD.first, MD.second);
  }
}
