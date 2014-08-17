//===- SimplifyAllocas.cpp - Simplify allocas to arrays of bytes         --===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Simplify all allocas into allocas of byte arrays.
//
//===----------------------------------------------------------------------===//

#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/Transforms/NaCl.h"

using namespace llvm;

struct SimplifyAllocas : public BasicBlockPass {
  static char ID; // Pass identification, replacement for typeid
  SimplifyAllocas() : BasicBlockPass(ID), DL("") {
    initializeSimplifyAllocasPass(*PassRegistry::getPassRegistry());
  }

  const Module *M  = nullptr;
  Type* IntPtrType = nullptr;
  Type* Int8Type   = nullptr;
  DataLayout DL;

  using llvm::Pass::doInitialization;
  bool doInitialization(Function &F) override {
    if (M != F.getParent()) {
      M = F.getParent();
      DL.reset(M->getDataLayoutStr());
      IntPtrType = DL.getIntPtrType(M->getContext());
      Int8Type = Type::getInt8Ty(M->getContext());
      return true;
    }
    return this->BasicBlockPass::doInitialization(F);
  }

  bool runOnBasicBlock(BasicBlock &BB) override {
    bool Changed = false;
    for(auto I = BB.getFirstInsertionPt(); I != BB.end();) {
      if(AllocaInst* Alloca = dyn_cast<AllocaInst>(I++)) {
        Changed = true;
        Type *ElementTy = Alloca->getType()->getPointerElementType();
        Constant *ElementSize = ConstantInt::get(IntPtrType,
                                                 DL.getTypeAllocSize(ElementTy));
        // Expand out alloca's built-in multiplication.
        Value *MulSize;
        if (ConstantInt *C = dyn_cast<ConstantInt>(Alloca->getArraySize())) {
          const APInt Value =
            C->getValue().zextOrTrunc(IntPtrType->getScalarSizeInBits());
          MulSize = ConstantExpr::getMul(ElementSize,
                                         ConstantInt::get(IntPtrType, Value));
        } else {
          Value *ArraySize = Alloca->getArraySize();
          if (ArraySize->getType() != IntPtrType) {
            ArraySize = CastInst::CreateIntegerCast(ArraySize,
                                                    IntPtrType,
                                                    // TODO(rdiamond): this is an assumption.
                                                    false,
                                                    "",
                                                    Alloca);
          }
          MulSize =
            CopyDebug(BinaryOperator::Create(Instruction::Mul, ElementSize,
                                             ArraySize,
                                             Alloca->getName() + ".alloca_mul",
                                             Alloca),
                      Alloca);

        }
        unsigned Alignment = Alloca->getAlignment();
        if (Alignment == 0)
          Alignment = DL.getPrefTypeAlignment(ElementTy);
        AllocaInst *Tmp = CopyDebug(new AllocaInst(Int8Type,
                                                   MulSize,
                                                   Alignment,
                                                   "",
                                                   Alloca),
                                    Alloca);
        Tmp->takeName(Alloca);
        auto* BC = CopyDebug(new BitCastInst(Tmp, Alloca->getType(),
                                             Tmp->getName() + ".bc",
                                             Alloca),
                             Alloca);
        Alloca->replaceAllUsesWith(BC);
        Alloca->eraseFromParent();
      }
    }
    return Changed;
  }
};
char SimplifyAllocas::ID = 0;

INITIALIZE_PASS(SimplifyAllocas, "simplify-allocas",
                "Simplify allocas to arrays of bytes",
                false, false)

BasicBlockPass *llvm::createSimplifyAllocasPass() {
  return new SimplifyAllocas();
}
