//===- SimplifyAllocas.cpp - Simplify allocas to arrays of bytes         --===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//===----------------------------------------------------------------------===//

#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/Transforms/NaCl.h"

using namespace llvm;

struct SimplifyAllocas : public BasicBlockPass {
  static char ID; // Pass identification, replacement for typeid
  SimplifyAllocas() : BasicBlockPass(ID) {
    initializeSimplifyAllocasPass(*PassRegistry::getPassRegistry());
  }

  const Module *M  = nullptr;
  Type* IntPtrType = nullptr;
  Type* Int8Type   = nullptr;
  std::unique_ptr<DataLayout> DL;

  using llvm::Pass::doInitialization;
  bool doInitialization(Function &F) override {
    if (M != F.getParent()) {
      M = F.getParent();
      DL.reset(new DataLayout(M));
      IntPtrType = DL->getIntPtrType(M->getContext());
      Int8Type = Type::getInt8Ty(M->getContext());
    }
    return this->BasicBlockPass::doInitialization(F);
  }

  bool runOnBasicBlock(BasicBlock &BB) override {
    SmallVector<std::tuple<AllocaInst*,     // old alloca
                           AllocaInst*,     // new alloca
                           BinaryOperator*, // if non-null, mul array
                           BitCastInst*>,   // bitcast -> expected type
                8> BCs;

    bool Changed = false;
    for(auto I = BB.getFirstInsertionPt(); I != BB.end(); I++) {
      if(!isa<AllocaInst>(I)) {
        continue;
      }
      Changed = true;
      AllocaInst* Alloca = cast<AllocaInst>(I);
      Type *ElementTy = Alloca->getType()->getPointerElementType();
      Type* ArraySizeTy = Alloca->getArraySize()->getType();
      Constant *ElementSize = ConstantInt::get(ArraySizeTy,
                                               DL->getTypeAllocSize(ElementTy));
      // Expand out alloca's built-in multiplication.
      Value *MulSize;
      if (ConstantInt *C = dyn_cast<ConstantInt>(Alloca->getArraySize())) {
        const auto Value = C->getValue().zextOrTrunc(ArraySizeTy->getScalarSizeInBits());
        MulSize = ConstantExpr::getMul(ElementSize,
                                       ConstantInt::get(ArraySizeTy,
                                                        Value));
      } else {
        MulSize = CopyDebug(BinaryOperator::Create(Instruction::Mul,
                                                   ElementSize,
                                                   Alloca->getArraySize(),
                                                   Alloca->getName() + ".alloca_mul"),
                            Alloca);
      }
      unsigned Alignment = Alloca->getAlignment();
      if (Alignment == 0)
        Alignment = DL->getPrefTypeAlignment(ElementTy);
      AllocaInst *Tmp = CopyDebug(new AllocaInst(Int8Type,
                                                 MulSize,
                                                 Alignment,
                                                 ""),
                                  Alloca);
      Tmp->takeName(Alloca);
      auto* BC = CopyDebug(new BitCastInst(Tmp, Alloca->getType(), Tmp->getName() + ".bc"),
                           Alloca);
      Alloca->replaceAllUsesWith(BC);
      BCs.push_back(std::make_tuple(Alloca,
                                    Tmp,
                                    dyn_cast<BinaryOperator>(MulSize),
                                    BC));
    }

    // Insert the various new instructions into the BB:
    // I use multiple loops because I'd like to keep the instructions grouped
    // together. This makes it easier for me to read while diff-ing.
    Instruction* IPt = BB.getFirstNonPHI();
    for(auto I : BCs) {
      AllocaInst* New = std::get<1>(I);
      BinaryOperator* MulSize = std::get<2>(I);

      if(MulSize == nullptr) {
        New->insertBefore(IPt);
      }
    }

    for(auto I : BCs) {
      BinaryOperator* MulSize = std::get<2>(I);

      if(MulSize != nullptr) {
        MulSize->insertBefore(IPt);
      }
    }

    for(auto I : BCs) {
      AllocaInst* New = std::get<1>(I);
      BinaryOperator* MulSize = std::get<2>(I);

      if(MulSize != nullptr) {
        New->insertBefore(IPt);
      }
    }

    for(auto I : BCs) {
      BitCastInst* BC = std::get<3>(I);
      BC->insertBefore(IPt);
    }

    for(auto I : BCs) {
      AllocaInst* Old = std::get<0>(I);
      Old->eraseFromParent();
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
