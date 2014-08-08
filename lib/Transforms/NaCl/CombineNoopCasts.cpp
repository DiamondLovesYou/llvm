//===- CombineNoopCasts.cpp - Remove redundant casts for diff-ing        --===//
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

struct CombineNoopCasts : public BasicBlockPass {
  static char ID; // Pass identification, replacement for typeid
  CombineNoopCasts() : BasicBlockPass(ID) {
    initializeCombineNoopCastsPass(*PassRegistry::getPassRegistry());
  }

  const Module *M = nullptr;
  Type* IntPtrType = nullptr;

  using llvm::Pass::doInitialization;
  bool doInitialization(Function &F) override {
    if (M != F.getParent()) {
      M = F.getParent();
      IntPtrType = DataLayout(M).getIntPtrType(M->getContext());
    }
    return this->BasicBlockPass::doInitialization(F);
  }

  bool runOnBasicBlock(BasicBlock &BB) override;
};
char CombineNoopCasts::ID = 0;

INITIALIZE_PASS(CombineNoopCasts, "combine-noop-casts",
                "Combine redundant casts, for diff-ing",
                false, false)

bool CombineNoopCasts::runOnBasicBlock(BasicBlock &BB) {
  bool Changed = false;

  for (BasicBlock::iterator BBI = BB.begin(), BBE = BB.end(); BBI != BBE;) {
    Instruction* Inst = BBI++;
    if (IntToPtrInst *Cast1 = dyn_cast<IntToPtrInst>(Inst)) {
      if (isa<PtrToIntInst>(Cast1->getOperand(0)) ||
          (isa<ConstantExpr>(Cast1->getOperand(0)) &&
           cast<ConstantExpr>(Cast1->getOperand(0))->getOpcode() == Instruction::PtrToInt)) {
        User *Cast2 = cast<User>(Cast1->getOperand(0));
        Value *V = Cast2->getOperand(0);
        if(Cast2->getType() != IntPtrType) {
          continue;
        }
        if (V->getType() != Cast1->getType())
          V = CopyDebug(new BitCastInst(V, Cast1->getType(),
                                        V->getName() + ".bc", Cast1),
                        Cast2);
        Cast1->replaceAllUsesWith(V);
        if (Cast1->use_empty())
          Cast1->eraseFromParent();
        if (Cast2->use_empty() && isa<Instruction>(Cast2))
          cast<Instruction>(Cast2)->eraseFromParent();

        Changed = true;
      }
    } else if(PtrToIntInst *Cast1 = dyn_cast<PtrToIntInst>(Inst)) {
      if(Cast1->getType() != IntPtrType) {
        continue;
      }
      if (isa<IntToPtrInst>(Cast1->getOperand(0)) ||
          (isa<ConstantExpr>(Cast1->getOperand(0)) &&
           cast<ConstantExpr>(Cast1->getOperand(0))->getOpcode() == Instruction::IntToPtr)) {
        User *Cast2 = cast<User>(Cast1->getOperand(0));
        Value *V = Cast2->getOperand(0);
        Cast1->replaceAllUsesWith(V);
        if (Cast1->use_empty())
          Cast1->eraseFromParent();
        if (Cast2->use_empty() && isa<Instruction>(Cast2))
          cast<Instruction>(Cast2)->eraseFromParent();

        Changed = true;
      } else if(BitCastInst *Cast2 = dyn_cast<BitCastInst>(Cast1->getOperand(0))) {
        Cast1->setOperand(0, Cast2->getOperand(0));
        if (Cast2->use_empty())
          Cast2->eraseFromParent();
      }
    } else if(BitCastInst* Cast1 = dyn_cast<BitCastInst>(Inst)) {
      if(Cast1->getOperand(0)->getType() == Cast1->getType()) {
        Cast1->replaceAllUsesWith(Cast1->getOperand(0));
        Cast1->eraseFromParent();
      } else if(BitCastInst* Cast2 = dyn_cast<BitCastInst>(Cast1->getOperand(0))) {
        if(Cast1->getType() == Cast2->getOperand(0)->getType()) {
          Cast1->replaceAllUsesWith(Cast2->getOperand(0));
          Cast1->eraseFromParent();
          
          if (Cast2->use_empty()) {
            Cast2->eraseFromParent();
          }
        }
      }
    }
  }

  // de-duplicate bitcasts:
  DenseMap<std::pair<Value*, Type*>, BitCastInst*> BCs;
  for (BasicBlock::iterator BBI = BB.begin(), BBE = BB.end(); BBI != BBE;) {
    Instruction* Inst = BBI++;

    if(PtrToIntInst *Cast1 = dyn_cast<PtrToIntInst>(Inst)) {
      if(Cast1->use_empty()) {
        Cast1->eraseFromParent();
        continue;
      }
    }

    if(!isa<BitCastInst>(Inst)) {
      continue;
    }

    BitCastInst* BC = cast<BitCastInst>(Inst);
    auto Val = std::make_pair(BC->getOperand(0), BC->getType());
    auto BI = BCs.find(Val);
    if(BI == BCs.end()) {
      BCs.insert(std::make_pair(Val, BC));
    } else {
      BC->replaceAllUsesWith(BI->second);
      BC->eraseFromParent();
    }
  }

  return Changed;
}

BasicBlockPass *llvm::createCombineNoopCastsPass() {
  return new CombineNoopCasts();
}
