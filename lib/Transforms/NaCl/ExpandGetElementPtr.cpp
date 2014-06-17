//===- ExpandGetElementPtr.cpp - Expand GetElementPtr into arithmetic------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This pass expands out GetElementPtr instructions into ptrtoint,
// inttoptr and arithmetic instructions.
//
// This simplifies the language so that the PNaCl translator does not
// need to handle GetElementPtr and struct types as part of a stable
// wire format for PNaCl.
//
// Note that we drop the "inbounds" attribute of GetElementPtr.
//
//===----------------------------------------------------------------------===//

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/InstrTypes.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Constants.h"
#include "llvm/Pass.h"
#include "llvm/Transforms/NaCl.h"

using namespace llvm;

namespace {
  class ExpandGetElementPtr : public BasicBlockPass {
  public:
    static char ID; // Pass identification, replacement for typeid
    ExpandGetElementPtr() : BasicBlockPass(ID) {
      initializeExpandGetElementPtrPass(*PassRegistry::getPassRegistry());
    }

    virtual bool runOnBasicBlock(BasicBlock &BB);
  };
}

char ExpandGetElementPtr::ID = 0;
INITIALIZE_PASS(ExpandGetElementPtr, "expand-getelementptr",
                "Expand out GetElementPtr instructions into arithmetic",
                false, false)

Value *CastToPtrSize(Value *Val, Instruction *InsertPt, Type *PtrType) {
  unsigned ValSize = Val->getType()->getIntegerBitWidth();
  unsigned PtrSize = PtrType->getIntegerBitWidth();
  if (ValSize == PtrSize)
    return Val;
  Instruction *Inst;
  if (ValSize > PtrSize) {
    Inst = new TruncInst(Val, PtrType, "gep_trunc", InsertPt);
  } else {
    // GEP indexes must be sign-extended.
    Inst = new SExtInst(Val, PtrType, "gep_sext", InsertPt);
  }
  return CopyDebug(Inst, InsertPt);
}

Value* FlushOffset(Value* Ptr, uint64_t *CurrentOffset,
                   Instruction *InsertPt, Type *PtrType) {
  if (*CurrentOffset) {
    Ptr = BinaryOperator::Create(Instruction::Add, Ptr,
                                  ConstantInt::get(PtrType, *CurrentOffset),
                                  "gep", InsertPt);
    CopyDebug(Ptr, InsertPt);
    *CurrentOffset = 0;
  }
  return Ptr;
}

static void ExpandGEP(GetElementPtrInst *GEP, DataLayout *DL, Type *PtrType) {

  // We do some limited constant folding ourselves.  An alternative
  // would be to generate verbose, unfolded output (e.g. multiple
  // adds; adds of zero constants, multiple unneeded casts) and use a later pass such as
  // "-instcombine" to clean that up.  However, "-instcombine" can
  // reintroduce GetElementPtr instructions.

  Value* Operand = GEP->getPointerOperand();
  BitCastInst* BC = nullptr; // Delete this afterwards if it's otherwise dead.
  if((BC = dyn_cast<BitCastInst>(Operand))) {
    // If GEP's operand is a bitcast, use the BC's operand instead.
    Operand = BC->getOperand(0);
  }
  Value *Ptr = nullptr;
  IntToPtrInst* ITP = dyn_cast<IntToPtrInst>(Operand);
  if(ITP != nullptr && ITP->getOperand(0)->getType() == PtrType) {
    // If GEP's ptr operand is a ptrtoint, and the inttoptr's operand's type matches PtrType,
    // use the inttoptr's operand as our Ptr.
    Ptr = ITP->getOperand(0);
  } else {
    Ptr = new PtrToIntInst(Operand, PtrType,
                           "gep_int", GEP);
    CopyDebug(Ptr, GEP);
  }
  Value* OriginalPtr = Ptr;

  Type *CurrentTy = GEP->getPointerOperand()->getType();
  uint64_t CurrentOffset = 0;

  for (GetElementPtrInst::op_iterator Op = GEP->op_begin() + 1;
       Op != GEP->op_end();
       ++Op) {
    Value *Index = *Op;
    if (StructType *StTy = dyn_cast<StructType>(CurrentTy)) {
      uint64_t Field = cast<ConstantInt>(Op)->getZExtValue();
      CurrentTy = StTy->getElementType(Field);
      CurrentOffset += DL->getStructLayout(StTy)->getElementOffset(Field);
    } else {
      CurrentTy = cast<SequentialType>(CurrentTy)->getElementType();
      uint64_t ElementSize = DL->getTypeAllocSize(CurrentTy);
      if (ConstantInt *C = dyn_cast<ConstantInt>(Index)) {
        CurrentOffset += C->getSExtValue() * ElementSize;
      } else {
        Ptr = FlushOffset(Ptr, &CurrentOffset, GEP, PtrType);
        Index = CastToPtrSize(Index, GEP, PtrType);
        if (ElementSize != 1) {
          Index = CopyDebug(
              BinaryOperator::Create(Instruction::Mul, Index,
                                     ConstantInt::get(PtrType, ElementSize),
                                     "gep_array", GEP),
              GEP);
        }
        Ptr = BinaryOperator::Create(Instruction::Add, Ptr,
                                     Index, "gep", GEP);
        CopyDebug(Ptr, GEP);
      }
    }
  }
  Ptr = FlushOffset(Ptr, &CurrentOffset, GEP, PtrType);

  assert(CurrentTy == GEP->getType()->getElementType());
  Value* Result = nullptr;
  if(OriginalPtr != Ptr) {
    Result = new IntToPtrInst(Ptr, GEP->getType(), "", GEP);
    CopyDebug(Result, GEP);
    Result->takeName(GEP);
  } else if(Operand->getType() != GEP->getType()) {
    // This means the ptr offset is zero. Obviously, the types are different, so bitcast.
    Result = new BitCastInst(Operand, GEP->getType(), "", GEP);
    CopyDebug(Result, GEP);
    Result->takeName(GEP);
  } else {
    // This is a no-op GEP (ptr offset is zero).
    Result = Operand;
  }

  // Erase an unused Ptr (this can happen if we created a ptrtoint inst) if needed.
  if(Ptr->getNumUses() == 0 && isa<Instruction>(Ptr)) {
    cast<Instruction>(Ptr)->eraseFromParent();
  }

  GEP->replaceAllUsesWith(Result);
  GEP->eraseFromParent();

  if(BC != nullptr && BC->getNumUses() == 0) {
    BC->eraseFromParent();
  }
  if(ITP != nullptr && ITP->getNumUses() == 0) {
    ITP->eraseFromParent();
  }
}

// From ReplacePtrsWithInts.cpp:
void SimplifyCasts(Instruction *Inst, Type *IntPtrType, bool* Modified);

bool ExpandGetElementPtr::runOnBasicBlock(BasicBlock &BB) {
  bool Modified = false;
  DataLayout DL(BB.getParent()->getParent());
  Type *PtrType = DL.getIntPtrType(BB.getContext());

  for (BasicBlock::InstListType::iterator Iter = BB.begin();
       Iter != BB.end(); ) {
    Instruction *Inst = Iter++;
    if (GetElementPtrInst *GEP = dyn_cast<GetElementPtrInst>(Inst)) {
      Modified = true;
      ExpandGEP(GEP, &DL, PtrType);
    } else {
      SimplifyCasts(Inst, PtrType, &Modified);
    }
  }
  return Modified;
}

BasicBlockPass *llvm::createExpandGetElementPtrPass() {
  return new ExpandGetElementPtr();
}
