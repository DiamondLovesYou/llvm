//===- ExpandStructRegs.cpp - Expand out variables with struct type--------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This pass expands out some uses of LLVM variables
// (a.k.a. registers) of struct type.  It replaces loads and stores of
// structs with separate loads and stores of the structs' fields.  The
// motivation is to omit struct types from PNaCl's stable ABI.
//
// ExpandStructRegs does not yet handle all possible uses of struct
// values.  It is intended to handle the uses that Clang and the SROA
// pass generate.  Clang generates struct loads and stores, along with
// extractvalue instructions, in its implementation of C++ method
// pointers, and the SROA pass sometimes converts this code to using
// insertvalue instructions too.
//
// ExpandStructRegs does not handle:
//
//  * Nested struct types.
//  * Array types.
//  * Function types containing arguments or return values of struct
//    type without the "byval" or "sret" attributes.  Since by-value
//    struct-passing generally uses "byval"/"sret", this does not
//    matter.
//
// Other limitations:
//
//  * ExpandStructRegs does not attempt to use memcpy() where that
//    might be more appropriate than copying fields individually.
//  * ExpandStructRegs does not preserve the contents of padding
//    between fields when copying structs.  However, the contents of
//    padding fields are not defined anyway.
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/Pass.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/NaCl.h"

using namespace llvm;

namespace {
  struct ExpandStructRegs : public FunctionPass {
    static char ID; // Pass identification, replacement for typeid
    ExpandStructRegs() : FunctionPass(ID) {
      initializeExpandStructRegsPass(*PassRegistry::getPassRegistry());
    }

    virtual bool runOnFunction(Function &F);
  };
}

char ExpandStructRegs::ID = 0;
INITIALIZE_PASS(ExpandStructRegs, "expand-struct-regs",
                "Expand out variables with struct types", false, false)


static bool DoAnotherPass(Type* Ty) {
  return isa<ArrayType>(Ty) || isa<StructType>(Ty);
}
static bool DoAnotherPass(Value* V) {
  return DoAnotherPass(V->getType());
}

template <class T>
static void SplitUpPHINode(PHINode *Phi, T* Ty, bool& NeedsAnotherPass) {
  Value *NewStruct = UndefValue::get(Ty);
  Instruction *NewStructInsertPt = Phi->getParent()->getFirstInsertionPt();

  // Create a separate PHINode for each struct field.
  for (unsigned Index = 0; Index < Ty->getNumElements(); ++Index) {
    SmallVector<unsigned, 1> EVIndexes;
    EVIndexes.push_back(Index);

    Type* ElemTy = NULL;
    if(StructType* STy = dyn_cast<StructType>(Ty)) {
      ElemTy = STy->getElementType(Index);
    } else if(ArrayType* ATy = dyn_cast<ArrayType>(Ty)) {
      ElemTy = ATy->getElementType();
    }
    NeedsAnotherPass = NeedsAnotherPass || DoAnotherPass(ElemTy);

    PHINode *NewPhi = PHINode::Create(
        ElemTy, Phi->getNumIncomingValues(),
        Phi->getName() + ".index", Phi);
    CopyDebug(NewPhi, Phi);
    for (unsigned PhiIndex = 0; PhiIndex < Phi->getNumIncomingValues();
         ++PhiIndex) {
      BasicBlock *IncomingBB = Phi->getIncomingBlock(PhiIndex);
      Value *EV = CopyDebug(
          ExtractValueInst::Create(
              Phi->getIncomingValue(PhiIndex), EVIndexes,
              Phi->getName() + ".extract", IncomingBB->getTerminator()), Phi);
      NewPhi->addIncoming(EV, IncomingBB);
    }

    // Reconstruct the original struct value.
    NewStruct = CopyDebug(
        InsertValueInst::Create(NewStruct, NewPhi, EVIndexes,
                                Phi->getName() + ".insert", NewStructInsertPt),
        Phi);
  }
  Phi->replaceAllUsesWith(NewStruct);
  Phi->eraseFromParent();
}
template <class T>
static void SplitUpSelect(SelectInst *Select, T* Ty, bool& NeedsAnotherPass) {
  Value *NewStruct = UndefValue::get(Ty);

  // Create a separate SelectInst for each struct field.
  for (unsigned Index = 0; Index < Ty->getNumElements(); ++Index) {
    SmallVector<unsigned, 1> EVIndexes;
    EVIndexes.push_back(Index);

    Value *TrueVal = CopyDebug(
        ExtractValueInst::Create(Select->getTrueValue(), EVIndexes,
                                 Select->getName() + ".extract", Select),
        Select);
    Value *FalseVal = CopyDebug(
        ExtractValueInst::Create(Select->getFalseValue(), EVIndexes,
                                 Select->getName() + ".extract", Select),
        Select);
    Value *NewSelect = CopyDebug(
        SelectInst::Create(Select->getCondition(), TrueVal, FalseVal,
                           Select->getName() + ".index", Select),
        Select);
    NeedsAnotherPass = NeedsAnotherPass || DoAnotherPass(NewSelect);

    // Reconstruct the original struct value.
    NewStruct = CopyDebug(
        InsertValueInst::Create(NewStruct, NewSelect, EVIndexes,
                                Select->getName() + ".insert", Select),
        Select);
  }
  Select->replaceAllUsesWith(NewStruct);
  Select->eraseFromParent();
}

template <class InstType>
static void ProcessLoadOrStoreAttrs(InstType *Dest, InstType *Src) {
  CopyDebug(Dest, Src);
  Dest->setVolatile(Src->isVolatile());
  if (Src->isAtomic()) {
    errs() << "Use: " << *Src << "\n";
    report_fatal_error("Atomic struct loads/stores not supported");
  }
  // Make a pessimistic assumption about alignment.  Preserving
  // alignment information here is tricky and is not really desirable
  // for PNaCl because mistakes here could lead to non-portable
  // behaviour.
  Dest->setAlignment(1);
}
template <class T>
static void SplitUpStore(StoreInst *Store, T* Ty, bool& NeedsAnotherPass) {
  // Create a separate store instruction for each struct field.
  for (unsigned Index = 0; Index < Ty->getNumElements(); ++Index) {
    SmallVector<Value *, 2> Indexes;
    Indexes.push_back(ConstantInt::get(Store->getContext(), APInt(32, 0)));
    Indexes.push_back(ConstantInt::get(Store->getContext(), APInt(32, Index)));
    Value *GEP = CopyDebug(GetElementPtrInst::Create(
                               Store->getPointerOperand(), Indexes,
                               Store->getPointerOperand()->getName() + ".index",
                               Store), Store);
    NeedsAnotherPass = NeedsAnotherPass || DoAnotherPass(GEP->getType()->getContainedType(0));

    SmallVector<unsigned, 1> EVIndexes;
    EVIndexes.push_back(Index);
    Value *Field = CopyDebug(ExtractValueInst::Create(Store->getValueOperand(),
                                                      EVIndexes, "", Store),
                             Store);
    StoreInst *NewStore = CopyDebug(new StoreInst(Field, GEP, Store), Store);
    ProcessLoadOrStoreAttrs(NewStore, Store);
  }
  Store->eraseFromParent();
}
template <class T>
static void SplitUpLoad(LoadInst *Load, T* Ty, bool& NeedsAnotherPass) {
  Value *NewStruct = UndefValue::get(Ty);

  // Create a separate load instruction for each struct field.
  for (unsigned Index = 0; Index < Ty->getNumElements(); ++Index) {
    SmallVector<Value *, 2> Indexes;
    Indexes.push_back(ConstantInt::get(Load->getContext(), APInt(32, 0)));
    Indexes.push_back(ConstantInt::get(Load->getContext(), APInt(32, Index)));
    Value *GEP = CopyDebug(
        GetElementPtrInst::Create(Load->getPointerOperand(), Indexes,
                                  Load->getName() + ".index", Load), Load);
    LoadInst *NewLoad = new LoadInst(GEP, Load->getName() + ".field", Load);
    NeedsAnotherPass = NeedsAnotherPass || DoAnotherPass(NewLoad);
    ProcessLoadOrStoreAttrs(NewLoad, Load);

    // Reconstruct the struct value.
    SmallVector<unsigned, 1> EVIndexes;
    EVIndexes.push_back(Index);
    NewStruct = CopyDebug(
        InsertValueInst::Create(NewStruct, NewLoad, EVIndexes,
                                Load->getName() + ".insert", Load), Load);
  }
  Load->replaceAllUsesWith(NewStruct);
  Load->eraseFromParent();
}

static void ExpandExtractValue(ExtractValueInst *EV, bool& NeedsAnotherPass) {
  // Search for the insertvalue instruction that inserts the struct
  // field referenced by this extractvalue instruction.
  Value *StructVal = EV->getAggregateOperand();
  Value *ResultField = NULL;
  size_t I = 0;
  for (;;) {
    if (InsertValueInst *IV = dyn_cast<InsertValueInst>(StructVal)) {
      size_t J = 0;
      for(; I < EV->getIndices().size() && J < IV->getIndices().size();
          ++J, ++I) {
        const bool Equal = (EV->getIndices()[I] == IV->getIndices()[J]);
        if(J + 1 == IV->getIndices().size() && Equal) {
          if(I + 1 == EV->getIndices().size()) {
            // Match
            ResultField = IV->getInsertedValueOperand();
          } else {
            StructVal = IV->getInsertedValueOperand();
            ++I;
          }
          break;
        } else if(!Equal) {
          // No match.  Try the next struct value in the chain.
          StructVal = IV->getAggregateOperand();
          break;
        } 
      }
      if(ResultField != nullptr) {
        break;
      } else if(I == EV->getIndices().size()) {
        // We've found an insertvalue that inserts at one or more levels deeper than this extractvalue.
        NeedsAnotherPass = true;
        SmallVector<unsigned, 4> Indices(IV->getIndices().begin() + J, IV->getIndices().end());
        
        InsertValueInst* Insert = InsertValueInst::Create(UndefValue::get(EV->getType()),
                                                          IV->getInsertedValueOperand(),
                                                          Indices,
                                                          "",
                                                          EV);
        ResultField = CopyDebug(Insert, EV);
        break;
      }
    } else if (Constant *C = dyn_cast<Constant>(StructVal)) {
      SmallVector<unsigned, 4> Indices(EV->getIndices().begin() + I, EV->getIndices().end());
      ResultField = ConstantExpr::getExtractValue(C, Indices);
      break;
    } else if(isa<LoadInst>(StructVal)) {
      ResultField = StructVal;
      break;
    } else if(isa<LandingPadInst>(StructVal)) {
      // Nothing we can do here. Just leave it to the PNaCl toolchain.
      return;
    } else {
      errs() << "Value: " << *StructVal << "\n";
      report_fatal_error("Unrecognized struct value");
    }
  }

  assert(ResultField != NULL);
  EV->replaceAllUsesWith(ResultField);
  EV->eraseFromParent();
}
// If an instruction (specifically, InsertValueInst) is used in a resume,
// we can't add it to the ToErase array.
static bool HasResumeUse(Instruction* IV) {
  for(Value::user_iterator I = IV->user_begin(); I != IV->user_end(); ++I) {
    if(isa<ResumeInst>(*I)) {
      return true;
    } else if(isa<InsertValueInst>(*I) && HasResumeUse(cast<Instruction>(*I))) {
      // A chain.
      return true;
    }
  }
  return false;
}

static bool UsesCmpXChg(Instruction* Inst) {
  ExtractValueInst* EV = dyn_cast<ExtractValueInst>(Inst);
  assert(EV != nullptr);
  return isa<AtomicCmpXchgInst>(EV->getAggregateOperand());
}

static bool ExpandExtractValues(Function& Func) {
  bool Changed = false;
  bool NeedsAnotherPass = false;

  // Expand out all the extractvalue instructions.  Also collect up
  // the insertvalue instructions for later deletion so that we do not
  // need to make extra passes across the whole function.
  SmallVector<Instruction *, 10> ToErase;
  for (Function::iterator BB = Func.begin(), E = Func.end();
       BB != E; ++BB) {
    for (BasicBlock::iterator Iter = BB->begin(), E = BB->end();
         Iter != E; ) {
      Instruction *Inst = Iter++;
      if (isa<ExtractValueInst>(Inst) && !UsesCmpXChg(Inst)) {
        ExtractValueInst *EV = dyn_cast<ExtractValueInst>(Inst);
        ExpandExtractValue(EV, NeedsAnotherPass);
        Changed = true;
      } else if (isa<InsertValueInst>(Inst) && !HasResumeUse(Inst)) {
        // Don't remove if this Inst has a connection to a resume.
        ToErase.push_back(Inst);
        Changed = true;
      }
    }
  }

  if(!NeedsAnotherPass) {
    // Delete the insertvalue instructions.  These can reference each
    // other, so we must do dropAllReferences() before doing
    // eraseFromParent(), otherwise we will try to erase instructions
    // that are still referenced.
    for (SmallVectorImpl<Instruction *>::iterator I = ToErase.begin(),
           E = ToErase.end();
         I != E; ++I) {
      (*I)->dropAllReferences();
    }
    for (SmallVectorImpl<Instruction *>::iterator I = ToErase.begin(),
           E = ToErase.end();
         I != E; ++I) {
      (*I)->eraseFromParent();
    }
  }

  return (NeedsAnotherPass && ExpandExtractValues(Func)) || Changed;
}

bool ExpandStructRegs::runOnFunction(Function &Func) {
  bool Changed = false;
  bool NeedsAnotherPass = false;

  // Split up aggregate loads, stores and phi nodes into operations on
  // scalar types.  This inserts extractvalue and insertvalue
  // instructions which we will expand out later.
  for (Function::iterator BB = Func.begin(), E = Func.end();
       BB != E; ++BB) {
    for (BasicBlock::iterator Iter = BB->begin(), E = BB->end();
         Iter != E; ) {
      Instruction *Inst = Iter++;
      if (StoreInst *Store = dyn_cast<StoreInst>(Inst)) {
        if (StructType* Ty = dyn_cast<StructType>(Store->getValueOperand()->getType())) {
          SplitUpStore(Store, Ty, NeedsAnotherPass);
          Changed = true;
	} else if(ArrayType* Ty = dyn_cast<ArrayType>(Store->getValueOperand()->getType())) {
	  SplitUpStore(Store, Ty, NeedsAnotherPass);
          Changed = true;
        }
      } else if (LoadInst *Load = dyn_cast<LoadInst>(Inst)) {
        if (StructType* Ty = dyn_cast<StructType>(Load->getType())) {
          SplitUpLoad(Load, Ty, NeedsAnotherPass);
          Changed = true;
        } else if(ArrayType* Ty = dyn_cast<ArrayType>(Load->getType())) {
	  SplitUpLoad(Load, Ty, NeedsAnotherPass);
	  Changed = true;
	}
      } else if (PHINode *Phi = dyn_cast<PHINode>(Inst)) {
        if (StructType* Ty = dyn_cast<StructType>(Phi->getType())) {
          SplitUpPHINode(Phi, Ty, NeedsAnotherPass);
          Changed = true;
        } else if(ArrayType* Ty = dyn_cast<ArrayType>(Phi->getType())) {
	  SplitUpPHINode(Phi, Ty, NeedsAnotherPass);
	  Changed = true;
	}
      } else if (SelectInst *Select = dyn_cast<SelectInst>(Inst)) {
        if (StructType* Ty = dyn_cast<StructType>(Select->getType())) {
          SplitUpSelect(Select, Ty, NeedsAnotherPass);
          Changed = true;
        } else if (ArrayType* Ty = dyn_cast<ArrayType>(Select->getType())) {
          SplitUpSelect(Select, Ty, NeedsAnotherPass);
          Changed = true;
        }
      }
    }
  }

  if(!NeedsAnotherPass) {
    Changed = ExpandExtractValues(Func) || Changed;
  }

  return (NeedsAnotherPass && runOnFunction(Func)) || Changed;
}

FunctionPass *llvm::createExpandStructRegsPass() {
  return new ExpandStructRegs();
}
