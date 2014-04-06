//===- ReplaceArraysWithInts.cpp - Replace remaining aggregate types with ints//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Replaces remaining BitCastInst|IntToPtrInst -> LoadInst
//   -> (ValOperand) StoreInst combos.
// This pass /should/ be safe because previous passes have reduced element
// access to pointer offsets, so all that remains is the movement of
// whole aggregate values.
//
//===----------------------------------------------------------------------===//

#include "llvm/IR/Attributes.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/Pass.h"
#include "llvm/Transforms/NaCl.h"
#include "llvm/Analysis/NaCl.h"
#include <set>


using namespace llvm;

template <class T> 
const std::string ToStr(const T &V) {
  std::string S;
  raw_string_ostream OS(S);
  OS << const_cast<T &>(V);
  return OS.str();
}

class ReplaceAggregatesWithInts :
  public FunctionPass {
public:
  static char ID;
  ReplaceAggregatesWithInts() : FunctionPass(ID) {
    initializeReplaceAggregatesWithIntsPass(*PassRegistry::getPassRegistry());
  }

  virtual void getAnalysisUsage(AnalysisUsage &Info) const {
    Info.addRequired<DataLayout>();
  }
  virtual bool doInitialization(Module& M) {
    this->M  = &M;
    this->DL = NULL;
    this->C  = &M.getContext();
    this->I8Ty = Type::getIntNTy(*C, 8);
    this->I32Ty = Type::getIntNTy(*C, 32);
    this->MemSet = this->M->getFunction("llvm.memset.p0i8.i32");
    this->Converted = 0;
    return false;
  }
  virtual bool doFinalization(Module&) {
    delete this->DL;
    this->DL = NULL;
    this->C  = NULL;
    this->M  = NULL;
    this->I8Ty = NULL;
    this->I32Ty = NULL;
    this->MemSet = NULL;
    return false;
  }
  size_t Converted;
  const DataLayout* DL;
  LLVMContext* C;
  IntegerType* I8Ty;
  IntegerType* I32Ty;
  Module* M;
  Function* MemSet;

  Type* getReplacedTy(Type* Ty) {
    unsigned Width;
    if(isa<ArrayType>(Ty)) {
      ArrayType* ATy = cast<ArrayType>(Ty);
      Type* ElemTy = ATy->getElementType();
      if(!ElemTy->isIntegerTy()) {
        errs() << "Type: " << ToStr(*ATy) << "\n";
        assert(0 && "Unsupported replacement!");
        report_fatal_error("Unsupported replacement!");
      }

      const unsigned Bits = ElemTy->getIntegerBitWidth();
      Width = Bits * ATy->getNumElements();
    } else if(isa<StructType>(Ty)) {
      Width = DL->getTypeSizeInBits(Ty);
    } else {
      errs() << "Type: " << ToStr(*Ty) << "\n";
      assert(0 && "Invalid type!");
      report_fatal_error("Invalid type!");
    }

    if(Width == 0) {
      errs() << "Type: " << ToStr(*Ty) << "\n";
      assert(0 && "Arrays of zero elements should be already replaced!");
      report_fatal_error("Arrays of zero elements should be already replaced!");
    } else if(Width > 64)
      return NULL;

    return Type::getIntNTy(*C, Width);
  }

  virtual bool runOnFunction(Function& F) {
    if(this->DL == NULL) {
      // apparently this can't go in module initialization
      this->DL = new DataLayout(getAnalysis<DataLayout>());
    }
    std::set<Instruction*> ToErase;

    bool Changed = false;

    const Function::iterator End = F.end();
    for(Function::iterator I = F.begin(); I != End; ++I) {
      BasicBlock* BB = I;
      const BasicBlock::iterator End = BB->end();
      for(BasicBlock::iterator J = BB->begin(); J != End; ++J) {
        Instruction* Inst = J;

        if(isa<IntToPtrInst>(Inst) || isa<BitCastInst>(Inst)) {
          CastInst* Cast = cast<CastInst>(Inst);
          Type* Ty = Cast->getType();
          if(!isa<PointerType>(Ty))
            continue;

          Type* ContainedTy = Ty->getContainedType(0);
          if(!(isa<ArrayType>(ContainedTy) || isa<StructType>(ContainedTy)))
            continue;

          Type* NewTy = getReplacedTy(ContainedTy);
          if(NewTy == NULL) {
	    Cast->mutateType(this->I8Ty->getPointerTo());

            const Value::use_iterator End = Cast->use_end();
            for(Value::use_iterator K = Cast->use_begin(); K != End;) {
              if(!isa<StoreInst>(*K)) {
                errs() << "Inst: " << ToStr(*Cast) << "\n";
                errs() << "Use : " << ToStr(**K) << "\n";
                assert(0 && "Unknown use!");
                report_fatal_error("Unknown use!");
              }

              Instruction* KInst = cast<Instruction>(*K++);
              /*if(isa<LoadInst>(KInst)) {
                const Value::use_iterator End = KInst->use_end();
                for(Value::use_iterator L = KInst->use_begin(); L != End; ++L) {
                  if(!isa<StoreInst>(*L)) {
                    errs() << "Inst: " << ToStr(*KInst) << "\n";
                    errs() << "Use: " << ToStr(**L) << "\n";
                    assert(0 && "Non-StoreInst use!");
                    report_fatal_error("Non-StoreInst use!");
                  }

                  ToErase.insert(cast<Instruction>(*L));
                }
		} else {*/
	      StoreInst* Store = cast<StoreInst>(KInst);

	      Value* ValOp = Store->getValueOperand();
	      assert(isa<ArrayType>(ValOp->getType()));
	      ArrayType* ValTy = cast<ArrayType>(ValOp->getType());
	      assert(isa<ConstantAggregateZero>(ValOp));

	      std::vector<Value*> Args;
	      Args.reserve(5);
	      Args.push_back(Cast);
	      Args.push_back(ConstantInt::get(I8Ty, 0, false));
	      Args.push_back(ConstantInt::get(I32Ty,
					      DL->getTypeSizeInBits(ValTy) / 8,
					      false));
	      Args.push_back(ConstantInt::get(I32Ty, 0, false));
	      Args.push_back(ConstantInt::getFalse(Type::getInt1Ty(*this->C)));
	      CallInst* Call = CallInst::Create(this->MemSet,
						Args,
						"",
						KInst);
	      CopyDebug(Call, KInst);
		//}
              ToErase.insert(KInst);
	      Changed = true;
            }
          } else {
            // mutate load types.
            const Value::use_iterator End = Cast->use_end();
            for(Value::use_iterator K = Cast->use_begin(); K != End; ++K) {
              if(isa<LoadInst>(*K)) {
                assert(K->getType() == ContainedTy);
                K->mutateType(NewTy);
                Changed = true;
              }
            }
            Cast->mutateType(PointerType::get(NewTy, 0));
          }

        } // BinOp
      } // BasicBlock::iterator
    } // Function::iterator

    const std::set<Instruction*>::iterator End2 = ToErase.end();
    for(std::set<Instruction*>::iterator I = ToErase.begin(); I != End2; ++I) {
      (*I)->dropAllReferences();
    }
    for(std::set<Instruction*>::iterator I = ToErase.begin(); I != End2; ++I) {
      (*I)->eraseFromParent();
    }

    this->Converted++;
    return Changed;
  }
};

char ReplaceAggregatesWithInts::ID = 0;
INITIALIZE_PASS(ReplaceAggregatesWithInts,
                "replace-aggregates-with-ints",
                "Replace remaining aggregates with a single integer", 
                false,
                false)

FunctionPass *llvm::createReplaceAggregatesWithIntsPass() {
  return new ReplaceAggregatesWithInts();
}
