//===- ReplaceVectorsWithArrays.cpp - Replace SIMD vectors with equivalent arrays//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
// ===---------------------------------------------------------------------===//
// ===---------------------------------------------------------------------===//

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
#include <map>

using namespace llvm;

class ReplaceVectorsWithArrays :
  public ModulePass {
public:
  static char ID;
  ReplaceVectorsWithArrays() 
    : ModulePass(ID) {
    initializeReplaceVectorsWithArraysPass(*PassRegistry::getPassRegistry());
  }
  typedef std::map<Type*, Type*>::iterator ty_iterator;
  std::map<Type*, Type*> m_types;

  typedef std::set<GlobalValue*>::iterator gv_iterator;
  std::set<GlobalValue*> m_globals;

  typedef std::map<Constant*, Constant*>::iterator const_iterator;
  std::map<Constant*, Constant*> m_consts;

  void promoteFunction(Function* F);
  void promoteGlobalVariable(GlobalVariable* G);
  
  Type* promoteType(Type* Ty);
  Constant* promoteConstant(Constant* C);
  void promoteOperands(User* U);
  Value* promoteOperand(Value* V);

  void promoteGlobal(GlobalValue* V);

  bool shouldPromote(Type* Ty);

  bool runOnModule(Module& M);
};

char ReplaceVectorsWithArrays::ID = 0;
INITIALIZE_PASS(ReplaceVectorsWithArrays, "replace-vectors-with-arrays",
                "Replace vector types with equivalent arrays",
                false, false)

size_t ActualPromotedFunctions3;

ModulePass* llvm::createReplaceVectorsWithArraysPass() {
  return new ReplaceVectorsWithArrays();
}

bool ReplaceVectorsWithArrays::shouldPromote(Type* Ty) {
  return isa<VectorType>(Ty);
}

Type* ReplaceVectorsWithArrays::promoteType(Type* Ty) {
  if(Ty == NULL)
    return NULL;

  ty_iterator i = m_types.find(Ty);
  if(i != m_types.end()) {
    assert(i->second != NULL && "promoteType");
    return i->second;
  }
  Type* NewTy = NULL;

  if(isa<PointerType>(Ty)) {
    Type* InnerTy = Ty->getContainedType(0);
    Type* NewInnerTy = promoteType(InnerTy);
    NewTy = PointerType::get(NewInnerTy, 0);
  } else if(isa<StructType>(Ty) && Ty->getStructNumElements() != 0) {
    StructType* STy = cast<StructType>(Ty);
    
    StructType* NewSTy;
    if(STy->hasName())
      NewSTy = StructType::create(Ty->getContext(), STy->getName());
    else
      NewSTy = StructType::create(Ty->getContext());

    NewTy = NewSTy;
    m_types[Ty] = NewTy;
    m_types[NewTy] = NewTy;

    std::vector<Type*> Types;
    Types.reserve(STy->getNumElements());

    for(unsigned j = 0; j < STy->getNumElements(); ++j) {
      Type* OldTy2 = STy->getElementType(j);
      Type* NewTy2 = promoteType(OldTy2);
      Types.push_back(NewTy2);
    }
    NewSTy->setBody(Types, STy->isPacked());
    return NewTy;
  } else if(isa<FunctionType>(Ty)) {
    FunctionType* FTy = cast<FunctionType>(Ty);
    Type* RetTy = FTy->getReturnType();
    Type* NewRetTy = promoteType(RetTy);

    std::vector<Type*> Args;
    Args.reserve(FTy->getNumParams());

    for(unsigned j = 0; j < FTy->getNumParams(); ++j) {
      Type* OldTy2 = FTy->getParamType(j);
      Type* NewTy2 = promoteType(OldTy2);
      Args.push_back(NewTy2);
    }

    NewTy = FunctionType::get(NewRetTy, Args, FTy->isVarArg());
  } else if(isa<ArrayType>(Ty)) {
    ArrayType* ATy = cast<ArrayType>(Ty);
    Type* ElementTy = ATy->getElementType();
    Type* NewElemTy = promoteType(ElementTy);
    NewTy = ArrayType::get(NewElemTy, ATy->getNumElements());
  } else if(isa<VectorType>(Ty)) {
    VectorType* VTy = cast<VectorType>(Ty);
    Type* ElementTy = VTy->getElementType();
    NewTy = ArrayType::get(ElementTy, VTy->getNumElements());
  } else {
    NewTy = Ty;
  }
  
  assert(NewTy != NULL);
  m_types.insert(std::make_pair(Ty, NewTy));
  if(Ty != NewTy)
    m_types.insert(std::make_pair(NewTy, NewTy));
  return NewTy;
}
void ReplaceVectorsWithArrays::promoteFunction(Function* F) {
  if(F->isIntrinsic())
    return;

  Type* OldTy = F->Value::getType();
  Type* NewTy = promoteType(OldTy);
  F->mutateType(NewTy);
  {
    const Function::arg_iterator end = F->arg_end();
    for(Function::arg_iterator i = F->arg_begin(); i != end; ++i) {
      Type* OldTy = i->getType();
      Type* NewTy = promoteType(OldTy);
      i->mutateType(NewTy);
    }
  }

  const Function::iterator end = F->end();
  for(Function::iterator i = F->begin(); i != end; ++i) {
    BasicBlock::iterator end = i->end();
    for(BasicBlock::iterator j = i->begin(); j != end;) {
      Type* Ty = j->getType();
      if(!shouldPromote(Ty) &&
	 !(isa<PointerType>(Ty) &&
	   shouldPromote(Ty->getContainedType(0))) &&
	 !isa<ExtractElementInst>(j)) {
	promoteOperands(j);
	promoteOperand(j++);
	continue;
      }

      if(BinaryOperator* BinOp = dyn_cast<BinaryOperator>(j)) {
	Instruction::BinaryOps Op = BinOp->getOpcode();
	ArrayType* Ty = cast<ArrayType>(promoteType(BinOp->getType()));

	Value* Left = promoteOperand(j->getOperand(0));
	Value* Right = promoteOperand(j->getOperand(1));

	Value* V = UndefValue::get(Ty);
	for(size_t k = 0; k < Ty->getNumElements(); ++k) {
	  Value* LV =
	    CopyDebug(ExtractValueInst::Create(Left,
					       std::vector<unsigned>(1, k),
					       "",
					       BinOp),
		      BinOp);
	  Value* RV =
	    CopyDebug(ExtractValueInst::Create(Right,
					       std::vector<unsigned>(1, k),
					       "",
					       BinOp),
		      BinOp);
	  BinaryOperator* BinOp2 =
	    CopyDebug(BinaryOperator::Create(Op, LV, RV, "", BinOp),
		      BinOp);

	  V = CopyDebug(InsertValueInst::Create(V,
						BinOp2,
						std::vector<unsigned>(1, k),
						"",
						BinOp),
			BinOp);
	}
	++j;
	BinOp->mutateType(V->getType());
	BinOp->replaceAllUsesWith(V);
	BinOp->eraseFromParent();
	continue;
      } else if(CmpInst* Cmp = dyn_cast<CmpInst>(j)) {
	VectorType* OldTy = cast<VectorType>(Cmp->getType());
	ArrayType*  NewTy = cast<ArrayType>(promoteType(OldTy));

	Value* Left = promoteOperand(j->getOperand(0));
	Value* Right = promoteOperand(j->getOperand(1));

	Value* V = UndefValue::get(NewTy);
	for(size_t k = 0; k < NewTy->getNumElements(); ++k) {
	  Value* LV = CopyDebug(ExtractValueInst::Create(Left,
							 std::vector<unsigned>(1, k),
							 "",
							 Cmp),
				Cmp);
	  Value* RV = CopyDebug(ExtractValueInst::Create(Right,
							 std::vector<unsigned>(1, k),
							 "",
							 Cmp),
				Cmp);

	  Value* Result = CopyDebug(CmpInst::Create(Cmp->getOpcode(),
						    Cmp->getPredicate(),
						    LV,
						    RV,
						    "",
						    Cmp),
				    Cmp);
	  V = CopyDebug(InsertValueInst::Create(V,
						Result,
						std::vector<unsigned>(1, k),
						"",
						Cmp),
			Cmp);
	}
	++j;
	Cmp->mutateType(V->getType());
	Cmp->replaceAllUsesWith(V);
	Cmp->eraseFromParent();
	continue;
      } else if(InsertElementInst* Insert = dyn_cast<InsertElementInst>(j)) {
	VectorType* OldTy = Insert->getType();
	ArrayType*  NewTy = cast<ArrayType>(promoteType(OldTy));

	Value* Op0 = promoteOperand(Insert->getOperand(0));
	Value* Op1 = promoteOperand(Insert->getOperand(1));

	Value* Idx0 = ConstantInt::get(Type::getInt32Ty(j->getContext()), 0);
	std::vector<Value*> Idxs;
	Idxs.push_back(Idx0);
	Idxs.push_back(promoteOperand(Insert->getOperand(2)));
	
	AllocaInst* Storage =
	  CopyDebug(new AllocaInst(NewTy, NULL, "", F->getEntryBlock().getFirstNonPHI()),
		    Insert);
	StoreInst* Store = CopyDebug(new StoreInst(Op0, Storage, "", Insert),
		  Insert);
	Store->setAlignment(0);
	Store->setOrdering(NotAtomic);
	Store->setVolatile(false);
	GetElementPtrInst* GEPi =
	  CopyDebug(GetElementPtrInst::Create(Storage, Idxs, "", Insert),
		    Insert);
	Store = CopyDebug(new StoreInst(Op1, GEPi, "", Insert),
		  Insert);
	Store->setAlignment(0);
	Store->setOrdering(NotAtomic);
	Store->setVolatile(false);
	LoadInst* Load =
	  CopyDebug(new LoadInst(Storage, "", Insert),
		    Insert);
	++j;
	Insert->mutateType(Load->getType());
	Insert->replaceAllUsesWith(Load);
	Insert->eraseFromParent();
	continue;
      } else if(ExtractElementInst* Extract = dyn_cast<ExtractElementInst>(j)) {
	Value* Idx0 = ConstantInt::get(Type::getInt32Ty(j->getContext()), 0);
	std::vector<Value*> Idxs;
	Idxs.push_back(Idx0);
	Idxs.push_back(promoteOperand(Extract->getIndexOperand()));
	Value* Operand = promoteOperand(Extract->getVectorOperand());
	Type* NewVecType = Operand->getType();
	AllocaInst* Storage =
	  CopyDebug(new AllocaInst(NewVecType,
				   NULL,
				   "",
				   F->getEntryBlock().getFirstNonPHI()),
		    Extract);
	StoreInst* Store = CopyDebug(new StoreInst(Operand,
						   Storage,
						   "",
						   Extract),
				     Extract);
	Store->setAlignment(0);
	Store->setOrdering(NotAtomic);
	Store->setVolatile(false);
	GetElementPtrInst* GEPi =
	  CopyDebug(GetElementPtrInst::Create(Storage, Idxs, "", Extract),
		    Extract);
	LoadInst* Load =
	  CopyDebug(new LoadInst(GEPi, "", Extract),
		    Extract);
	++j;
	Extract->replaceAllUsesWith(Load);
	Extract->eraseFromParent();
	continue;
      } else if(ShuffleVectorInst* Shuffle = dyn_cast<ShuffleVectorInst>(j)) {
	VectorType* OldTy = Shuffle->getType();
	ArrayType* NewTy = cast<ArrayType>(promoteType(OldTy));

	Value* Left = promoteOperand(Shuffle->getOperand(0));
	Value* Right = promoteOperand(Shuffle->getOperand(1));
	const unsigned Size = cast<ArrayType>(Left->getType())->getNumElements();
	
	SmallVector<int, 16> Mask = Shuffle->getShuffleMask();
	Value* V = UndefValue::get(NewTy);
	for(size_t k = 0; k < Mask.size(); ++k) {
	  unsigned Idx;
	  Value* Side = NULL;
	  if((unsigned)Mask[k] < Size) {
	    Side = Left;
	    Idx = Mask[k];
	  } else {
	    Side = Right;
	    Idx = Mask[k] - Size;
	  }

	  Value* Value = CopyDebug(ExtractValueInst::Create(Side,
							    std::vector<unsigned>(1, Idx),
							    "",
							    Shuffle),
				   Shuffle);
	  V = CopyDebug(InsertValueInst::Create(V,
						Value,
						std::vector<unsigned>(1, k),
						"",
						Shuffle),
			Shuffle);
	}
	++j;
	Shuffle->mutateType(V->getType());
	Shuffle->replaceAllUsesWith(V);
	Shuffle->eraseFromParent();
	continue;
      } else if(isa<CastInst>(j) && !isa<PointerType>(j->getType())) {
	CastInst* Cast = cast<CastInst>(j);
	Instruction::CastOps Opcode = Cast->getOpcode();
	Value* Operand = promoteOperand(Cast->getOperand(0));
	VectorType* OldTy = cast<VectorType>(Cast->getType());
	ArrayType* NewTy = cast<ArrayType>(promoteType(OldTy));

	Value* V = UndefValue::get(NewTy);
	for(size_t k = 0; k < OldTy->getNumElements(); ++k) {
	  Value* V2 =
	    CopyDebug(ExtractValueInst::Create(Operand,
					       std::vector<unsigned>(1, k),
					       "",
					       Cast),
		      Cast);

	  Value* Result =
	    CopyDebug(CastInst::Create(Opcode,
				       V2,
				       NewTy->getElementType(),
				       "",
				       Cast),
		      Cast);
	  V = CopyDebug(InsertValueInst::Create(V,
						Result,
						std::vector<unsigned>(1, k),
						"",
						Cast),
			Cast);
	}
	++j;
	Cast->mutateType(V->getType());
	Cast->replaceAllUsesWith(V);
	Cast->eraseFromParent();
	continue;
      } else {
	promoteOperands(j);
	promoteOperand(j++);
      }
    }
  }
}
void ReplaceVectorsWithArrays::promoteGlobalVariable(GlobalVariable* G) {
  Type* OriginalTy = G->getType();
  Type* PromotedTy = promoteType(OriginalTy);
  G->mutateType(PromotedTy);
  if(G->hasInitializer()) {
    Constant* OldC = G->getInitializer();
    Constant* NewC = promoteConstant(OldC);
    G->setInitializer(NewC);
  }
}
Constant* ReplaceVectorsWithArrays::promoteConstant(Constant* C) {
  if(isa<GlobalValue>(C)) {
    GlobalValue* V = cast<GlobalValue>(C);
    promoteGlobal(V);
    return C;
  }

  std::pair<const_iterator, bool> i
    = m_consts.insert(std::make_pair(C, (Constant*)NULL));
  // If i.first->second is NULL, we've encountered a recursion.
  // See the comment in the first branch.
  if(!i.second && i.first->second != NULL) {
    return i.first->second;
  }

  Constant*& NewC = i.first->second;

  Type* OldTy = C->getType();
  Type* NewTy = promoteType(OldTy);
  if(isa<ConstantExpr>(C) ||
     isa<ConstantStruct>(C) ||
     isa<ConstantArray>(C)) {
    std::vector<Constant*> Consts;
    Consts.reserve(C->getNumOperands());
    const User::value_op_iterator end = C->value_op_end();
    for(User::value_op_iterator i = C->value_op_begin(); i != end; ++i) {
      Constant* OldC2 = cast<Constant>(*i);
      Constant* NewC2 = promoteConstant(OldC2);

      // the promotion of one of the operands caused us to circle back around to this const.
      // the only way this can happen is through a global, which means the second time around
      // would skip the global causing the recursion, allowing the promotion to finish.
      // if all that happens, our reference into the map will reflect the promotion,
      // NewC won't be NULL, and we can just return.
      if(NewC != NULL)
        return NewC;

      Consts.push_back(NewC2);
    }

    if(ConstantExpr* CE = dyn_cast<ConstantExpr>(C)) {
      NewC = CE->getWithOperands(Consts, NewTy);
    } else if(isa<ConstantStruct>(C)) {
      StructType* STy = cast<StructType>(NewTy);
      NewC = ConstantStruct::get(STy, Consts);
    } else if(isa<ConstantArray>(C)) {
      ArrayType* ATy = cast<ArrayType>(NewTy);
      NewC = ConstantArray::get(ATy, Consts);
    }
  } else if(isa<UndefValue>(C)) {
    NewC = UndefValue::get(NewTy);
  } else if(isa<ConstantPointerNull>(C)) {
    NewC = ConstantPointerNull::get(cast<PointerType>(NewTy));
  } else if(isa<ConstantAggregateZero>(C)) {
    NewC = ConstantAggregateZero::get(NewTy);
  } else if(isa<ConstantVector>(C) || isa<ConstantDataVector>(C)) {
    std::vector<Constant*> ArrayVals;
    if(C->getSplatValue()) {
      ArrayVals = std::vector<Constant*>(cast<VectorType>(C->getType())->getNumElements(),
					 C->getSplatValue());
    } else if(isa<ConstantVector>(C)) {
      ArrayVals.reserve(C->getNumOperands());
      for(User::op_iterator i = C->op_begin(); i != C->op_end(); ++i) {
	Constant* Op = cast<Constant>(*i);
	ArrayVals.push_back(Op);
      }
    } else if(ConstantDataVector* CDV = dyn_cast<ConstantDataVector>(C)) {
      const unsigned End = CDV->getNumElements();
      ArrayVals.reserve(End);
      for(unsigned i = 0; i < End; ++i) {
	Constant* Op = CDV->getElementAsConstant(i);
	ArrayVals.push_back(Op);
      }
    }
    NewC = ConstantArray::get(cast<ArrayType>(NewTy), ArrayVals);
  } else {
    NewC = C;
  }

  assert(NewC != NULL);
  if(C != NewC) {
    m_consts.insert(std::make_pair(NewC, NewC));
  }
  return NewC;
}
void ReplaceVectorsWithArrays::promoteOperands(User* U) {
  unsigned pos = 0;
  const User::value_op_iterator end = U->value_op_end();
  for(User::value_op_iterator k = U->value_op_begin(); k != end; ++k, ++pos) {
    Value* NewV = promoteOperand(*k);
    U->setOperand(pos, NewV);
  }
}
Value* ReplaceVectorsWithArrays::promoteOperand(Value* V) {
  if(isa<Constant>(V)) {
    Constant* C = cast<Constant>(V);
    Constant* NewC = promoteConstant(C);
    return NewC;
  } else {
    Type* Ty = V->getType();
    Type* NewTy = promoteType(Ty);
    V->mutateType(NewTy);
    return V;
  }
}
void ReplaceVectorsWithArrays::promoteGlobal(GlobalValue* V) {
  if(m_globals.insert(V).second) {
    if(isa<Function>(V)) {
      Function* F = cast<Function>(V);
      promoteFunction(F);
      ++ActualPromotedFunctions3;
    } else if(isa<GlobalVariable>(V)) {
      GlobalVariable* G = cast<GlobalVariable>(V);
      promoteGlobalVariable(G);
    }
  }
}

bool ReplaceVectorsWithArrays::runOnModule(Module& M) {
  size_t Promoted = 0;
  {
    const Module::iterator end = M.end();
    for(Module::iterator i = M.begin(); i != end; ++i) {
      promoteGlobal(i);
      ++Promoted;
    }
  }

  Promoted = 0;
  {
    const Module::global_iterator end = M.global_end();
    for(Module::global_iterator i = M.global_begin(); i != end; ++i) {
      promoteGlobal(i);
      ++Promoted;
    }
  }

  // Ensure alias types are up-to-date:
  for (Module::alias_iterator I = M.alias_begin(), E = M.alias_end();
       I != E; ++I) {
    auto* Aliasee = promoteConstant(I->getAliasee());
    I->mutateType(Aliasee->getType());
    I->setAliasee(Aliasee);
  }

  // remove dangling consts:
  {
    const const_iterator end = m_consts.end();
    for(const_iterator i = m_consts.begin(); i != end; ++i) {
      (*i).second->removeDeadConstantUsers();
    }
  }
  {
    const gv_iterator end = m_globals.end();
    for(gv_iterator i = m_globals.begin(); i != end; ++i) {
      (*i)->removeDeadConstantUsers();
    }
  }

  m_globals.clear();
  m_types.clear();
  m_consts.clear();
  return true;
}
