//===- PromoteSimpleStructs.cpp - Expand out structs with a single element-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
//===----------------------------------------------------------------------===//


#include "llvm/Pass.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/Transforms/NaCl.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/IR/Instructions.h"
#include "llvm/Transforms/Utils/ValueMapper.h"
#include <iostream>
#include <set>

using namespace llvm;

template <class T> 
const std::string ToStr(const T &V) {
  std::string S;
  raw_string_ostream OS(S);
  OS << const_cast<T &>(V);
  return OS.str();
}

struct PromotedType {
public:
  Type* Original;
  Type* Promoted;
  bool InProgress;
  bool Protected;
  std::vector<size_t> Bumps;

  void bump(const size_t Idx) {
    Bumps.push_back(Idx);
  }
  size_t bumpOffset(const size_t Idx) const {
    if(!Original->isStructTy() || Bumps.size() == 0)
      return Idx;

    size_t B = 0;
    for(; B < Bumps.size(); ++B) {
      // we can't bump an idx to
      // an element that was removed:
      assert(Idx != Bumps[B]);
      if(Idx < Bumps[B])
	break;
    }
    return Idx - B;
  }
  Value* bumpOffset(Value* Idx) const {
    if(!Original->isStructTy() || Bumps.size() == 0)
      return Idx;

    ConstantInt* I = cast<ConstantInt>(Idx);
    IntegerType* IntTy = I->getType();
    size_t B = 0;
    for(; B < Bumps.size(); ++B) {
      // we can't bump an idx to
      // an element that was removed:
      assert(I->getValue() != Bumps[B]);
      if(!I->uge(Bumps[B])) {
	if(B == 0) {
	  return Idx;
	} else {
	  break;
	}
      }
    }
    return ConstantInt::get(IntTy, I->getValue() - B);
  }
  bool isBumped(const size_t Idx) const {
    return std::binary_search(Bumps.begin(), Bumps.end(), Idx);
  }

  PromotedType promote(Type* Replacing) const {
    assert(!InProgress && Promoted != NULL);
    PromotedType PT = *this;
    PT.Original = Replacing;
    return PT;
  }

  static PromotedType startWithPlaceholder(Type* Original, Type* Promoted, const bool Protected) {
    PromotedType PT;
    PT.Original = Original;
    PT.Promoted = Promoted;
    PT.InProgress = true;
    PT.Protected = Protected;
    return PT;
  }
  static PromotedType start(Type* Original, const bool Protected) {
    PromotedType PT;
    PT.Original = Original;
    PT.Promoted = NULL;
    PT.InProgress = true;
    PT.Protected = Protected;
    return PT;
  }

  PromotedType finishPlaceholder() {
    assert(InProgress && Promoted != NULL);
    InProgress = false;
    Protected = false;
    return *this;
  }

  PromotedType finish(Type* PTy) {
    assert(InProgress && Promoted == NULL);
    Promoted = PTy;
    InProgress = false;
    Protected = false;
    return *this;
  }

  bool isFinished() const {
    return !InProgress;
  }

  static PromotedType noOp(Type* Ty, const bool Protected) {
    PromotedType PT;
    PT.Original = Ty;
    PT.Promoted = Ty;
    PT.InProgress = false;
    PT.Protected = Protected;
    return PT;
  }
  static PromotedType protectedPlaceholder(ArrayType* OriginalTy, ArrayType* PromotedTy) {
    PromotedType PT;
    PT.Original = OriginalTy;
    PT.Promoted = PromotedTy;
    PT.InProgress = false;
    PT.Protected = true;
    return PT;
  }
};

struct PromoteSimpleStructs : public ModulePass {
  static char ID;
  PromoteSimpleStructs() : ModulePass(ID) {
    initializePromoteSimpleStructsPass(*PassRegistry::getPassRegistry());
  }
    
  struct ConversionState {
    typedef std::set<Instruction*>::iterator iterator;
    std::set<Instruction*> m_replacements;
    PromoteSimpleStructs* m_p;

    Function* m_f;

    void convertOperands(User* From);

    void recordConverted(Instruction* I);
    void eraseConverted(Instruction* I);
    size_t convertedSize();
      
    Value* get(Value* From, Type** OldTy = NULL);

    void convertBlock(BasicBlock* Bb);
    Value* convertInstruction(Instruction* Inst);
    Value* convertGEPInstruction(GetElementPtrInst* Inst,
                                 Type* OriginalTy,
                                 Type* PromotedTy,
                                 Value* PointerOp,
                                 Type* PointerOpOriginalTy);
    Value* convertEVOrIVInstruction(Instruction* Inst,
                                    Type* OriginalTy,
                                    Type* PromotedTy,
                                    Value* AggOp,
                                    Type* AggOpOriginalTy);
    template <class T>
    T* convertCall(T* Call,
		   Type* OriginalTy,
		   Type* PromotedTy);
    void possiblyConvertUsers(Instruction* Inst, Value* Replacement, Type* OriginalTy);

    ConversionState() {}
    ConversionState(PromoteSimpleStructs* P, Function* F) 
      : m_p(P), m_f(F) {
      const Function::arg_iterator end = F->arg_end();
      for(Function::arg_iterator i = F->arg_begin(); i != end; ++i) {
        Type* OriginalTy = i->getType();
        Type* PromotedTy = m_p->getPromotedType(OriginalTy);
        m_p->mutateAndReplace(i, i, OriginalTy, PromotedTy);
      }
    }
    ~ConversionState() {
      const PromoteSimpleStructs::origin_ty_iterator end = m_p->m_original_types.end();
      for(PromoteSimpleStructs::origin_ty_iterator i = m_p->m_original_types.begin();
          i != end;) {
        if(isa<Instruction>(i->first) || isa<Argument>(i->first))
          m_p->m_original_types.erase(i++);
        else
          ++i;
      }
    }
  };
  
  Module* m_module;
  typedef std::set<GlobalValue*>::iterator iterator;
  typedef std::map<Type*, PromotedType>::iterator ty_iterator;
  typedef std::map<Value*, Type*>::iterator origin_ty_iterator;
  typedef std::map<Constant*, Constant*>::iterator const_iterator;

  std::set<GlobalValue*> m_promoted;
  std::map<Type*, PromotedType> m_promoted_types;
  std::map<Constant*, Constant*> m_promoted_consts;
  std::map<Value*, Type*> m_original_types;

#ifndef NDEBUG
  void debug_print_all_original_types();
#endif

  GlobalValue* getPromoted(GlobalValue* F);
  Type* getPromotedType(Type* T, const PromotedType** Raw, const bool Protected = false);
  Type* getPromotedType(Type* T) {
    return getPromotedType(T, (const PromotedType**) NULL);
  }
  Type* getPromotedType(Type* T, PromotedType*  Raw) {
    if(Raw != NULL) {
      const PromotedType* Buf = NULL;
      T = getPromotedType(T, &Buf);
      *Raw = *Buf;

    } else {
      T = getPromotedType(T, (const PromotedType**) NULL);
    }
    return T;
  }
  PromotedType getPromotedTypeData(Type* T) {
    PromotedType PT;
    getPromotedType(T, &PT);
    return PT;
  }

  PromotedType getPromotedTypeImpl(Type* T, const bool Protected);
  static bool shouldPromote(Type* T) {
    std::set<Type*> chain;
    return shouldPromote(T, chain);
  }
  static bool shouldPromote(Type* T, std::set<Type*>& Chain);
  static bool shouldPromote(Function* F);
  inline static bool isShallowPromotable(Type* T) {
    return (isa<StructType>(T) && getInnerMeaningfulType(cast<StructType>(T)) != NULL) ||
      (isa<ArrayType>(T)       && cast<ArrayType>(T)->getNumElements() == 1) ||
      (isa<PointerType>(T)     && isShallowPromotable(T->getContainedType(0)));
  }
  inline static bool isMeaningful(Type* T) {
    return !(isa<StructType>(T) && (cast<StructType>(T)->getNumElements() == 0 ||
				    (cast<StructType>(T)->getNumElements() == 1 &&
				     !isMeaningful(T->getContainedType(0))))) &&
      !(isa<ArrayType>(T) && cast<ArrayType>(T)->getNumElements() == 0);
  }
  // returns non-null if this structure has a single meaningful member.
  inline static Type* getInnerMeaningfulType(StructType* ST) {
    bool FoundSoleInner = false;
    Type* OnlyMeaningfulTy = NULL;

    if(ST->getNumElements() == 1)
      OnlyMeaningfulTy = ST->getElementType(0);

    const StructType::element_iterator end = ST->element_end();
    for(StructType::element_iterator i = ST->element_begin();
	i != end && !(FoundSoleInner xor (OnlyMeaningfulTy != NULL));
	++i) {
      const bool Meaningful = isMeaningful(*i);
      if(Meaningful && !FoundSoleInner){
	FoundSoleInner = true;
	OnlyMeaningfulTy = *i;
      } else if(Meaningful && FoundSoleInner) {
	OnlyMeaningfulTy = NULL;
      }
    }
    return OnlyMeaningfulTy;
  }
  inline Constant* getPromotedConstant(Use* U) {
    return getPromotedConstant(cast<Constant>(U));
  }
  Constant* getPromotedConstant(Constant* C);

  Type* getOriginalType(Value* V);
  void recordOriginalType(Value* NewV, Type* OldTy);
  void eraseOriginalType(Value* V) {
    origin_ty_iterator i = m_original_types.find(V);
    assert(i != m_original_types.end());
    m_original_types.erase(i);
  }
  void mutateAndReplace(Value* OldV, Value* NewV, Type* OldT, Type* NewT);

  void promoteGlobal(GlobalVariable* G);
  bool isGlobalVariablePromotable(GlobalVariable* G) {
    return !G->hasAppendingLinkage();
  }
  // Used to prevent us from promoting types that are used in appending linkages.
  // After we've started promoting functions/vars, calling this is no longer allowed.
  // Note that we don't actually check for ^
  ArrayType* protectType(Type* Ty) {
    ArrayType* ATy = dyn_cast<ArrayType>(Ty);
    assert(ATy && "May only protect array types");
    ArrayType* NewATy;

    const unsigned NumElements = ATy->getNumElements();
    Type* ElemTy = ATy->getElementType();
    NewATy = ArrayType::get(getPromotedType(ElemTy, NULL, true), NumElements);
    
    const std::pair<ty_iterator, bool> r =
      m_promoted_types.insert(std::make_pair(ATy,
					     PromotedType::protectedPlaceholder(ATy, NewATy)));

    if(!r.second && r.first->second.Promoted != Ty) {
      errs() << "Original type: " << ToStr(*Ty) << "\n";
      errs() << "Promoted type: " << ToStr(*r.first->second.Promoted) << "\n";
      assert(0 && "Did you call protectType after starting the main pass?");
      llvm_unreachable("Type already promoted");
    }
    return NewATy;
  }
  Function* promoteFunction(Function& F, const bool PrototypeOnly = false);

  bool isAggregateType(Type* T);

  bool runOnModule(Module& M);
};
char PromoteSimpleStructs::ID = 0;
INITIALIZE_PASS(PromoteSimpleStructs,
                "promote-simple-structs",
                "Promote out structs with a single element", 
                false, 
                false)

#ifndef NDEBUG
// for use in gdb
void debug_printf(Value* V) {
  std::cout << ToStr(*V) << std::endl;
}
void debug_printf(Type* T) {
  std::cout << ToStr(*T) << std::endl;
}

void debug_collect_all_subtypes(Type* T, std::set<Type*>& Collection);
void debug_printf_all_subtypes(Type* T) {
  std::set<Type*> C;
  debug_collect_all_subtypes(T, C);
  for(; isa<PointerType>(T); T = T->getContainedType(0)) {
  }
  std::cout << "debug_printf_all_subtypes for: ";
  debug_printf(T);
  const std::set<Type*>::iterator end = C.end();
  for(std::set<Type*>::iterator i = C.begin(); i != end; ++i) {
    if(*i != T)
      debug_printf(*i);
  }
}
void debug_collect_all_subtypes(Type* T, std::set<Type*>& Collection) {
  if(Collection.count(T) > 0)
    return;
  else if(isa<StructType>(T)) {
    Collection.insert(T);
  }

  for(size_t i = 0; i < T->getNumContainedTypes(); ++i) {
    debug_collect_all_subtypes(T->getContainedType(i), Collection);
  }
}

void PromoteSimpleStructs::debug_print_all_original_types() {
  const origin_ty_iterator end = m_original_types.end();
  for(origin_ty_iterator i = m_original_types.begin(); i != end; ++i) {
    std::cout << "Value        : " << ToStr(*i->first) << std::endl;
    std::cout << "Original type: " << ToStr(*i->second) << std::endl;
  }
}
#endif
size_t PromoteSimpleStructs::ConversionState::convertedSize() {
  return m_replacements.size();
}
void PromoteSimpleStructs::ConversionState::eraseConverted(Instruction* I) {
  iterator i = m_replacements.find(I);
#ifndef NDEBUG
  if(i == m_replacements.end()) {
      errs() << "Value: " << ToStr(*I) << "\n";
      assert(i != m_replacements.end() && "Value not converted!");
      llvm_unreachable("Value not converted!");
  }
#endif
  m_replacements.erase(i);
}
void PromoteSimpleStructs::recordOriginalType(Value* NewV, Type* OldTy) {
  std::pair<origin_ty_iterator, bool> R = m_original_types.insert(std::make_pair(NewV, OldTy));
  if(!R.second && R.first->second != OldTy) {
    errs() << "New value    : " << ToStr(*NewV) << "\n";
    errs() << "Original type: " << ToStr(*R.first->second) << "\n";
    errs() << "Old type     : " << ToStr(*OldTy) << "\n";
    assert(0 && "Value already promoted!");
    llvm_unreachable("Value already promoted!");
  }
}
void PromoteSimpleStructs::mutateAndReplace(Value* OldV, Value* NewV, Type* OldT, Type* NewT) {
  recordOriginalType(NewV, OldT);

  if(OldT != NewT)
    OldV->mutateType(NewT);
  if(OldV != NewV) {
    OldV->replaceAllUsesWith(NewV);
    if(isa<Instruction>(OldV))
      cast<Instruction>(OldV)->eraseFromParent();
  }
}

GlobalValue* PromoteSimpleStructs::getPromoted(GlobalValue* F) {
  if(!m_promoted.count(F)) {
    if(isa<Function>(F)) {
      promoteFunction(*cast<Function>(F), true);
    } else if(isa<GlobalVariable>(F)) {
      promoteGlobal(cast<GlobalVariable>(F));
    }
  }
  return F;
}
Type* PromoteSimpleStructs::getOriginalType(Value* V) {
  if(isa<Constant>(V) && !isa<GlobalValue>(V)) {
    return V->getType();
  } else if(GlobalVariable* G = dyn_cast<GlobalVariable>(V)) {
    if(!isGlobalVariablePromotable(G)) {
      return G->getType();
    }
  }

  origin_ty_iterator i = m_original_types.find(V);
  if(i == m_original_types.end()) {
      errs() << "Value: " << ToStr(*V) << "\n";
#ifndef NDEBUG
      assert(0 && "Couldn't find the original type!");
#endif
      llvm_unreachable("Couldn't find the original type!");
  } else
    return i->second;
}
Type* PromoteSimpleStructs::getPromotedType(Type* T,
					    const PromotedType** Raw,
					    const bool Protected) {
  const ty_iterator i = m_promoted_types.find(T);
  if(i == m_promoted_types.end()) {
    PromotedType NewT = getPromotedTypeImpl(T, Protected);
    assert(NewT.isFinished());

    PromotedType* Instance =
      &m_promoted_types.insert(std::make_pair(T, NewT)).first->second;
    m_promoted_types.insert(std::make_pair(NewT.Promoted, NewT));
    if(Raw != NULL)
      *Raw = Instance;
    return NewT.Promoted;
  } else {
    if(Raw != NULL)
      *Raw = &i->second;
    return i->second.Promoted;
  }
}
PromotedType PromoteSimpleStructs::getPromotedTypeImpl(Type* T, const bool Protected) {
  if(FunctionType* FT = dyn_cast<FunctionType>(T)) {
    Type* RetT;
    RetT = getPromotedType(FT->getReturnType());

    std::vector<Type*> ArgTs;
    ArgTs.reserve(FT->getNumParams());
  
    const FunctionType::param_iterator i_end = FT->param_end();
    for(FunctionType::param_iterator i = FT->param_begin(); i != i_end; ++i) {
        ArgTs.push_back(getPromotedType(*i));
    }

    return PromotedType::start(T, Protected)
      .finish(FunctionType::get(RetT, ArgTs, FT->isVarArg()));
  } else if(PointerType* PT = dyn_cast<PointerType>(T)) {
    Type* InnerTy = PT->getElementType();
    Type* NewInnerTy = getPromotedType(InnerTy);
    return PromotedType::start(T, Protected)
      .finish(PointerType::get(NewInnerTy, 0));
  } else if(StructType* ST = dyn_cast<StructType>(T)) {
    if(ST->getNumElements() == 0)
      return PromotedType::noOp(T, Protected);
    Type* OnlyMeaningfulTy = getInnerMeaningfulType(ST);
    if(OnlyMeaningfulTy != NULL) {
      PromotedType PT;
      getPromotedType(OnlyMeaningfulTy, &PT);
      return PT.promote(T);
    }

    StructType* Struct;
    if(ST->hasName())
      Struct = StructType::create(T->getContext(), ST->getName());
    else
      Struct = StructType::create(T->getContext());
    // This is a requisite for recursive structures.
    ty_iterator It = m_promoted_types.insert
      (std::make_pair(T, PromotedType::startWithPlaceholder(T, Struct, Protected))).first;

    std::vector<Type*> ArgTs;
    ArgTs.reserve(ST->getNumElements());

    size_t BIdx = 0;
    const StructType::element_iterator end = ST->element_end();
    for(StructType::element_iterator i = ST->element_begin(); i != end; ++i, ++BIdx) {
      Type* Old = *i;
      const PromotedType* Inner = NULL;
      Type* New = getPromotedType(Old, &Inner);
      // If it's in-progress, by virtue of this implementation, it must be a meaningful type.
      if(Inner->InProgress || isMeaningful(New))
	ArgTs.push_back(New);
      else
	It->second.bump(BIdx);
    }
    Struct->setBody(ArgTs);
    return It->second.finishPlaceholder();
  } else if(ArrayType* AT = dyn_cast<ArrayType>(T)) {
    PromotedType InnerPT;
    getPromotedType(AT->getElementType(), &InnerPT);
    if(AT->getNumElements() == 1)
      return InnerPT.promote(T);

    return PromotedType::start(T, Protected)
      .finish(ArrayType::get(InnerPT.Promoted,
			     AT->getNumElements()));
  } else {
    return PromotedType::noOp(T, Protected);
  }
}

struct PromotionChainJanitor {
  std::set<Type*>& chain;
  Type* t;
  PromotionChainJanitor(Type* T, std::set<Type*>& Chain) 
    : chain(Chain)
    , t(T) {
    assert(chain.count(T) == 0);
    chain.insert(T);
  }
  ~PromotionChainJanitor() {
    assert(chain.count(t) != 0);
    chain.erase(t);
  }
};

bool PromoteSimpleStructs::shouldPromote(Type* T, std::set<Type*>& Chain) {
  assert(T != NULL);
  if(Chain.count(T) > 0)
    return false;

  PromotionChainJanitor cleanup(T, Chain);

  if(isa<FunctionType>(T)) {
    FunctionType* FT = cast<FunctionType>(T);
    if(shouldPromote(FT->getReturnType(), Chain))
      return true;
      
    const FunctionType::param_iterator end = FT->param_end();
    for(FunctionType::param_iterator i = FT->param_begin(); i != end; ++i) {
      if(shouldPromote(*i, Chain))
        return true;
    }

    return false;
  } else if(isa<StructType>(T)) {
    StructType* ST = cast<StructType>(T);
    if(ST->getNumElements() == 1)
      return true;

    const StructType::element_iterator end = ST->element_end();
    for(StructType::element_iterator i = ST->element_begin(); i != end; ++i) {
      // short cut for recursive structures
      if(shouldPromote(*i, Chain))
        return true;
    }
    return false;
  }

  return (isa<PointerType>(T) && shouldPromote(cast<PointerType>(T)->getElementType(), Chain)) ||
    (isa<ArrayType>(T)        && (cast<ArrayType>(T)->getNumElements() == 1 ||
                                  shouldPromote(T->getContainedType(0), Chain)));
}
bool PromoteSimpleStructs::shouldPromote(Function* F) {
  return F && shouldPromote(F->getFunctionType());
}
bool PromoteSimpleStructs::isAggregateType(Type* T) {
  return T && (isa<StructType>(T) || isa<ArrayType>(T));
}

void PromoteSimpleStructs::ConversionState::recordConverted(Instruction* I) {
  if(!I) {
    errs() << __FUNCTION__ << ":\n";
    assert(0);
    llvm_unreachable("I is NULL");
  }
  const bool result = m_replacements.insert(I).second;
  assert(result && "Instruction already patched!");
  (void)result;
}
Value* PromoteSimpleStructs::ConversionState::get(Value* From, Type** OldTy) {
  if(isa<Argument>(From)) {
    if(OldTy != NULL)
        *OldTy = m_p->getOriginalType(From);
    return From;
  } else if(isa<Instruction>(From)) {
    Instruction* Inst = cast<Instruction>(From);
    if(m_replacements.count(Inst)) {
      if(OldTy != NULL)
        *OldTy = m_p->getOriginalType(From);
      return From;
    } else {
      if(OldTy != NULL)
        *OldTy = Inst->getType();
      return convertInstruction(Inst);
    }
  } else if(isa<GlobalValue>(From)) {
    Value* Promoted = m_p->getPromoted(cast<GlobalValue>(From));
    if(OldTy != NULL)
      *OldTy = m_p->getOriginalType(From);
    return Promoted;
  } else if(isa<Constant>(From)) {
    if(OldTy != NULL)
      *OldTy = From->getType();
    return m_p->getPromotedConstant(cast<Constant>(From));
  } else {
    if(OldTy != NULL)
      *OldTy = From->getType();
    return From;
  }
}

void PromoteSimpleStructs::ConversionState::convertOperands(User* From) {
  unsigned j = 0;
  User::op_iterator i = From->op_begin();
  for(bool FirstIteration = true;; FirstIteration = false) {
    // Keep i an element before the current. This way, if the current operand
    // is replaced, our iterator won't be invalidated. This works because the
    // previous position is guaranteed to be already converted, and hence
    // won't give us nasty surprises.
    User::op_iterator Next = i;

    if(!FirstIteration) {
      ++Next;
      ++j;
    }
    if(Next == From->op_end()) {
      break;
    }

    Value* NextValue = *Next;
    Value* R = get(NextValue);

    // sometimes constant prop will short circuit an expression
    // possibly yielding a global var; hence check the old operand
    // for global var-ness.
    if(isa<Constant>(R) && !isa<GlobalValue>(NextValue))
      From->setOperand(j, R);

    if(FirstIteration) {
      // iterator could be invalidated if the first operand was replaced.
      i = From->op_begin();
    } else {
      ++i;
    }
  }
}

void PromoteSimpleStructs::ConversionState::convertBlock(BasicBlock* Bb) {
  bool FirstIteration = true;
  BasicBlock::iterator Prev = Bb->begin();
  for(;Prev != Bb->end();) {
    BasicBlock::iterator Next = Prev;
    if(!FirstIteration) {
      ++Next;
      if(Next == Bb->end()) {
	break;
      }
    }
    
    Instruction* Inst = cast<Instruction>(&(*Next));
    if(!m_replacements.count(Inst))
      convertInstruction(Inst);

    if(FirstIteration) {
      if(Prev != Bb->begin()) {
	// The first instruction was replaced.
	// We can't just unset FirstIteration or else we'll end up skipping
	// the new head instruction (previously the second instruction
	// in this block).
	Prev = Bb->begin();
      } else {
	// The head instruction was not replaced. Commence bumping.
	FirstIteration = false;
      }
    } else {
      BasicBlock::iterator PrevNext = Prev; ++PrevNext;

      // If the current (or rather the last) instruction was replaced, we can't
      // ++Next lest we skip the next (now the current) instruction.
      // We check by imcrementing a copy of Prev and compairing that to Next.
      // If they differ, an instruction was replaced/removed/etc.
      if(PrevNext == Next) {
	Prev = PrevNext;
      }
    }
  }
}
void PromoteSimpleStructs::ConversionState::possiblyConvertUsers(Instruction* Inst,
                                                                 Value* Replacement,
                                                                 Type* OriginalTy) {
  const Value::user_iterator end = Inst->user_end();
  for(Value::user_iterator i = Inst->user_begin(); i != end;) {
    if(!isa<Instruction>(*i)) {
      ++i;
      continue;
    } else if(m_replacements.count(cast<Instruction>(*i))) {
      ++i;
      continue;
    } else if(isa<GetElementPtrInst>(*i)) {
      GetElementPtrInst* GEP = cast<GetElementPtrInst>(*i++);
      if(GEP->getPointerOperand() != Inst)
        continue;

      Type* GEPOriginalTy = GEP->getType();
      Type* GEPPromotedTy = m_p->getPromotedType(GEPOriginalTy);

      Value* Converted;
      recordConverted(GEP);
      Converted = convertGEPInstruction(GEP,
					GEPOriginalTy,
					GEPPromotedTy,
					Replacement,
					OriginalTy);
      if(Converted != GEP)
	eraseConverted(GEP);
    } else if(isa<ExtractValueInst>(*i)) {
      ExtractValueInst* EV = cast<ExtractValueInst>(*i++);
      if(EV->getAggregateOperand() != Inst)
        continue;

      Type* EVOriginalTy = EV->getType();
      Type* EVPromotedTy = m_p->getPromotedType(EVOriginalTy);

      Value* Converted;
      recordConverted(EV);
      Converted = convertEVOrIVInstruction(EV,
                                           EVOriginalTy,
                                           EVPromotedTy,
                                           Replacement,
                                           OriginalTy);
      if(Converted != EV)
        eraseConverted(EV);

    } else if(isa<InsertValueInst>(*i)) {
      InsertValueInst* IV = cast<InsertValueInst>(*i++);
      if(IV->getAggregateOperand() != Inst)
        continue;

      Type* IVOriginalTy = IV->getType();
      Type* IVPromotedTy = m_p->getPromotedType(IVOriginalTy);
      
      Value* Converted;

      recordConverted(IV);
      Converted = convertEVOrIVInstruction(IV,
                                           IVOriginalTy,
                                           IVPromotedTy,
                                           Replacement,
                                           OriginalTy);
      if(Converted != IV)
        eraseConverted(IV);
    } else
      ++i;
  }
}
Value* PromoteSimpleStructs::ConversionState::convertEVOrIVInstruction(Instruction* I,
                                                                       Type* OriginalTy,
                                                                       Type* PromotedTy,
                                                                       Value* AggOp,
                                                                       Type* AggOpOriginalTy) {
  Value* Converted = NULL;
  ArrayRef<unsigned> Indices;

  if(isa<ExtractValueInst>(I))
    Indices = cast<ExtractValueInst>(I)->getIndices();
  else if(isa<InsertValueInst>(I))
    Indices = cast<InsertValueInst>(I)->getIndices();
      
  std::vector<unsigned> NewIndices;
  std::vector<unsigned> OldIndices;
  OldIndices.reserve(Indices.size());
  NewIndices.reserve(Indices.size());

  Type* LastTy = AggOpOriginalTy;
  const size_t end = Indices.size();
  for(size_t i = 0; i < end; ++i) {
    const unsigned Idx = Indices[i];
    OldIndices.push_back(Idx);
    Type* Ty = ExtractValueInst::getIndexedType(AggOpOriginalTy, OldIndices);

    if(!isMeaningful(Ty)) {
      // if this is one of those awkward 0-byte aggregates, replace with an undef value.      
      Converted = UndefValue::get(PromotedTy);
      possiblyConvertUsers(I, Converted, OriginalTy);
      I->mutateType(PromotedTy);
      I->replaceAllUsesWith(Converted);
      I->eraseFromParent();
      return Converted;
    }
    if(!isShallowPromotable(LastTy)) {
      const PromotedType* PTy = NULL;
      m_p->getPromotedType(LastTy, &PTy);
      NewIndices.push_back(PTy->bumpOffset(Idx));
    }
    LastTy = Ty;
  }
  if(NewIndices.size() == 0) {
    if(isa<ExtractValueInst>(I))
      Converted = AggOp;
    else if(isa<InsertValueInst>(I))
      Converted = get(cast<InsertValueInst>(I)->getInsertedValueOperand());

    m_p->recordOriginalType(I, OriginalTy);
    I->mutateType(PromotedTy);
    possiblyConvertUsers(I, Converted, OriginalTy);
    m_p->eraseOriginalType(I);
    I->replaceAllUsesWith(Converted);
    I->eraseFromParent();
  } else if(NewIndices == OldIndices) {
    if(isa<ExtractValueInst>(I))
      I->setOperand(ExtractValueInst::getAggregateOperandIndex(), AggOp);
    else if(isa<InsertValueInst>(I)) {
      I->setOperand(InsertValueInst::getAggregateOperandIndex(), AggOp);
      I->setOperand(InsertValueInst::getInsertedValueOperandIndex(),
                    get(cast<InsertValueInst>(I)->getInsertedValueOperand()));
    }
    Converted = I;
    m_p->mutateAndReplace(I, I, OriginalTy, PromotedTy);
  } else {
    Instruction* NewI;
    if(isa<ExtractValueInst>(I))
      NewI = CopyDebug(ExtractValueInst::Create(AggOp,
                                                NewIndices,
                                                "",
                                                I),
                       I);
    else if(isa<InsertValueInst>(I)) {
      // make sure AggOp has our expected resultant type: 
      AggOp->mutateType(PromotedTy);
      NewI =
        CopyDebug(InsertValueInst::Create(AggOp,
                                          get(cast<InsertValueInst>(I)->getInsertedValueOperand()),
                                          NewIndices,
                                          "",
                                          I),
                  I);
    }

    recordConverted(NewI);
    Converted = NewI;
    m_p->mutateAndReplace(I, NewI, OriginalTy, PromotedTy);
  }
  return Converted;
}

void FlushOffset(Instruction **Ptr, uint64_t *CurrentOffset,
		 Instruction *InsertPt, Type *PtrType);
Value *CastToPtrSize(Value *Val, Instruction *InsertPt, Type *PtrType);

Value* PromoteSimpleStructs::ConversionState::convertGEPInstruction(GetElementPtrInst* Inst,
                                                                    Type* OriginalTy,
                                                                    Type* PromotedTy,
                                                                    Value* PointerOp,
                                                                    Type* PointerOpOriginalTy) {
  Value* Converted = NULL;
  std::vector<Value*> OldIndices;
  std::vector<Value*> NewIndices;
  OldIndices.reserve(Inst->getNumIndices());
  NewIndices.reserve(Inst->getNumIndices());

  Type* LastTy = PointerOpOriginalTy->getContainedType(0);
  bool ForceInitialIndex = isShallowPromotable(LastTy);
  const User::op_iterator end = Inst->idx_end();
  for(User::op_iterator i = Inst->idx_begin(); i != end; ++i) {
    Value* IdxVal = cast<Value>(*i);
    OldIndices.push_back(IdxVal);
    Type* NextTy = GetElementPtrInst::getIndexedType(PointerOpOriginalTy,
						     OldIndices);
    if(!isMeaningful(NextTy)) {
      // if this is one of those awkward 0-byte aggregates. Replace with some
      // pointer arithmetic to point the pointer to one byte past the end.
      // We'll remove any useless stores/loads with these pointers so all these
      // can be used for is sentinel values.
      // Important: be sure to never insert before Inst so we don't invalidate
      // any iterators.

      DataLayout DL(m_f->getParent());
      IntegerType* IntPtrTy = DL.getIntPtrType(PointerOp->getContext());
      Instruction *Ptr = CopyDebug(new PtrToIntInst(PointerOp,
						    IntPtrTy),
				   Inst);
      Ptr->insertAfter(Inst);

      // 'borrowed' from ExpandGEP in ExpandGetElementPtr.cpp
      uint64_t CurrentOffset = 0;
      Type* CurrentTy = PointerOpOriginalTy;
      for (GetElementPtrInst::op_iterator Op = Inst->op_begin() + 1;
	   Op != Inst->op_end(); ++Op) {
	Value *Index = *Op;
	if (StructType *StTy = dyn_cast<StructType>(CurrentTy)) {
	  uint64_t Field = cast<ConstantInt>(Op)->getZExtValue();
	  CurrentTy = StTy->getElementType(Field);
	  CurrentOffset += DL.getStructLayout(StTy)->getElementOffset(Field);
	} else {
	  CurrentTy = cast<SequentialType>(CurrentTy)->getElementType();
	  uint64_t ElementSize = DL.getTypeAllocSize(CurrentTy);
	  if (ConstantInt *C = dyn_cast<ConstantInt>(Index)) {
	    CurrentOffset += C->getSExtValue() * ElementSize;
	  } else {
	    Instruction* Prev = Ptr;
	    FlushOffset(&Ptr, &CurrentOffset, NULL, IntPtrTy);
	    if(Ptr->getParent() == NULL) {
	      Ptr->insertAfter(Prev);
	      Prev = Ptr;
	    }
	    Index = CopyDebug(CastToPtrSize(Index, NULL, IntPtrTy), Inst);
	    if(Instruction* I = dyn_cast<Instruction>(Index)) {
	      if(I->getParent() == NULL) {
		I->insertAfter(Prev);
		Prev = I;
	      }
	    }
	    if (ElementSize != 1) {
	      Index = CopyDebug(BinaryOperator::Create(Instruction::Mul,
						       Index,
						       ConstantInt::get(IntPtrTy, ElementSize)),
				Inst);
	      if(Instruction* I = dyn_cast<Instruction>(Index)) {
		if(I->getParent() == NULL) {
		  I->insertAfter(Prev);
		  Prev = I;
		}
	      }
	    }
	    Ptr = BinaryOperator::Create(Instruction::Add,
					 Ptr,
					 Index);
	    CopyDebug(Ptr, Inst);
	    Ptr->insertAfter(Prev);
	  }
	}
      }
      Instruction* PrevPtr = Ptr;
      FlushOffset(&Ptr, &CurrentOffset, NULL, IntPtrTy);
      if(Ptr->getParent() == NULL) {
	Ptr->insertAfter(PrevPtr);
      }
      IntToPtrInst* Result = CopyDebug(new IntToPtrInst(Ptr,
							PromotedTy),
				       Inst);
      Result->insertAfter(Ptr);
      Converted = Result;
      possiblyConvertUsers(Inst, Converted, OriginalTy);
      Inst->mutateType(PromotedTy);
      Inst->replaceAllUsesWith(Converted);
      Inst->eraseFromParent();
      return Converted;
    }

    if(!isShallowPromotable(LastTy) || ForceInitialIndex) {
      // check for array element by ptr offset, ie (&a[0])[index].
      // in such cases it would be invalid to bump the index offset.
      if(LastTy != NextTy) {
	const PromotedType* Last = NULL;
	m_p->getPromotedType(LastTy, &Last);
	IdxVal = Last->bumpOffset(IdxVal);
      }
      NewIndices.push_back(IdxVal);
      ForceInitialIndex = false;
    }
    LastTy = NextTy;
  }
  assert(Converted == NULL);
  if(NewIndices.size() == 0 ||
     (NewIndices.size() == 1 && 
      isa<ConstantInt>(NewIndices[0]) &&
      cast<ConstantInt>(NewIndices[0])->isZero())) {
    possiblyConvertUsers(Inst, PointerOp, OriginalTy);
    Converted = PointerOp;
    Inst->mutateType(PromotedTy);
    Inst->replaceAllUsesWith(Converted);
    Inst->eraseFromParent();
  } else {
    GetElementPtrInst* GEPI = GetElementPtrInst::Create(PointerOp,
							NewIndices,
							Inst->getName(),
							Inst);
    GEPI->setIsInBounds(Inst->isInBounds());
    CopyDebug(Inst, GEPI);
    Converted = GEPI;
    recordConverted(GEPI);
    /// Sometimes pointer types are created (outside of our control, that is)
    /// that cause GEPI's type pointer to differ from the type of what we are
    /// replacing, even when they are otherwise identical.
    m_p->mutateAndReplace(Inst, GEPI, OriginalTy, GEPI->getType());
  }
  return Converted;
}
template <class T>
T* PromoteSimpleStructs::ConversionState::convertCall(T* Call,
						      Type* OriginalTy,
						      Type* PromotedTy) {
  Value* OldCalled = Call->getCalledValue();
  Value* NewCalled = get(OldCalled);
  T* NewCall = Call;
  
  std::vector<Value*> Args;
  const unsigned end = Call->getNumArgOperands();
  Args.reserve(end);
  // Sometimes in C/CXX we will loop back around to this instruction.
  // We record the original type here so instructions like GEP can still
  // find the original type. After we finish promoting the call's args,
  // we remove the original type entry.
  m_p->recordOriginalType(Call, OriginalTy);
  for(unsigned i = 0; i < end; ++i) {
    Value* V = Call->getArgOperand(i);
    Value* NewV = get(V);
    Args.push_back(NewV);
  }
  m_p->eraseOriginalType(Call);

  if(OldCalled != NewCalled) {
    Value* BaseCall = NULL;
    if(isa<InvokeInst>(Call)) {
      InvokeInst* Inst = cast<InvokeInst>(Call);
      BaseCall = CopyDebug(InvokeInst::Create(NewCalled,
                                              Inst->getNormalDest(),
                                              Inst->getUnwindDest(),
                                              Args,
                                              "",
                                              Call),
                           Call);
    } else if(isa<CallInst>(Call)) {
      CallInst* OldCall = cast<CallInst>(Call);
      CallInst* NewCall = CopyDebug(CallInst::Create(NewCalled,
						     Args,
						     "",
						     Call),
				    Call);
      if(OldCall->canReturnTwice())
        NewCall->setCanReturnTwice();
      if(OldCall->cannotDuplicate())
        NewCall->setCannotDuplicate();
      BaseCall = NewCall;
    }
    NewCall = cast<T>(BaseCall);
    if(Call->doesNotThrow())
      NewCall->setDoesNotThrow();
    if(Call->isNoInline())
      NewCall->setIsNoInline();
    if(Call->doesNotAccessMemory())
      NewCall->setDoesNotAccessMemory();
    if(Call->doesNotReturn())
      NewCall->setDoesNotReturn();
    if(Call->onlyReadsMemory())
      NewCall->setOnlyReadsMemory();

    recordConverted(NewCall);
  } else {
    for(unsigned i = 0; i < end; ++i) {
      Value* V = Args[i];
      Call->setArgOperand(i, V);
    }
  }
  m_p->mutateAndReplace(Call, NewCall, OriginalTy, PromotedTy);
  return NewCall;
}
Value* PromoteSimpleStructs::ConversionState::convertInstruction(Instruction* I) {
  recordConverted(I);
  Value* Converted = NULL;
  Type* OriginalType = I->getType();
  Type* PromotedType = m_p->getPromotedType(OriginalType);

  if(isa<GetElementPtrInst>(I)) {
    GetElementPtrInst* Inst = cast<GetElementPtrInst>(I);
    Value* PointerOp = Inst->getPointerOperand();
    Type* PointerOpOriginalTy;
    PointerOp = get(PointerOp, &PointerOpOriginalTy);
    Converted = convertGEPInstruction(Inst,
                                      OriginalType,
                                      PromotedType,
                                      PointerOp,
                                      PointerOpOriginalTy);
  } else if(isa<ExtractValueInst>(I) || isa<InsertValueInst>(I)) {
    Value* AggOp;
    if(isa<ExtractValueInst>(I))
      AggOp = cast<ExtractValueInst>(I)->getAggregateOperand();
    else if(isa<InsertValueInst>(I))
      AggOp = cast<InsertValueInst>(I)->getAggregateOperand();

    Type* AggOpOriginalTy;
    AggOp = get(AggOp, &AggOpOriginalTy);

    Converted = convertEVOrIVInstruction(I,
                                         OriginalType,
                                         PromotedType,
                                         AggOp,
                                         AggOpOriginalTy);
  } else if(isa<PHINode>(I)) {
    PHINode* Phi = cast<PHINode>(I);
    m_p->mutateAndReplace(I, I, OriginalType, PromotedType);

    for(size_t l = 0; l < Phi->getNumIncomingValues(); ++l) {
      Value* NewIn = get(Phi->getIncomingValue(l));
      Phi->setIncomingValue(l, NewIn);
    }
    Converted = Phi;
  } else if(isa<CallInst>(I) || isa<InvokeInst>(I)) {
    if(isa<CallInst>(I)) {
      CallInst* Call = cast<CallInst>(I);
      Converted = convertCall(Call, OriginalType, PromotedType);
    } else if(isa<InvokeInst>(I)) {
      InvokeInst* Invoke = cast<InvokeInst>(I);
      Converted = convertCall(Invoke, OriginalType, PromotedType);
    }
  } else if(isa<LoadInst>(I) && !isMeaningful(PromotedType)) {
    Converted = UndefValue::get(PromotedType);
    I->mutateType(PromotedType);
    I->replaceAllUsesWith(Converted);
    I->eraseFromParent();
  } else if(StoreInst* Store = dyn_cast<StoreInst>(I)) {
    Value* ValOp = Store->getValueOperand();
    Type* ValOriginalTy = ValOp->getType();
    Type* ValPromotedTy = m_p->getPromotedType(ValOriginalTy);
    if(isMeaningful(ValPromotedTy)) {
      m_p->mutateAndReplace(I, I, OriginalType, PromotedType);
      convertOperands(I);
      Converted = I;
    } else {
      return NULL;
    }
  } else {
    m_p->mutateAndReplace(I, I, OriginalType, PromotedType);
    convertOperands(I);
    Converted = I;
  }

  assert(Converted != NULL);
  if(Converted != I)
    eraseConverted(I);
  return Converted;
}
Constant* PromoteSimpleStructs::getPromotedConstant(Constant* C) {
  if(isa<GlobalValue>(C)) {
    return getPromoted(cast<GlobalValue>(C));
  }

  User::op_iterator i;
  Type* OriginalT;
  Type* NewT;

  std::pair<const_iterator, bool> ci =
    m_promoted_consts.insert(std::make_pair(C, (Constant*)NULL));
  // If ci.first->second is NULL, we've encountered a recursion.
  // See the comment in the ConstantExpr branch.
  if(!ci.second && ci.first->second != NULL) {
    return ci.first->second;
  }
  Constant*& NewC = ci.first->second;

  OriginalT = C->getType();
  NewT = getPromotedType(OriginalT);

  if(isa<UndefValue>(C)) { // fast path for this common const
    NewC = UndefValue::get(NewT);
  } else if(isa<ConstantExpr>(C)) {
    ConstantExpr* CE = cast<ConstantExpr>(C);
    unsigned Code = CE->getOpcode();

    i = C->op_begin();

    Constant* Agg = cast<Constant>(i++);
    Type* AggOriginalTy;
    const bool IsGlobal = isa<GlobalValue>(Agg);
    if(!IsGlobal)
      AggOriginalTy = Agg->getType();
    Agg = getPromotedConstant(Agg);
    if(IsGlobal)
      AggOriginalTy = getOriginalType(Agg);

    if(Code == Instruction::GetElementPtr) {
      assert(isa<PointerType>(AggOriginalTy) && "Original type isn't a pointer!");
      
      std::vector<Constant*> NewIndices;
      {
        std::vector<Constant*> OldIndices;
        OldIndices.reserve(C->getNumOperands());
        NewIndices.reserve(C->getNumOperands());

	Type* LastTy = AggOriginalTy->getContainedType(0);
	// check for array element by ptr offset, ie (&a[0])[index].
	// in such cases it would be invalid to bump the index offset.
	// If the first element is also promotable, without this check
	// we'd skip the array element offset.
	bool ForceInitialIndex = isShallowPromotable(LastTy);
        const User::op_iterator end = C->op_end();
        for(; i != end; ++i) {
          Constant* C2 = cast<Constant>(*i);
          Constant* C3 = getPromotedConstant(C2);

	  // One of the operands caused us to circle back around to this const.
	  // The only way this can happen is through a global, which means the second time around
	  // would skip the global causing the recursion, allowing the promotion to finish.
	  // if all that happens, our reference into the map will reflect the promotion,
	  // NewC != NULL, and we can just return.
	  if(NewC != NULL)
	    return NewC;

          OldIndices.push_back(C2);          
          Type* NextTy = GetElementPtrInst::getIndexedType(AggOriginalTy,
							   OldIndices);
          if(!isMeaningful(NextTy)) {
	    NewIndices.clear();
	    // if this is one of those awkward 0-byte aggregates, replace with an undef value.
	    Agg = UndefValue::get(NewT);
	    break;
	  }

	  if(!isShallowPromotable(LastTy) ||
	     ForceInitialIndex ||
	     getPromotedTypeData(LastTy).Protected) {
	    if(ConstantInt* IdxVal = dyn_cast<ConstantInt>(C3)) {
	      const PromotedType* Last = NULL;
	      getPromotedType(LastTy, &Last);
	      Value* Bumped = Last->bumpOffset(IdxVal);
	      C3 = cast<Constant>(Bumped);
	    }
	    NewIndices.push_back(C3);
	    ForceInitialIndex = false;
	  }
	  LastTy = NextTy;
        }
      }
      if(NewIndices.size() != 0)
        NewC = ConstantExpr::getGetElementPtr(Agg, NewIndices);
      else
        NewC = Agg;
    } else if(CE->hasIndices()) {
      Constant* Inserted = getPromotedConstant(cast<Constant>(i++));

      std::vector<unsigned> NewIndices;
      {
        std::vector<unsigned> OldIndices;
        OldIndices.reserve(C->getNumOperands());
        NewIndices.reserve(C->getNumOperands());

        const ArrayRef<unsigned> Indices = CE->getIndices();
        const size_t end = Indices.size();
	Type* LastTy = AggOriginalTy;
        for(size_t i = 0; i < end; ++i) {
          unsigned Idx = Indices[i];

          OldIndices.push_back(Idx);
          Type* NextTy = ExtractValueInst::getIndexedType(AggOriginalTy, OldIndices);
          if(!isMeaningful(NextTy)) {
	    // if this is one of those awkward 0-byte aggregates, replace with an undef value.
	    NewC = UndefValue::get(NewT);
	    break;
	  }
	  if(!isShallowPromotable(LastTy)) {
	    const PromotedType* PTy = NULL;
	    getPromotedType(LastTy, &PTy);
	    NewIndices.push_back(PTy->bumpOffset(Idx));
	  }
	  LastTy = NextTy;
        }
      }
      if(NewC == NULL) {
	if(Code == Instruction::ExtractValue)
	  NewC = ConstantExpr::getExtractValue(Agg, NewIndices);
	else if(Code == Instruction::InsertValue)
	  NewC = ConstantExpr::getInsertValue(Agg,
					      Inserted,
					      NewIndices);
      }
    } else {
      std::vector<Constant*> Consts;
      Consts.reserve(C->getNumOperands());
      const User::op_iterator end = C->op_end();
      for(i = C->op_begin(); i != end; ++i) {
        Constant* C2 = cast<Constant>(*i);
        Consts.push_back(getPromotedConstant(C2));
      }
      NewC = CE->getWithOperands(Consts, NewT);
    }
  } else if(isShallowPromotable(C->getType())) {
    if(isa<ConstantStruct>(C) || isa<ConstantArray>(C))
      NewC = getPromotedConstant(C->getAggregateElement((unsigned)0));
    else if(isa<ConstantAggregateZero>(C)) {
      if(isAggregateType(NewT) || isa<VectorType>(NewT)) {
        NewC = ConstantAggregateZero::get(NewT);
      } else if(isa<PointerType>(NewT)) {
        NewC = ConstantPointerNull::get(cast<PointerType>(NewT));
      } else if(NewT->isIntegerTy()) {
        IntegerType* IntTy = cast<IntegerType>(NewT);
        NewC = ConstantInt::get(IntTy, 0, IntTy->getSignBit());
      } else if(NewT->isFloatingPointTy()) {
        NewC = ConstantFP::get(NewT, 0.0);
      } else {
        errs() << "Constant: " << ToStr(*C) << "\n";
        assert(0 && "Unhandled else");
        llvm_unreachable("Unhandled else");
      }
    } else if(isa<ConstantPointerNull>(C)) {
      NewC = ConstantPointerNull::get(cast<PointerType>(NewT));
    } else if(isa<ConstantDataSequential>(C))
      NewC = cast<ConstantDataSequential>(C)->getElementAsConstant(0);
    else
      NewC = getPromotedConstant(cast<Constant>(C->getOperand(0)));
  } else if(isa<ConstantDataSequential>(C)) {
    if(cast<ConstantDataSequential>(C)->getNumElements() == 1)
      NewC = cast<ConstantDataSequential>(C)->getElementAsConstant(0);
    else
      NewC = C;
  } else if(isa<ConstantStruct>(C) ||
            isa<ConstantArray>(C)) {
    std::vector<Constant*> Consts;
    Consts.reserve(C->getNumOperands());
    const User::op_iterator end = C->op_end();
    size_t Idx = 0;
    const PromotedType* PT = NULL; getPromotedType(OriginalT, &PT);
    for(i = C->op_begin(); i != end; ++i, ++Idx) {
      Constant* OldC = cast<Constant>(*i);
      if(!PT->isBumped(Idx)) {
	Constant* NewC = getPromotedConstant(OldC);
	Consts.push_back(NewC);
      }
    }
    if(isa<ConstantStruct>(C))
      NewC = ConstantStruct::get(cast<StructType>(NewT), Consts);
    else if(isa<ConstantArray>(C))
      NewC = ConstantArray::get(cast<ArrayType>(NewT), Consts);
  } else if(isa<BlockAddress>(C)) {
    BlockAddress* Addr = cast<BlockAddress>(C);

    // make sure the function type is promoted
    getPromoted(Addr->getFunction());
    NewC = C;
  } else if(isa<ConstantPointerNull>(C)) {
    NewC = ConstantPointerNull::get(cast<PointerType>(NewT));
  } else if(isa<ConstantAggregateZero>(C)) {
    NewC = ConstantAggregateZero::get(NewT);
  } else {
    NewC = C;
  }

  assert(NewC != NULL && "NewC is NULL");
  assert(NewC->getType() == NewT);
  m_promoted_consts.insert(std::make_pair(NewC, NewC));
  return NewC;
}


void PromoteSimpleStructs::promoteGlobal(GlobalVariable* G) {
  std::pair<iterator, bool> result = m_promoted.insert(G);
  if(!result.second)
    return;
  else if(!isGlobalVariablePromotable(G)) {
    recordOriginalType(G, G->getType());
    return;
  }
  
  PointerType* OriginalTy = G->getType();
  PointerType* NewTy = cast<PointerType>(getPromotedType(OriginalTy));
  mutateAndReplace(G, G, OriginalTy, NewTy);
  if(G->hasInitializer()) {
    Constant* Old = G->getInitializer();
    Constant* Initer = getPromotedConstant(Old);
    G->setInitializer(Initer);
  }
}

static size_t ConvertedFunctions;

Function* PromoteSimpleStructs::promoteFunction(Function& F, const bool PrototypeOnly) {
  if(F.isIntrinsic()) {
    m_promoted.insert(&F);
    return &F;
  }

  std::pair<iterator, bool> result = m_promoted.insert(&F);
  Type* NewTy;
  Function* NewF;
  if(result.second) {
    Type* OriginalTy = F.getType();
    NewTy = getPromotedType(OriginalTy);
    mutateAndReplace(&F, &F, OriginalTy, NewTy);
    NewF = &F;
  } else {
    NewF = &F;
    NewTy = NewF->getType();
  }

  if(PrototypeOnly)
    return NewF;

  ConversionState State(this, &F);

  const Function::iterator i_end = F.end();
  for(Function::iterator i = F.begin(); i != i_end; ++i) {
    State.convertBlock(&*i);
  } // function

  ConvertedFunctions++;

  return NewF;
}

bool PromoteSimpleStructs::runOnModule(Module& M) {
  m_module = &M;
  {
    const Module::global_iterator end = M.global_end();
    for(Module::global_iterator i = M.global_begin(); i != end; ++i) {
      if(!isGlobalVariablePromotable(i)) {
	// Protect the value type of the var:
	protectType(i->getType()->getContainedType(0));
	recordOriginalType(i, i->getType());
      }
    }
  }

  size_t PromotedFunctions = 0;
  const Module::iterator i_end = M.end();
  for(Module::iterator i = M.begin(); i != i_end; ++i) {
    promoteFunction(*i, false);
    ++PromotedFunctions;
  }
  size_t PromotedGlobals = 0;
  const Module::global_iterator j_end = M.global_end();
  for(Module::global_iterator j = M.global_begin(); j != j_end; ++j) {
    promoteGlobal(j);
    ++PromotedGlobals;
  }
  
  // Ensure alias types are up-to-date:
  for (Module::alias_iterator I = M.alias_begin(), E = M.alias_end();
       I != E; ++I) {
    auto* Aliasee = getPromotedConstant(I->getAliasee());
    I->mutateType(Aliasee->getType());
    I->setAliasee(Aliasee);
  }

  // remove dangling consts:
  {
    const const_iterator end = m_promoted_consts.end();
    for(const_iterator i = m_promoted_consts.begin(); i != end; ++i) {
      (*i).second->removeDeadConstantUsers();
    }
  }
  {
    const iterator end = m_promoted.end();
    for(iterator i = m_promoted.begin(); i != end; ++i) {
      (*i)->removeDeadConstantUsers();
    }
  }

  m_original_types.clear();
  m_promoted_types.clear();
  m_promoted.clear();
  m_promoted_consts.clear();
  return true;
}

ModulePass *llvm::createPromoteSimpleStructsPass() {
  return new PromoteSimpleStructs();
}
