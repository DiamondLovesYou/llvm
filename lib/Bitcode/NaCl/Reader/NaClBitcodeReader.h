//===- NaClBitcodeReader.h ------------------------------------*- C++ -*-===//
//     Internal NaClBitcodeReader implementation
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This header defines the NaClBitcodeReader class.
//
//===----------------------------------------------------------------------===//

#ifndef NACL_BITCODE_READER_H
#define NACL_BITCODE_READER_H

#include "llvm/ADT/DenseMap.h"
#include "llvm/Bitcode/NaCl/NaClBitcodeHeader.h"
#include "llvm/Bitcode/NaCl/NaClBitstreamReader.h"
#include "llvm/Bitcode/NaCl/NaClLLVMBitCodes.h"
#include "llvm/GVMaterializer.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/OperandTraits.h"
#include "llvm/IR/Type.h"
#include "llvm/Support/ValueHandle.h"
#include <vector>

namespace llvm {
  class MemoryBuffer;
  class LLVMContext;

//===----------------------------------------------------------------------===//
//                          NaClBitcodeReaderValueList Class
//===----------------------------------------------------------------------===//

class NaClBitcodeReaderValueList {
  std::vector<WeakVH> ValuePtrs;

  /// ResolveConstants - As we resolve forward-referenced constants, we add
  /// information about them to this vector.  This allows us to resolve them in
  /// bulk instead of resolving each reference at a time.  See the code in
  /// ResolveConstantForwardRefs for more information about this.
  ///
  /// The key of this vector is the placeholder constant, the value is the slot
  /// number that holds the resolved value.
  typedef std::vector<std::pair<Constant*, unsigned> > ResolveConstantsTy;
  ResolveConstantsTy ResolveConstants;
  LLVMContext &Context;
public:
  NaClBitcodeReaderValueList(LLVMContext &C) : Context(C) {}
  ~NaClBitcodeReaderValueList() {
    assert(ResolveConstants.empty() && "Constants not resolved?");
  }

  // vector compatibility methods
  unsigned size() const { return ValuePtrs.size(); }
  void resize(unsigned N) { ValuePtrs.resize(N); }
  void push_back(Value *V) {
    ValuePtrs.push_back(V);
  }

  void clear() {
    assert(ResolveConstants.empty() && "Constants not resolved?");
    ValuePtrs.clear();
  }

  Value *operator[](unsigned i) const {
    assert(i < ValuePtrs.size());
    return ValuePtrs[i];
  }

  Value *back() const { return ValuePtrs.back(); }
    void pop_back() { ValuePtrs.pop_back(); }
  bool empty() const { return ValuePtrs.empty(); }
  void shrinkTo(unsigned N) {
    assert(N <= size() && "Invalid shrinkTo request!");
    ValuePtrs.resize(N);
  }

  // Declares the type of the forward-referenced value Idx.  Returns
  // true if an error occurred.  It is an error if Idx's type has
  // already been declared.
  bool createValueFwdRef(unsigned Idx, Type *Ty);

  // Declares the type of the forward-referenced constant Idx. Returns
  // 0 if an error occurred.
  // TODO(kschimpf) Convert these to be like createValueFwdRef and
  // getValueFwdRef.
  Constant *getConstantFwdRef(unsigned Idx, Type *Ty);

  // Gets the forward reference value for Idx.
  Value *getValueFwdRef(unsigned Idx);

  // Gets the corresponding constant defining the address of the
  // corresponding global variable defined by Idx, if already defined.
  // Otherwise, creates a forward reference for Idx, and returns the
  // placeholder constant for the address of the corresponding global
  // variable defined by Idx.
  Constant *getOrCreateGlobalVarRef(unsigned Idx, Module* M);

  // Assigns Idx to the given value (if new), or assigns V to Idx (if Idx
  // was forward referenced).
  void AssignValue(Value *V, unsigned Idx);

  // Assigns Idx to the given global variable. If the Idx currently has
  // a forward reference (built by createGlobalVarFwdRef(unsigned Idx)),
  // replaces uses of the global variable forward reference with the
  // value GV.
  void AssignGlobalVar(GlobalVariable *GV, unsigned Idx);

  /// ResolveConstantForwardRefs - Once all constants are read, this method bulk
  /// resolves any forward references.
  void ResolveConstantForwardRefs();
};


class NaClBitcodeReader : public GVMaterializer {
  NaClBitcodeHeader Header;  // Header fields of the PNaCl bitcode file.
  LLVMContext &Context;
  Module *TheModule;
  MemoryBuffer *Buffer;
  bool BufferOwned;
  OwningPtr<NaClBitstreamReader> StreamFile;
  NaClBitstreamCursor Stream;
  DataStreamer *LazyStreamer;
  uint64_t NextUnreadBit;
  bool SeenValueSymbolTable;

  std::string ErrorString;

  std::vector<Type*> TypeList;
  NaClBitcodeReaderValueList ValueList;
  SmallVector<SmallVector<uint64_t, 64>, 64> UseListRecords;

  /// FunctionBBs - While parsing a function body, this is a list of the basic
  /// blocks for the function.
  std::vector<BasicBlock*> FunctionBBs;

  // When reading the module header, this list is populated with functions that
  // have bodies later in the file.
  std::vector<Function*> FunctionsWithBodies;

  // When intrinsic functions are encountered which require upgrading they are
  // stored here with their replacement function.
  typedef std::vector<std::pair<Function*, Function*> > UpgradedIntrinsicMap;
  UpgradedIntrinsicMap UpgradedIntrinsics;

  // Several operations happen after the module header has been read, but
  // before function bodies are processed. This keeps track of whether
  // we've done this yet.
  bool SeenFirstFunctionBody;

  /// DeferredFunctionInfo - When function bodies are initially scanned, this
  /// map contains info about where to find deferred function body in the
  /// stream.
  DenseMap<Function*, uint64_t> DeferredFunctionInfo;

  /// UseRelativeIDs - Indicates that we are using a new encoding for
  /// instruction operands where most operands in the current
  /// FUNCTION_BLOCK are encoded relative to the instruction number,
  /// for a more compact encoding.  Some instruction operands are not
  /// relative to the instruction ID: basic block numbers, and types.
  /// Once the old style function blocks have been phased out, we would
  /// not need this flag.
  bool UseRelativeIDs;

  /// \brief True if we should only accept supported bitcode format.
  bool AcceptSupportedBitcodeOnly;

  /// \brief Integer type use for PNaCl conversion of pointers.
  Type *IntPtrType;

public:
  explicit NaClBitcodeReader(MemoryBuffer *buffer, LLVMContext &C,
                             bool AcceptSupportedOnly = true)
    : Context(C), TheModule(0), Buffer(buffer), BufferOwned(false),
      LazyStreamer(0), NextUnreadBit(0), SeenValueSymbolTable(false),
      ValueList(C),
      SeenFirstFunctionBody(false), UseRelativeIDs(false),
      AcceptSupportedBitcodeOnly(AcceptSupportedOnly),
      IntPtrType(IntegerType::get(C, PNaClIntPtrTypeBitSize)) {
  }
  explicit NaClBitcodeReader(DataStreamer *streamer, LLVMContext &C,
                             bool AcceptSupportedOnly = true)
    : Context(C), TheModule(0), Buffer(0), BufferOwned(false),
      LazyStreamer(streamer), NextUnreadBit(0), SeenValueSymbolTable(false),
      ValueList(C),
      SeenFirstFunctionBody(false), UseRelativeIDs(false),
      AcceptSupportedBitcodeOnly(AcceptSupportedOnly),
      IntPtrType(IntegerType::get(C, PNaClIntPtrTypeBitSize)) {
  }
  ~NaClBitcodeReader() {
    FreeState();
  }

  void FreeState();

  /// setBufferOwned - If this is true, the reader will destroy the MemoryBuffer
  /// when the reader is destroyed.
  void setBufferOwned(bool Owned) { BufferOwned = Owned; }

  virtual bool isMaterializable(const GlobalValue *GV) const;
  virtual bool isDematerializable(const GlobalValue *GV) const;
  virtual bool Materialize(GlobalValue *GV, std::string *ErrInfo = 0);
  virtual bool MaterializeModule(Module *M, std::string *ErrInfo = 0);
  virtual void Dematerialize(GlobalValue *GV);

  bool Error(const std::string &Str) {
    ErrorString = Str;
    return true;
  }
  const std::string &getErrorString() const { return ErrorString; }

  /// @brief Main interface to parsing a bitcode buffer.
  /// @returns true if an error occurred.
  bool ParseBitcodeInto(Module *M);

private:
  // Returns false if Header is acceptable.
  bool AcceptHeader() const {
    return !(Header.IsSupported() ||
             (!AcceptSupportedBitcodeOnly && Header.IsReadable()));
  }
  uint32_t GetPNaClVersion() const {
    return Header.GetPNaClVersion();
  }
  Type *getTypeByID(unsigned ID);
  // Returns the value associated with ID. The value must already exist,
  // or a forward referenced value created by getOrCreateFnVaueByID.
  Value *getFnValueByID(unsigned ID) {
    return ValueList.getValueFwdRef(ID);
  }
  BasicBlock *getBasicBlock(unsigned ID) const {
    if (ID >= FunctionBBs.size()) return 0; // Invalid ID
    return FunctionBBs[ID];
  }

  /// \brief Read a value out of the specified record from slot '*Slot'.
  /// Increment *Slot past the number of slots used by the value in the record.
  /// Return true if there is an error.
  bool popValue(const SmallVector<uint64_t, 64> &Record, unsigned *Slot,
                unsigned InstNum, Value **ResVal) {
    if (*Slot == Record.size()) return true;
    unsigned ValNo = (unsigned)Record[(*Slot)++];
    // Adjust the ValNo, if it was encoded relative to the InstNum.
    if (UseRelativeIDs)
      ValNo = InstNum - ValNo;
    *ResVal = getFnValueByID(ValNo);
    return *ResVal == 0;
  }

  /// getValue -- Version of getValue that returns ResVal directly,
  /// or 0 if there is an error.
  Value *getValue(const SmallVector<uint64_t, 64> &Record, unsigned Slot,
                  unsigned InstNum) {
    if (Slot == Record.size()) return 0;
    unsigned ValNo = (unsigned)Record[Slot];
    // Adjust the ValNo, if it was encoded relative to the InstNum.
    if (UseRelativeIDs)
      ValNo = InstNum - ValNo;
    return getFnValueByID(ValNo);
  }

  /// getValueSigned -- Like getValue, but decodes signed VBRs.
  Value *getValueSigned(const SmallVector<uint64_t, 64> &Record, unsigned Slot,
                        unsigned InstNum) {
    if (Slot == Record.size()) return 0;
    unsigned ValNo = (unsigned) NaClDecodeSignRotatedValue(Record[Slot]);
    // Adjust the ValNo, if it was encoded relative to the InstNum.
    if (UseRelativeIDs)
      ValNo = InstNum - ValNo;
    return getFnValueByID(ValNo);
  }

  /// \brief Add instructions to cast Op to the given type T into block BB.
  /// Follows rules for pointer conversion as defined in
  /// llvm/lib/Transforms/NaCl/ReplacePtrsWithInts.cpp.
  ///
  /// Returns 0 if unable to generate conversion value (also generates
  /// an appropriate error message and calls Error).
  Value *ConvertOpToType(Value *Op, Type *T, BasicBlock *BB);

  /// \brief If Op is a scalar value, this is a nop. If Op is a
  /// pointer value, a PtrToInt instruction is inserted (in BB) to
  /// convert Op to an integer.
  Value *ConvertOpToScalar(Value *Op, BasicBlock *BB);

  /// \brief Returns the corresponding, PNaCl non-pointer equivalent
  /// for the given type.
  Type *ConvertTypeToScalarType(Type *T);

  /// \brief Install instruction I into basic block BB.
  bool InstallInstruction(BasicBlock *BB, Instruction *I);

  bool ParseModule(bool Resume);
  bool ParseTypeTable();
  bool ParseTypeTableBody();
  bool ParseGlobalVars();
  bool ParseValueSymbolTable();
  bool ParseConstants();
  bool RememberAndSkipFunctionBody();
  bool ParseFunctionBody(Function *F);
  bool GlobalCleanup();
  bool ParseUseLists();
  bool InitStream();
  bool InitStreamFromBuffer();
  bool InitLazyStream();
  bool FindFunctionInStream(Function *F,
         DenseMap<Function*, uint64_t>::iterator DeferredFunctionInfoIterator);
};

} // End llvm namespace

#endif
