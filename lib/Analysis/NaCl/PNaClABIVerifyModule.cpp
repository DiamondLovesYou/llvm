//===- PNaClABIVerifyModule.cpp - Verify PNaCl ABI rules --------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// Verify module-level PNaCl ABI requirements (specifically those that do not
// require looking at the function bodies)
//
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/Twine.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Module.h"
#include "llvm/Pass.h"

#include "llvm/Support/raw_ostream.h"
#include "llvm/Analysis/NaCl.h"

#include "CheckTypes.h"
using namespace llvm;

namespace {
// This pass should not touch function bodies, to stay streaming-friendly
class PNaClABIVerifyModule : public ModulePass {
 public:
  static char ID;
  PNaClABIVerifyModule();
  bool runOnModule(Module &M);
  virtual void print(llvm::raw_ostream &O, const Module *M) const;
 private:
  TypeChecker TC;
  std::string ErrorsString;
  llvm::raw_string_ostream Errors;
};

static const char *linkageName(GlobalValue::LinkageTypes LT) {
  // This logic is taken from PrintLinkage in lib/VMCore/AsmWriter.cpp
  switch (LT) {
    case GlobalValue::ExternalLinkage: return "external";
    case GlobalValue::PrivateLinkage:       return "private ";
    case GlobalValue::LinkerPrivateLinkage: return "linker_private ";
    case GlobalValue::LinkerPrivateWeakLinkage: return "linker_private_weak ";
    case GlobalValue::InternalLinkage:      return "internal ";
    case GlobalValue::LinkOnceAnyLinkage:   return "linkonce ";
    case GlobalValue::LinkOnceODRLinkage:   return "linkonce_odr ";
    case GlobalValue::LinkOnceODRAutoHideLinkage:
      return "linkonce_odr_auto_hide ";
    case GlobalValue::WeakAnyLinkage:       return "weak ";
    case GlobalValue::WeakODRLinkage:       return "weak_odr ";
    case GlobalValue::CommonLinkage:        return "common ";
    case GlobalValue::AppendingLinkage:     return "appending ";
    case GlobalValue::DLLImportLinkage:     return "dllimport ";
    case GlobalValue::DLLExportLinkage:     return "dllexport ";
    case GlobalValue::ExternalWeakLinkage:  return "extern_weak ";
    case GlobalValue::AvailableExternallyLinkage:
      return "available_externally ";
    default:
      return "unknown";
  }
}

} // end anonymous namespace

PNaClABIVerifyModule::PNaClABIVerifyModule() : ModulePass(ID),
                                               Errors(ErrorsString) {}

bool PNaClABIVerifyModule::runOnModule(Module &M) {
  for (Module::const_global_iterator MI = M.global_begin(), ME = M.global_end();
       MI != ME; ++MI) {
    // Check types of global variables and their initializers
    if (!TC.isValidType(MI->getType())) {
      // GVs are pointers, so print the pointed-to type for clarity
      Errors << "Variable " + MI->getName() +
          " has disallowed type: " +
          TypeChecker::getTypeName(MI->getType()->getContainedType(0)) + "\n";
    } else if (MI->hasInitializer()) {
      // If the type of the global is bad, no point in checking its initializer
      Type *T = TC.checkTypesInValue(MI->getInitializer());
      if (T)
        Errors << "Initializer for " + MI->getName() +
            " has disallowed type: " +
            TypeChecker::getTypeName(T) + "\n";
    }

    // Check GV linkage types
    switch (MI->getLinkage()) {
      case GlobalValue::ExternalLinkage:
      case GlobalValue::AvailableExternallyLinkage:
      case GlobalValue::InternalLinkage:
      case GlobalValue::PrivateLinkage:
        break;
      default:
        Errors << "Variable " + MI->getName() +
            " has disallowed linkage type: " +
            linkageName(MI->getLinkage()) + "\n";
    }
  }
  // No aliases allowed for now.
  for (Module::alias_iterator MI = M.alias_begin(),
           E = M.alias_end(); MI != E; ++MI)
    Errors << "Variable " + MI->getName() + " is an alias (disallowed)\n";

  Errors.flush();
  return false;
}

void PNaClABIVerifyModule::print(llvm::raw_ostream &O, const Module *M) const {
  O << ErrorsString;
}

char PNaClABIVerifyModule::ID = 0;

static RegisterPass<PNaClABIVerifyModule> X("verify-pnaclabi-module",
    "Verify module for PNaCl", false, false);

ModulePass *llvm::createPNaClABIVerifyModulePass() {
  return new PNaClABIVerifyModule();
}
