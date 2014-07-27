//===---- IRReader.cpp - Reader for LLVM IR files -------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "llvm/IRReader/IRReader.h"
#include "llvm-c/Core.h"
#include "llvm-c/IRReader.h"
#include "llvm/AsmParser/Parser.h"
#include "llvm/Bitcode/ReaderWriter.h"
#include "llvm/Bitcode/NaCl/NaClReaderWriter.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/Timer.h"
#include "llvm/Support/raw_ostream.h"
#include <system_error>

using namespace llvm;

namespace llvm {
  extern bool TimePassesIsEnabled;
}

static const char *const TimeIRParsingGroupName = "LLVM IR Parsing";
static const char *const TimeIRParsingName = "Parse IR";


static Module *getLazyIRModule(MemoryBuffer *Buffer, SMDiagnostic &Err,
                               LLVMContext &Context, FileFormat Format = AutoDetectFormat) {
  FileFormat RealFormat;
  bool Assembly = false;
  if (Format == AutoDetectFormat) {
    if (isBitcode(Buffer)) {
      RealFormat = LLVMFormat;
    } else if (isNaClBitcode(Buffer)) {
      RealFormat = PNaClFormat;
    } else {
      RealFormat = LLVMFormat;
      Assembly = true;
    }
  } else {
    RealFormat = Format;
    Assembly = !isBitcode(Buffer);
  }

  if (RealFormat == LLVMFormat) {
    if (!Assembly) {
      std::string ErrMsg;
      ErrorOr<Module *> ModuleOrErr = getLazyBitcodeModule(Buffer, Context);
      if (std::error_code EC = ModuleOrErr.getError()) {
        Err = SMDiagnostic(Buffer->getBufferIdentifier(), SourceMgr::DK_Error,
                           EC.message());
        // getLazyBitcodeModule does not take ownership of the Buffer in the
        // case of an error.
        delete Buffer;
        return nullptr;
      }
      return ModuleOrErr.get();
    }

    return ParseAssembly(Buffer, nullptr, Err, Context);
  } else if (RealFormat == PNaClFormat &&
             (Format == AutoDetectFormat || isNaClBitcode(Buffer))) {
    std::string ErrMsg;
    Module *M = getNaClLazyBitcodeModule(Buffer, Context, &ErrMsg);
    if (M == 0)
      Err = SMDiagnostic(Buffer->getBufferIdentifier(), SourceMgr::DK_Error,
                         ErrMsg);
    // NaClParseBitcodeFile does not take ownership of the Buffer.
    delete Buffer;
    return M;
  } else {
    Err = SMDiagnostic(Buffer->getBufferIdentifier(), SourceMgr::DK_Error,
                       "Did not specify correct format for file");
    return 0;
  }
}

Module *llvm::getLazyIRFileModule(const std::string &Filename, SMDiagnostic &Err,
                                  LLVMContext &Context, FileFormat Format) {
  ErrorOr<std::unique_ptr<MemoryBuffer>> FileOrErr =
      MemoryBuffer::getFileOrSTDIN(Filename);
  if (std::error_code EC = FileOrErr.getError()) {
    Err = SMDiagnostic(Filename, SourceMgr::DK_Error,
                       "Could not open input file: " + EC.message());
    return nullptr;
  }

  return getLazyIRModule(FileOrErr.get().release(), Err, Context, Format);
}

Module *llvm::ParseIR(MemoryBuffer *Buffer, SMDiagnostic &Err,
                      LLVMContext &Context, FileFormat Format) {
  NamedRegionTimer T(TimeIRParsingName, TimeIRParsingGroupName,
                     TimePassesIsEnabled);

  FileFormat RealFormat;
  bool Assembly = false;
  if (Format == AutoDetectFormat) {
    if (isBitcode(Buffer)) {
      RealFormat = LLVMFormat;
    } else if (isNaClBitcode(Buffer)) {
      RealFormat = PNaClFormat;
    } else {
      RealFormat = LLVMFormat;
      Assembly = true;
    }
  } else {
    RealFormat = Format;
    Assembly = !isBitcode(Buffer);
  }

  if (RealFormat == LLVMFormat) {
    if (!Assembly) {
      ErrorOr<Module *> ModuleOrErr = parseBitcodeFile(Buffer, Context);
      Module *M = nullptr;
      if (std::error_code EC = ModuleOrErr.getError())
        Err = SMDiagnostic(Buffer->getBufferIdentifier(), SourceMgr::DK_Error,
                           EC.message());
      else
        M = ModuleOrErr.get();
      // parseBitcodeFile does not take ownership of the Buffer.
      return M;
    }

    return ParseAssembly(MemoryBuffer::getMemBuffer(
                         Buffer->getBuffer(), Buffer->getBufferIdentifier()),
                         nullptr, Err, Context);
  } else if (RealFormat == PNaClFormat &&
             (Format == AutoDetectFormat || isNaClBitcode(Buffer))) {
    std::string ErrMsg;
    Module *M = NaClParseBitcodeFile(Buffer, Context, &ErrMsg);
    if (M == 0)
      Err = SMDiagnostic(Buffer->getBufferIdentifier(), SourceMgr::DK_Error,
                         ErrMsg);
    // NaClParseBitcodeFile does not take ownership of the Buffer.
    delete Buffer;
    return M;
  } else {
    Err = SMDiagnostic(Buffer->getBufferIdentifier(), SourceMgr::DK_Error,
                       "Did not specify correct format for file");
    return 0;
  }
}

Module *llvm::ParseIRFile(const std::string &Filename, SMDiagnostic &Err,
                          LLVMContext &Context, FileFormat Format) {
  ErrorOr<std::unique_ptr<MemoryBuffer>> FileOrErr =
      MemoryBuffer::getFileOrSTDIN(Filename);
  if (std::error_code EC = FileOrErr.getError()) {
    Err = SMDiagnostic(Filename, SourceMgr::DK_Error,
                       "Could not open input file: " + EC.message());
    return nullptr;
  }

  return ParseIR(FileOrErr.get().get(), Err, Context, Format);
}

/// isBitcode - Return true if the given bytes are the magic bytes for
/// LLVM IR bitcode, either with or without a wrapper. Does not take ownership
/// of Buffer. Placed here so tools don't need to depend on extra components.
bool llvm::isBitcode(const MemoryBuffer *Buffer) {
  return isBitcodeWrapper((const unsigned char *)Buffer->getBufferStart(),
                          (const unsigned char *)Buffer->getBufferEnd()) ||
    isRawBitcode((const unsigned char *)Buffer->getBufferStart(),
                 (const unsigned char *)Buffer->getBufferEnd());
}
/// isNaClBitcode - Return true if the given bytes are the magic bytes for
/// PNaCl bitcode wire format. Does not take ownership of Buffer. Placed here so
/// tools don't need to depend on extra components.
bool llvm::isNaClBitcode(const MemoryBuffer *Buffer) {
  return isNaClBitcode((const unsigned char *)Buffer->getBufferStart(),
                       (const unsigned char *)Buffer->getBufferEnd());
}


//===----------------------------------------------------------------------===//
// C API.
//===----------------------------------------------------------------------===//

LLVMBool LLVMParseIRInContext(LLVMContextRef ContextRef,
                              LLVMMemoryBufferRef MemBuf, LLVMModuleRef *OutM,
                              char **OutMessage) {
  SMDiagnostic Diag;

  std::unique_ptr<MemoryBuffer> MB(unwrap(MemBuf));
  *OutM = wrap(ParseIR(MB.get(), Diag, *unwrap(ContextRef)));

  if(!*OutM) {
    if (OutMessage) {
      std::string buf;
      raw_string_ostream os(buf);

      Diag.print(nullptr, os, false);
      os.flush();

      *OutMessage = strdup(buf.c_str());
    }
    return 1;
  }

  return 0;
}
