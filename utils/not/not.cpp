//===- not.cpp - The 'not' testing tool -----------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//

#include "llvm/Support/Path.h"
#include "llvm/Support/Program.h"
#include "llvm/Support/raw_ostream.h"
using namespace llvm;

// @LOCALMOD-BEGIN
#if defined(__native_client__)
int main(int argc, char *argv_alt[]) {
  const char **argv = const_cast<const char **>(argv_alt);
#else
int main(int argc, const char **argv) {
#endif
// @LOCALMOD-END
  bool ExpectCrash = false;

  ++argv;
  --argc;

  if (argc > 0 && StringRef(argv[0]) == "--crash") {
    ++argv;
    --argc;
    ExpectCrash = true;
  }

  if (argc == 0)
    return 1;

  std::string Program = sys::FindProgramByName(argv[0]);

  std::string ErrMsg;
  int Result = sys::ExecuteAndWait(Program, argv, nullptr, nullptr, 0, 0,
                                   &ErrMsg);
#ifdef _WIN32
  // Handle abort() in msvcrt -- It has exit code as 3.  abort(), aka
  // unreachable, should be recognized as a crash.  However, some binaries use
  // exit code 3 on non-crash failure paths, so only do this if we expect a
  // crash.
  if (ExpectCrash && Result == 3)
    Result = -3;
#endif
  if (Result < 0) {
    errs() << "Error: " << ErrMsg << "\n";
    if (ExpectCrash)
      return 0;
    return 1;
  }

  if (ExpectCrash)
    return 1;

  return Result == 0;
}
