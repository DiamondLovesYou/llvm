//=== llvm/Support/Unix/ThreadLocal.inc - Unix Thread Local Data -*- C++ -*-===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the Unix specific (non-pthread) ThreadLocal class.
//
//===----------------------------------------------------------------------===//

//===----------------------------------------------------------------------===//
//=== WARNING: Implementation here must contain only generic UNIX code that
//===          is guaranteed to work on *all* UNIX variants.
//===----------------------------------------------------------------------===//

namespace llvm {
using namespace sys;
ThreadLocalImpl::ThreadLocalImpl() : data() { }
ThreadLocalImpl::~ThreadLocalImpl() { }
void ThreadLocalImpl::setInstance(const void* d) { align_data = reinterpret_cast<ThreadLocalDataTy>(d);}
const void* ThreadLocalImpl::getInstance() { return reinterpret_cast<const void*>(align_data); }
void ThreadLocalImpl::removeInstance() { setInstance(0); }
}
