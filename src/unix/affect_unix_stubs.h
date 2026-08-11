/*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#define OCAML_AFFECT_RAISE_SYS_ERROR(ERR)                             \
  do { caml_raise_sys_error (caml_copy_string("affect stubs: " ERR)); } \
  while (0)

/* Detect platform */

#if defined(__APPLE__) && defined(__MACH__)
  #define OCAML_AFFECT_DARWIN
#endif

#if defined(__unix__) || defined(__unix)
  #include <unistd.h>
  #if defined(_POSIX_VERSION)
    #define OCAML_AFFECT_POSIX
  #endif
#endif

#if defined(__linux__)
  #define OCAML_AFFECT_LINUX
#endif

#if defined (__CYGWIN__)
  #define OCAML_AFFECT_CYGWIN
#endif

#if defined (_WIN32)
  #define OCAML_AFFECT_WINDOWS
  #define WIN32_LEAN_AND_MEAN
#endif
