/*---------------------------------------------------------------------------
   Copyright (c) 2022 The affect programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*/

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#define OCAML_EVLOOP_RAISE_SYS_ERROR(ERR)                               \
  do { caml_raise_sys_error (caml_copy_string("Evloop stubs: " ERR)); } \
  while (0)

/* Detect platform */

#if defined(__APPLE__) && defined(__MACH__)
  #define OCAML_EVLOOP_DARWIN

#elif defined(__unix__) || defined(__unix)
 #include <unistd.h>
 #if defined(_POSIX_VERSION)
   #define OCAML_EVLOOP_POSIX
 #endif

#elif defined (_WIN32)
#define OCAML_EVLOOP_WINDOWS
#define WIN32_LEAN_AND_MEAN

#endif

/*---------------------------------------------------------------------------
   Copyright (c) 2022 The affect programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*/
