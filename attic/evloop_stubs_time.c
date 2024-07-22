/*---------------------------------------------------------------------------
   Copyright (c) 2022 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

#include "evloop_stubs.h"
#include <stdint.h>

/* Darwin */

#if defined(OCAML_EVLOOP_DARWIN)

#include <mach/mach_time.h>

static mach_timebase_info_data_t scale = {0};
static void _ocaml_evloop_clock_init_scale (void)
{
  if (mach_timebase_info (&scale) != KERN_SUCCESS)
    OCAML_EVLOOP_RAISE_SYS_ERROR ("mach_timebase_info () failed");

  if (scale.denom == 0)
    OCAML_EVLOOP_RAISE_SYS_ERROR ("mach_timebase_info_data.denom is 0");
}

CAMLprim value ocaml_evloop_monotonic_now_ns (value unit)
{
  if (scale.denom == 0) { _ocaml_evloop_clock_init_scale (); }
  uint64_t now = mach_absolute_time ();
  return caml_copy_int64 ((now * scale.numer) / scale.denom);
}

/* POSIX */

#elif defined(OCAML_EVLOOP_POSIX)

#include <time.h>

CAMLprim value ocaml_evloop_monotonic_now_ns (value unit)
{
  struct timespec now;

  if (clock_gettime (CLOCK_MONOTONIC, &now))
    OCAML_EVLOOP_RAISE_SYS_ERROR ("clock_gettime () failed");

  return caml_copy_int64 ((uint64_t)(now.tv_sec) *
                          (uint64_t)1000000000 +
                          (uint64_t)(now.tv_nsec));
}

/* Windows */

#elif defined(OCAML_EVLOOP_WINDOWS)

#include <windows.h>

static double freq = 0;
static void _ocaml_evloop_clock_init_freq (void)
{
  LARGE_INTEGER f;
  if (!QueryPerformanceFrequency(&f))
    OCAML_EVLOOP_RAISE_SYS_ERROR ("QueryPerformanceFrequency () failed");
  freq = (1000000000.0 / f.QuadPart);
}

CAMLprim value ocaml_evloop_monotonic_now_ns (value unit)
{
  static LARGE_INTEGER now;
  if (freq == 0) _ocaml_evloop_clock_init_freq ();
  if (!QueryPerformanceCounter(&now))
    OCAML_EVLOOP_RAISE_SYS_ERROR ("QueryPerformanceCounter () failed");
  return caml_copy_int64 ((uint64_t)(now.QuadPart * freq));
}

/* Unsupported */

#else

#warning OCaml Nandi library: unsupported platform, monotonic timings will be wrong

CAMLprim value ocaml_evloop_monotonic_now_ns (value unit)
{
  return caml_copy_int64 ((uint64_t)0);
}

#endif
