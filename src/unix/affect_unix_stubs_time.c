/*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*/

#include "affect_unix_stubs.h"
#include <stdint.h>

#define OCAML_AFFECT_DAY_MAX 2932896 // See Ptime.max

// MacOS

#if defined(OCAML_AFFECT_DARWIN)

#include <mach/mach_time.h>
#include <time.h>
#include <sys/time.h>

#include <AvailabilityMacros.h>

#if MACOSX_VERSION_MIN_REQUIRED >= 101200
  #define ocaml_darwin_mach_time mach_continuous_time
#else
  #define ocaml_darwin_mach_time mach_absolute_time
#endif

static mach_timebase_info_data_t scale = {0};
static void _ocaml_affect_mtime_clock_init_scale (void)
{
  if (mach_timebase_info (&scale) != KERN_SUCCESS)
    OCAML_AFFECT_RAISE_SYS_ERROR ("mach_timebase_info () failed");

  if (scale.denom == 0)
    OCAML_AFFECT_RAISE_SYS_ERROR ("mach_timebase_info_data.denom is 0");
}

CAMLprim value ocaml_affect_mtime_now_ns (value unit)
{
  if (scale.denom == 0) { _ocaml_affect_mtime_clock_init_scale (); }
  uint64_t now = ocaml_darwin_mach_time ();
  return caml_copy_int64 ((now * scale.numer) / scale.denom);
}

CAMLprim value ocaml_affect_ptime_now_d_ps (value unit)
{
  CAMLparam1 (unit);
  CAMLlocal1 (pair);
  struct timeval now;

  gettimeofday(&now, NULL);

  /* Make sure to return valid Ptime.t values. */

  /* We only handle reasonable timevals (not specified in POSIX it seems) */
  if (now.tv_usec < 0 || now.tv_usec > 999999)
    OCAML_AFFECT_RAISE_SYS_ERROR ("unreasonable tv_usec in timeval");

  /* To make it easier, we do not lie, this can't possibly be now.
     See e.g. Ptime.Span.of_int_s if this is a problem. */
  if (now.tv_sec < 0)
    OCAML_AFFECT_RAISE_SYS_ERROR ("negative tv_sec in timeval");

  int d = now.tv_sec / 86400;
  if (d > OCAML_AFFECT_DAY_MAX)
    OCAML_AFFECT_RAISE_SYS_ERROR ("can't represent timeval in Ptime.t");

  pair = caml_alloc (2, 0);
  Store_field (pair, 0, Val_int (d));
  Store_field (pair, 1,
               /* Given the above checks, in the right range for Ptime */
               caml_copy_int64 ((now.tv_sec % 86400) * 1000000000000L +
                                (now.tv_usec * 1000000L)));
  CAMLreturn (pair);
}

// POSIX and Linux

#elif defined(OCAML_AFFECT_POSIX)

#include <unistd.h>
#include <time.h>

#if defined(OCAML_AFFECT_LINUX)
  #define ocaml_clockid CLOCK_BOOTTIME
#else
  #define ocaml_clockid CLOCK_MONOTONIC
#endif

CAMLprim value ocaml_affect_mtime_now_ns (value unit)
{
  struct timespec now;
  if (clock_gettime (ocaml_clockid, &now))
    OCAML_AFFECT_RAISE_SYS_ERROR ("clock_gettime () failed");

  return caml_copy_int64 ((uint64_t)(now.tv_sec) *
                          (uint64_t)1000000000 +
                          (uint64_t)(now.tv_nsec));
}

CAMLprim value ocaml_affect_ptime_now_d_ps (value unit)
{
  CAMLparam1 (unit);
  CAMLlocal1 (pair);
  struct timespec now;

  if (clock_gettime (CLOCK_REALTIME, &now))
    OCAML_AFFECT_RAISE_SYS_ERROR ("can't determine current time");

  /* Make sure to return valid Ptime.t values. */

  /* We only handle valid timespec structs as per POSIX def (§2.8.5 in 2013) */
  if (now.tv_nsec < 0 || now.tv_nsec > 999999999)
    OCAML_AFFECT_RAISE_SYS_ERROR ("invalid tv_nsec in timespec");

  /* To make it easier, we do not lie, this can't possibly be now.
     See e.g. Ptime.Span.of_int_s if this is a problem. */
  if (now.tv_sec < 0)
    OCAML_AFFECT_RAISE_SYS_ERROR ("negative tv_sec in timespec");

  int d = now.tv_sec / 86400;
  if (d > OCAML_AFFECT_DAY_MAX)
    OCAML_AFFECT_RAISE_SYS_ERROR ("can't represent timespec in Ptime.t");

  pair = caml_alloc (2, 0);
  Store_field (pair, 0, Val_int (d));
  Store_field (pair, 1,
               /* Given the above checks, in the right range for Ptime */
               caml_copy_int64 ((now.tv_sec % 86400) * 1000000000000L +
                                (now.tv_nsec * 1000L)));
  CAMLreturn (pair);
}

// Windows

#elif defined(OCAML_AFFECT_WINDOWS)

#include <windows.h>

static double freq = 0;
static void _ocaml_affect_clock_init_freq (void)
{
  LARGE_INTEGER f;
  if (!QueryPerformanceFrequency(&f))
    OCAML_AFFECT_RAISE_SYS_ERROR ("QueryPerformanceFrequency () failed");
  freq = (1000000000.0 / f.QuadPart);
}

CAMLprim value ocaml_affect_mtime_now_ns (value unit)
{
  static LARGE_INTEGER now;
  if (freq == 0) _ocaml_affect_clock_init_freq ();
  if (!QueryPerformanceCounter(&now))
    OCAML_AFFECT_RAISE_SYS_ERROR ("QueryPerformanceCounter () failed");
  return caml_copy_int64 ((uint64_t)(now.QuadPart * freq));
}

CAMLprim value ocaml_affect_ptime_now_d_ps (value unit)
{
  CAMLparam1 (unit);
  CAMLlocal1 (pair);
  long sec, usec;
  SYSTEMTIME stime;
  FILETIME ftime;
  ULARGE_INTEGER time;

  GetSystemTime (&stime);
  SystemTimeToFileTime (&stime, &ftime);
  time.LowPart = ftime.dwLowDateTime;
  time.HighPart = ftime.dwHighDateTime;

#define EPOCH (116444736000000000ULL)
  sec = (long)((time.QuadPart - EPOCH) / 10000000L);
#undef EPOCH
  usec = (long)(stime.wMilliseconds * 1000);

  if (usec < 0 || usec > 999999)
    OCAML_AFFECT_RAISE_SYS_ERROR ("unreasonable usec in FILETIME");

  if (sec < 0)
    OCAML_AFFECT_RAISE_SYS_ERROR ("negative sec in FILETIME");

  int d = sec / 86400;
  if (d > OCAML_AFFECT_DAY_MAX)
    OCAML_AFFECT_RAISE_SYS_ERROR ("can't represent FILETIME in Ptime.t");

  pair = caml_alloc (2, 0);
  Store_field (pair, 0, Val_int (d));
  Store_field (pair, 1,
               caml_copy_int64 ((sec % 86400) * 1000000000000L +
                                (usec * 1000000L)));
  CAMLreturn (pair);
}

#else

// Unknown platform

#warning OCaml affect library: unknown platform, monotonic timings and POSIX time will be wrong

CAMLprim value ocaml_affect_mtime_now_ns (value unit)
{ return caml_copy_int64 ((uint64_t)0); }

CAMLprim value ocaml_affect_ptime_now_d_ps (value unit)
{
  value pair = caml_alloc (2, 0);
  Store_field (pair, 0, Val_int (d));
  Store_field (pair, 1, caml_copy_int64 ((uint64_t)0));
  return pair;
}

#endif
