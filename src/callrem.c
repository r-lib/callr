/*
 * callrem: a launcher binary that embeds R.
 *
 * For now this is a transparent passthrough: it links libR, runs the
 * standard R main loop with the argv handed to it by callr, and exits.
 * Output must remain byte-identical to running R directly so that
 * existing callr tests pass unchanged.
 *
 * We use Rf_initialize_R / setup_Rmainloop / run_Rmainloop (the same
 * sequence R's own R.bin uses) rather than Rf_initEmbeddedR, because
 * the latter forces R_Interactive = TRUE and would break the `-f file`
 * non-interactive script mode that callr relies on.
 *
 * Future iterations can install custom WriteConsoleEx callbacks
 * (see #243) without touching callr's R-side glue.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <Rembedded.h>

#ifdef _WIN32

#define Win32
#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>
#include <Rversion.h>
#define LibExtern __declspec(dllimport) extern
#include <Rinternals.h>
#include <R_ext/RStartup.h>

extern void cmdlineoptions(int, char **);

int main(int argc, char **argv) {
  /*
   * Rterm.exe in R's own front-ends uses cmdlineoptions() to parse
   * argv and set up the structRstart, then setup_Rmainloop() and
   * run_Rmainloop(). We mimic that so behavior matches Rterm.
   */
  cmdlineoptions(argc, argv);
  setup_Rmainloop();
  run_Rmainloop();
  Rf_endEmbeddedR(0);
  return 0;
}

#else

int main(int argc, char **argv) {
  Rf_initialize_R(argc, argv);
  setup_Rmainloop();
  run_Rmainloop();
  Rf_endEmbeddedR(0);
  return 0;
}

#endif
