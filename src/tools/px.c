
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#include <stdlib.h>

void usage() {
  fprintf(stderr, "Usage: px [command arg] [command arg] ...\n\n");
  fprintf(stderr, "Commands:   sleep  <seconds>  -- "
	  "sleep for a number os seconds\n");
  fprintf(stderr, "            out    <string>   -- "
	  "print string to stdout\n");
  fprintf(stderr, "            err    <string>   -- "
	  "print string to stderr\n");
  fprintf(stderr, "            outln  <string>   -- "
	  "print string to stdout, add newline\n");
  fprintf(stderr, "            errln  <string>   -- "
	  "print string to stderr, add newline\n");
  fprintf(stderr, "            cat    <filename> -- "
	  "print file to stdout\n");
  fprintf(stderr, "            return <exitcode> -- "
	  "return with exitcode\n");
}

void cat2(int f, const char *s) {
  char buf[8192];
  long n;

  while ((n = read(f, buf, (long) sizeof buf)) > 0) {
    if (write(1, buf, n) != n){
      fprintf(stderr, "write error copying %s", s);
      exit(6);
    }
  }

  if (n < 0) fprintf(stderr, "error reading %s", s);
}

void cat(const char* filename) {
  int f = open(filename, O_RDONLY);

  if (f < 0) {
    fprintf(stderr, "can't open %s", filename);
    exit(6);
  }

  cat2(f, filename);
  close(f);
}

int main(int argc, const char **argv) {

  int num, idx, ret;
  double fnum;

  if (argc == 2 && !strcmp("--help", argv[1])) { usage(); return 0; }

  for (idx = 1; idx < argc; idx++) {
    const char *cmd = argv[idx];

    if (idx + 1 == argc) {
      fprintf(stderr, "Missing argument for '%s'\n", argv[idx]);
      return 5;
    }

    if (!strcmp("sleep", cmd)) {
      ret = sscanf(argv[++idx], "%lf", &fnum);
      if (ret != 1) {
	fprintf(stderr, "Invalid seconds for px sleep: '%s'\n", argv[idx]);
	return 3;
      }
      num = (int) fnum;
      sleep(num);
      fnum = fnum - num;
      if (fnum > 0) usleep(fnum * 1000.0 * 1000.0);

    } else if (!strcmp("out", cmd)) {
      printf("%s", argv[++idx]);
      fflush(stdout);

    } else if (!strcmp("err", cmd)) {
      fprintf(stderr, "%s", argv[++idx]);

    } else if (!strcmp("outln", cmd)) {
      printf("%s\n", argv[++idx]);
      fflush(stdout);

    } else if (!strcmp("errln", cmd)) {
      fprintf(stderr, "%s\n", argv[++idx]);

    } else if (!strcmp("cat", cmd)) {
      cat(argv[++idx]);

    } else if (!strcmp("return", cmd)) {
      ret = sscanf(argv[++idx], "%d", &num);
      if (ret != 1) {
	fprintf(stderr, "Invalid seconds for px return: '%s'\n", argv[idx]);
	return 4;
      }
      return num;

    } else {
      fprintf(stderr, "Unknown px command: '%s'\n", cmd);
      return 2;
    }
  }

  return 0;
}
