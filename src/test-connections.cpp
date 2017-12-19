
#include <testthat.h>

#include "callr.h"

#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>

#ifndef _WIN32
#include <sys/types.h>
#include <sys/stat.h>
#endif

// LCOV_EXCL_START

#ifdef _WIN32
#include <windows.h>

HANDLE open_file(const char *filename) {
  HANDLE handle = CreateFile(
    /* lpFileName =            */ filename,
    /* dwDesiredAccess =       */ GENERIC_READ,
    /* dwShareMode =           */ 0,
    /* lpSecurityAttributes =  */ NULL,
    /* dwCreationDisposition = */ OPEN_EXISTING,
    /* dwFlagsAndAttributes =  */ FILE_FLAG_OVERLAPPED,
    /* hTemplateFile =         */ NULL);

  if (handle == INVALID_HANDLE_VALUE) error("Cannot open temporary test file");

  return handle;
}

HANDLE make_temp_file(char **filename) {
  char *wd = getcwd(NULL, 0);
  char *tmpdir = (char*) malloc(snprintf(NULL, 0, "%s/fixtures", wd) + 1);
  sprintf(tmpdir, "%s/fixtures", wd);
  *filename = R_tmpnam2(0, tmpdir, ".test");
  free(tmpdir);
  free(wd);

  HANDLE h = CreateFile(
    /* lpFileName =            */ *filename,
    /* dwDesiredAccess =       */ GENERIC_WRITE,
    /* dwShareMode =           */ 0,
    /* lpSecurityAttributes =  */ NULL,
    /* dwCreationDisposition = */ CREATE_ALWAYS,
    /* dwFlagsAndAttributes =  */ FILE_ATTRIBUTE_NORMAL,
    /* hTemplateFile =         */ NULL);

  if (h == INVALID_HANDLE_VALUE) error("Cannot create temporary test file");

  return h;
}

HANDLE open_temp_file(char **filename, size_t bytes, const char *pattern) {
  HANDLE h = make_temp_file(filename);
  DWORD abytes = 0;
  const char *default_pattern = "Nem csak a gyemant es arany\n";
  const char *mypattern = pattern ? pattern : default_pattern;
  size_t pattern_size = strlen(mypattern);
  DWORD written;

  for (abytes = 0; abytes < bytes; abytes += pattern_size) {
    BOOL status = WriteFile(
      /* hFile =                  */ h,
      /* lpBuffer =               */ mypattern,
      /* nNumberOfBytesToWrite =  */ pattern_size,
      /* lpNumberOfBytesWritten = */ &written,
      /* lpOverlappedWriteFile =  */ NULL);
    if (!status) error("Cannot write temporary test file");
    abytes += written;
  }

  CloseHandle(h);

  return open_file(*filename);
}

#else

int open_file(const char *filename) {
  int handle = open(filename, O_RDONLY);
  if (handle < 0) error("Cannot open test file");
  return handle;
}

int make_temp_file(char **filename) {
  char *wd = getcwd(NULL, 0);
  char *tmpdir = (char*) malloc(snprintf(NULL, 0, "%s/fixtures", wd) + 1);
  sprintf(tmpdir, "%s/fixtures", wd);
  *filename = R_tmpnam2(0, tmpdir, ".test");
  free(tmpdir);
  free(wd);

  int fd = open(*filename, O_WRONLY | O_CREAT | O_TRUNC, S_IRWXU);
  return fd;
}

int open_temp_file(char **filename, size_t bytes, const char *pattern) {
  int fd = make_temp_file(filename);
  int abytes = 0;
  const char *default_pattern = "Nem csak a gyemant es arany\n";
  const char *mypattern = pattern ? pattern : default_pattern;
  size_t pattern_size = strlen(mypattern);

  for (abytes = 0; abytes < bytes; abytes += pattern_size) {
    (void) write(fd, mypattern, pattern_size);
  }

  close(fd);

  fd = open(*filename, O_RDONLY);
  return fd;
}

#endif

context("Basics") {

  test_that("can create a connection from os handle") {
    callr_file_handle_t handle = open_file("fixtures/simple.txt");
    callr_connection_t *ccon =
      callr_c_connection_create(handle, CALLR_FILE_TYPE_ASYNCFILE, "UTF-8", 0);
    expect_true(ccon != 0);
    callr_c_connection_destroy(ccon);
  }
}

context("Reading characters") {

  test_that("can read characters and set EOF") {
    callr_file_handle_t handle = open_file("fixtures/simple.txt");
    callr_connection_t *ccon =
      callr_c_connection_create(handle, CALLR_FILE_TYPE_ASYNCFILE, "UTF-8", 0);

    expect_false(callr_c_connection_is_eof(ccon));

    callr_pollable_t pollable;
    callr_c_pollable_from_connection(&pollable, ccon);

    char buffer[10];
    callr_c_connection_poll(&pollable, 1, -1);
    size_t ret = callr_c_connection_read_chars(ccon, buffer, 10);
    expect_true(ret == 10);
    expect_true(! strncmp(buffer, "simple tex", 10));

    expect_false(callr_c_connection_is_eof(ccon));

    ret = callr_c_connection_read_chars(ccon, buffer, 10);
    // on windows it might end with \r\n, depending on git settings for EOL
    expect_true(ret >= 7);
    expect_true(ret <= 8);
    if (ret == 7) expect_true(! strncmp(buffer, "t file\n", 7));
    if (ret == 8) expect_true(! strncmp(buffer, "t file\r\n", 8));

    expect_false(callr_c_connection_is_eof(ccon));

    callr_c_connection_poll(&pollable, 1, -1);
    ret = callr_c_connection_read_chars(ccon, buffer, 10);
    expect_true(ret == 0);

    expect_true(callr_c_connection_is_eof(ccon));

    callr_c_connection_destroy(ccon);
  }

  test_that("EOF edge case") {
    callr_file_handle_t handle = open_file("fixtures/simple.txt");
    callr_connection_t *ccon =
      callr_c_connection_create(handle, CALLR_FILE_TYPE_ASYNCFILE, "UTF-8", 0);

    // Read all contents of the file, it is still not EOF
    char buffer[18];
    size_t ret = callr_c_connection_read_chars(ccon, buffer, 18);
    expect_true(ret >= 17);
    expect_true(ret <= 18);
    if (ret == 17) expect_true(! strncmp(buffer, "simple text file\n", 17));
    if (ret == 18) expect_true(! strncmp(buffer, "simple text file\r\n", 18));
    expect_false(callr_c_connection_is_eof(ccon));

    // But if we read again, EOF is set
    ret = callr_c_connection_read_chars(ccon, buffer, 17);
    expect_true(ret == 0);
    expect_true(callr_c_connection_is_eof(ccon));

    callr_c_connection_destroy(ccon);
  }

  test_that("A larger file that needs buffering") {
    char *filename;
    callr_file_handle_t handle = open_temp_file(&filename, 100000, 0);
    callr_connection_t *ccon =
      callr_c_connection_create(handle, CALLR_FILE_TYPE_ASYNCFILE, "UTF-8", 0);

    expect_false(callr_c_connection_is_eof(ccon));

    char buffer[1024];
    while (! callr_c_connection_is_eof(ccon)) {
      size_t ret = callr_c_connection_read_chars(ccon, buffer, 1024);
      if (ret == 0) expect_true(callr_c_connection_is_eof(ccon));
    }

    callr_c_connection_destroy(ccon);
    unlink(filename);
    free(filename);
  }

  test_that("Reading UTF-8 file") {
    char *filename;
    // A 2-byte character, then a 3-byte character, then a 4-byte one
    callr_file_handle_t handle =
      open_temp_file(&filename, 1, "\xc2\xa0\xe2\x86\x92\xf0\x90\x84\x82");
    callr_connection_t *ccon =
      callr_c_connection_create(handle, CALLR_FILE_TYPE_ASYNCFILE, "UTF-8", 0);

    expect_false(callr_c_connection_is_eof(ccon));

    char buffer[4];
    ssize_t ret = callr_c_connection_read_chars(ccon, buffer, 4);
    expect_true(ret == 2);
    expect_true(buffer[0] == '\xc2');
    expect_true(buffer[1] == '\xa0');

    ret = callr_c_connection_read_chars(ccon, buffer, 4);
    expect_true(ret == 3);
    expect_true(buffer[0] == '\xe2');
    expect_true(buffer[1] == '\x86');
    expect_true(buffer[2] == '\x92');

    ret = callr_c_connection_read_chars(ccon, buffer, 4);
    expect_true(ret == 4);
    expect_true(buffer[0] == '\xf0');
    expect_true(buffer[1] == '\x90');
    expect_true(buffer[2] == '\x84');
    expect_true(buffer[3] == '\x82');

    expect_false(callr_c_connection_is_eof(ccon));
    ret = callr_c_connection_read_chars(ccon, buffer, 4);
    expect_true(ret == 0);
    expect_true(callr_c_connection_is_eof(ccon));

    callr_c_connection_destroy(ccon);
    unlink(filename);
    free(filename);
  }

  test_that("Conversion to UTF-8") {
    char *filename;
    const char *latin1 = "\xe1\xe9\xed";
    const char *utf8 = "\xc3\xa1\xc3\xa9\xc3\xad";
    callr_file_handle_t handle = open_temp_file(&filename, 1, latin1);

    callr_connection_t *ccon =
      callr_c_connection_create(handle, CALLR_FILE_TYPE_ASYNCFILE, "latin1", 0);

    expect_false(callr_c_connection_is_eof(ccon));

    char buffer[10];
    ssize_t ret = callr_c_connection_read_chars(ccon, buffer, 10);
    expect_true(ret == 6);
    buffer[6] = '\0';
    expect_true(!strcmp(buffer, utf8));

    expect_false(callr_c_connection_is_eof(ccon));
    ret = callr_c_connection_read_chars(ccon, buffer, 4);
    expect_true(ret == 0);
    expect_true(callr_c_connection_is_eof(ccon));

    callr_c_connection_destroy(ccon);
    unlink(filename);
    free(filename);
  }

}

context("Reading lines") {

  test_that("Reading a line") {
    char *filename;
    callr_file_handle_t handle = open_temp_file(&filename, 50, "hello\n");
    callr_connection_t *ccon =
      callr_c_connection_create(handle, CALLR_FILE_TYPE_ASYNCFILE, "UTF-8", 0);

    char *linep = 0;
    size_t linecapp = 0;
    ssize_t read = callr_c_connection_read_line(ccon, &linep, &linecapp);
    expect_true(read == 5);
    expect_true(!strcmp(linep, "hello"));
    expect_true(linecapp == 6);

    free(linep);
    callr_c_connection_destroy(ccon);
    unlink(filename);
    free(filename);
  }

  test_that("Reading the last incomplete line") {
    char *filename;
    callr_file_handle_t handle = open_temp_file(&filename, 1, "hello\nhello\nagain");
    callr_connection_t *ccon =
      callr_c_connection_create(handle, CALLR_FILE_TYPE_ASYNCFILE, "UTF-8", 0);

    char *linep = 0;
    size_t linecapp = 0;
    ssize_t read;

    for (int i = 0; i < 2; i++) {
      read = callr_c_connection_read_line(ccon, &linep, &linecapp);
      expect_true(linep[5] == '\0');
      expect_true(read == 5);
      expect_true(!strcmp(linep, "hello"));
      expect_true(linecapp == 6);
      expect_false(callr_c_connection_is_eof(ccon));
    }

    read = callr_c_connection_read_line(ccon, &linep, &linecapp);
    expect_true(linep[5] == '\0');
    expect_true(read == 5);
    expect_true(!strcmp(linep, "again"));
    expect_true(linecapp == 6);
    expect_false(callr_c_connection_is_eof(ccon));

    read = callr_c_connection_read_chars(ccon, linep, 4);
    expect_true(read == 0);
    expect_true(callr_c_connection_is_eof(ccon));

    free(linep);
    callr_c_connection_destroy(ccon);
    unlink(filename);
    free(filename);
  }
}

// LCOV_EXCL_STOP
