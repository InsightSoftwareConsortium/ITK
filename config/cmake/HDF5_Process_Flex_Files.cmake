#
# Copyright by The HDF Group.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the COPYING file, which can be found at the root of the source code
# distribution tree, or in https://support.hdfgroup.org/ftp/HDF5/releases.
# If you do not have access to either file, you may request a copy from
# help@hdfgroup.org.
#
# post process flex/bison files

message (STATUS "File: ${GEN_DIR} ${FILE_PARSE} ${FILE_ANALYZE}")

if (FILE_PARSE)
  # fix H5LTparse.c to declare H5LTyyparse return type as an hid_t
  # instead of int.  Currently the generated function H5LTyyparse is
  # generated with a return value of type int, which is a mapping to the
  # flex yyparse function.  The return value in the HL library should be
  # an hid_t.
  # I propose to not use flex to generate this function, but for now I am
  # adding a perl command to find and replace this function declaration in
  # H5LTparse.c.
    file (READ ${GEN_DIR}/${FILE_PARSE}.c TEST_STREAM)
    string (REGEX REPLACE "int yyparse" "hid_t yyparse" TEST_STREAM "${TEST_STREAM}")
    string (REGEX REPLACE "int\nyyparse" "hid_t\nyyparse" TEST_STREAM "${TEST_STREAM}")
    string (REGEX REPLACE "int H5LTyyparse" "hid_t H5LTyyparse" TEST_STREAM "${TEST_STREAM}")
    file (WRITE ${FILE_PARSE}.c "${TEST_STREAM}")
    message (STATUS "replacing signature in H5LTparse.c")

  # Add code that disables warnings in the flex/bison-generated code.
  #
  # Note that the GCC pragmas did not exist until gcc 4.2. Earlier versions
  # will simply ignore them, but we want to avoid those warnings.
    file (READ ${FILE_PARSE}.c TEST_STREAM)
    file (WRITE ${FILE_PARSE}.c "
#if __GNUC__ >= 4 && __GNUC_MINOR__ >=2\n
#pragma GCC diagnostic ignored \"-Wconversion\"\n
#pragma GCC diagnostic ignored \"-Wimplicit-function-declaration\"\n
#pragma GCC diagnostic ignored \"-Wlarger-than=\"\n
#pragma GCC diagnostic ignored \"-Wmissing-prototypes\"\n
#pragma GCC diagnostic ignored \"-Wnested-externs\"\n
#pragma GCC diagnostic ignored \"-Wold-style-definition\"\n
#pragma GCC diagnostic ignored \"-Wsign-compare\"\n
#pragma GCC diagnostic ignored \"-Wsign-conversion\"\n
#pragma GCC diagnostic ignored \"-Wstrict-prototypes\"\n
#pragma GCC diagnostic ignored \"-Wswitch-default\"\n
#pragma GCC diagnostic ignored \"-Wunused-function\"\n
#pragma GCC diagnostic ignored \"-Wunused-macros\"\n
#pragma GCC diagnostic ignored \"-Wunused-parameter\"\n
#pragma GCC diagnostic ignored \"-Wredundant-decls\"\n
#elif defined __SUNPRO_CC\n
#pragma disable_warn\n
#elif defined _MSC_VER\n
#pragma warning(push, 1)\n
#endif\n
    ")
    file (APPEND ${FILE_PARSE}.c "${TEST_STREAM}")
    message (STATUS "processed pragma in ${FILE_PARSE}")
    file (READ ${GEN_DIR}/${FILE_PARSE}.h TEST_STREAM)
    file (WRITE ${FILE_PARSE}.h "${TEST_STREAM}")
endif ()

if (FILE_ANALYZE)
  # Add code that disables warnings in the flex/bison-generated code.
  #
  # Note that the GCC pragmas did not exist until gcc 4.2. Earlier versions
  # will simply ignore them, but we want to avoid those warnings.
    file (READ ${GEN_DIR}/${FILE_ANALYZE} TEST_STREAM)
    file (WRITE ${FILE_ANALYZE} "
#if __GNUC__ >= 4 && __GNUC_MINOR__ >=2\n
#pragma GCC diagnostic ignored \"-Wconversion\"\n
#pragma GCC diagnostic ignored \"-Wimplicit-function-declaration\"\n
#pragma GCC diagnostic ignored \"-Wlarger-than=\"\n
#pragma GCC diagnostic ignored \"-Wmissing-prototypes\"\n
#pragma GCC diagnostic ignored \"-Wnested-externs\"\n
#pragma GCC diagnostic ignored \"-Wold-style-definition\"\n
#pragma GCC diagnostic ignored \"-Wsign-compare\"\n
#pragma GCC diagnostic ignored \"-Wsign-conversion\"\n
#pragma GCC diagnostic ignored \"-Wstrict-prototypes\"\n
#pragma GCC diagnostic ignored \"-Wswitch-default\"\n
#pragma GCC diagnostic ignored \"-Wunused-function\"\n
#pragma GCC diagnostic ignored \"-Wunused-macros\"\n
#pragma GCC diagnostic ignored \"-Wunused-parameter\"\n
#pragma GCC diagnostic ignored \"-Wredundant-decls\"\n
#elif defined __SUNPRO_CC\n
#pragma disable_warn\n
#elif defined _MSC_VER\n
#pragma warning(push, 1)\n
#endif\n
    ")
    file (APPEND ${FILE_ANALYZE} "${TEST_STREAM}")
    message (STATUS "processed pragma in ${FILE_ANALYZE}")
endif ()
