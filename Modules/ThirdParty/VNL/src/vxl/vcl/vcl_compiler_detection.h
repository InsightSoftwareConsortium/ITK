#ifndef VXL_COMPILER_DETECTION_H
#define VXL_COMPILER_DETECTION_H

/*
Remove deprecated compiler feature detection mechansims

Since cmake version 3.0 the use of The WriteCompilerDetectionHeader
module is not recommended.

Run "cmake --help-policy CMP0120" for policy details.

Prevent configuration time warning messages when using
cmake versions greater than 3.20
*/
#if !defined(_MSC_VER) //MSVC does not consistently define __cplusplus
#pragma message "The vcl_compiler_detection.h header file is no longer used.\n"   \
         "In most cases, references to this file can simply be removed.\n" \
         "Use of VXL_DEPRECATED or VXL_DEPRECATED_MSG should include\n"    \
         "vcl_compiler.h instead."
#else
#warning "The vcl_compiler_detection.h header file is no longer used.\n"   \
         "In most cases, references to this file can simply be removed.\n" \
         "Use of VXL_DEPRECATED or VXL_DEPRECATED_MSG should include\n"    \
         "vcl_compiler.h instead."
#endif

#include <vcl_compiler.h>

#endif
