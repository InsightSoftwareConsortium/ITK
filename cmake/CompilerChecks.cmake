# Compiler feature checks
#
# Copyright © 2015 Open Microscopy Environment / University of Dundee
# Copyright © 2021 Roger Leigh <rleigh@codelibre.net>
# Written by Roger Leigh <rleigh@codelibre.net>
#
# Permission to use, copy, modify, distribute, and sell this software and
# its documentation for any purpose is hereby granted without fee, provided
# that (i) the above copyright notices and this permission notice appear in
# all copies of the software and related documentation, and (ii) the names of
# Sam Leffler and Silicon Graphics may not be used in any advertising or
# publicity relating to the software without the specific, prior written
# permission of Sam Leffler and Silicon Graphics.
#
# THE SOFTWARE IS PROVIDED "AS-IS" AND WITHOUT WARRANTY OF ANY KIND,
# EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY
# WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.
#
# IN NO EVENT SHALL SAM LEFFLER OR SILICON GRAPHICS BE LIABLE FOR
# ANY SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY KIND,
# OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
# WHETHER OR NOT ADVISED OF THE POSSIBILITY OF DAMAGE, AND ON ANY THEORY OF
# LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE
# OF THIS SOFTWARE.


include(CheckCCompilerFlag)


# These are annoyingly verbose, produce false positives or don't work
# nicely with all supported compiler versions, so are disabled unless
# explicitly enabled.  These warnings are expected to be clean for
# CI builds when combined with fatal-warnings.
option(extra-warnings "Enable extra compiler warnings" OFF)

# These are annoyingly verbose, produce false positives or don't work
# nicely with all supported compiler versions, so are disabled unless
# explicitly enabled.  Not used in CI builds because it cannot be
# guaranteed that the builds will be warning-free and so cannot be
# combined with fatal-warnings without breaking the builds.
option(broken-warnings "Enable compiler warnings which will warn erroneously or are broken on some platforms" OFF)

# This will cause the compiler to fail when an error occurs.
option(fatal-warnings "Compiler warnings are errors" OFF)

# Enable C++ compatibility warnings for C code. Note that -Wc++-compat
# is limited and won't catch all C++ incompatibilities. For thorough
# checking, use cxx-compat-mode instead which compiles C as C++.
# Issues caught by -Wc++-compat:
# - using C++ keywords as identifiers (new, class, template, etc.)
# - some enum/int implicit conversions (GCC only)
option(cxx-compat-warnings "Enable C++ compatibility warnings for C code" OFF)

# Compile C source files as C++ to catch C++ incompatibilities as errors.
# This catches issues that -Wc++-compat misses:
# - implicit void* to typed pointer conversions
# - register storage class (removed in C++17)
# - goto/switch jumping over variable initialization
# - enum arithmetic and implicit conversions
# Note: This changes the language semantics and may break valid C code
# that relies on C-specific behavior.
option(cxx-compat-mode "Compile C files as C++ for compatibility checking" OFF)

# Check if the compiler supports each of the following additional
# flags, and enable them if supported.  This greatly improves the
# quality of the build by checking for a number of common problems,
# some of which are quite serious.
if(CMAKE_C_COMPILER_ID STREQUAL "GNU" OR
        CMAKE_C_COMPILER_ID MATCHES "Clang")
    set(test_flags
            -Wall
            -Winline
            -Wformat-security
            -Wpointer-arith
            -Wdisabled-optimization
            -Wno-unknown-pragmas
            -fstrict-aliasing)
    if(extra-warnings)
        list(APPEND test_flags
                -pedantic
                -Wextra
                -Wformat
                -Wformat-overflow
                -Wformat-nonliteral
                -Wformat-signedness
                -Wformat-truncation
                -Wnull-dereference
                -Wshadow
                -Wstrict-prototypes
                -Wmissing-prototypes
                -Wswitch-default
                -Wswitch-enum
                -Wwrite-strings
                -Wc99-c11-compat
                -Wconversion
                -Wsign-conversion
                -Warith-conversion
                -Wdouble-promotion
                -Wfloat-conversion
                -Wfloat-equal
                -Wuninitialized
                -Wduplicated-branches
                -Wduplicated-cond
                -Wunused-parameter
                -Wmissing-declarations
                -Wredundant-decls
                -Wsizeof-array-div
                -Wsizeof-pointer-div
                -Wsizeof-pointer-memaccess
                -Wlogical-op
                -Wlogical-not-parentheses
                -Wno-int-to-pointer-cast
                -Wdangling-else
                -Wunreachable-code
                -Wbool-operation
                -Wmissing-include-dirs
                -Wunused-local-typedefs
                -Wmisleading-indentation
                -Wunused-macros
                -Wundef
                -Wold-style-definition
                -Wnested-externs
                -Wjump-misses-init
                -Wvla
                -Warray-bounds=3
                -Wstringop-overflow=4
                -Walloc-zero
                -Wtrampolines
        )
    endif()
    if(broken-warnings)
        list(APPEND test_flags
                -Wcast-qual
                -Wcast-align
                -Wpadded
                -Wstack-usage=N
                -Wunsafe-loop-optimizations)
    endif()
    if(fatal-warnings)
        list(APPEND test_flags
                -Werror)
    endif()
    if(cxx-compat-warnings)
        list(APPEND test_flags
                -Wc++-compat)
    endif()
elseif(CMAKE_C_COMPILER_ID STREQUAL "MSVC")
    # Remove default /W3 from CMake's default flags to avoid D9025 warning
    string(REGEX REPLACE "/W[0-4]" "" CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
    string(REGEX REPLACE "/W[0-4]" "" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")

    set(test_flags
            # Suppress warnings from system headers (MSVC 16.10+)
            /external:anglebrackets
            /external:W0)
    if(extra-warnings)
        list(APPEND test_flags
                /W4
                /w44365
                /w44668
                /w44062
                /w44242
                /w44826
                /w44905
                /w44906)
    else()
        list(APPEND test_flags
                /W3)
    endif()
    if (fatal-warnings)
        list(APPEND test_flags
                /WX)
    endif()
endif()

foreach(flag ${test_flags})
    string(REGEX REPLACE "[^A-Za-z0-9]" "_" flag_var "${flag}")
    set(test_c_flag "C_FLAG${flag_var}")
    CHECK_C_COMPILER_FLAG(${flag} "${test_c_flag}")
    if (${test_c_flag})
        set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${flag}")
    endif (${test_c_flag})
endforeach(flag ${test_flags})

# Function to compile a target's C sources as C++ for compatibility checking
# This is a better approach than modifying CMAKE_C_FLAGS globally, as it:
# - Doesn't break CMake's feature detection tests
# - Uses proper target properties instead of global flags
# - Follows CMake best practices
function(tiff_target_compile_as_cxx target_name)
    if(NOT cxx-compat-mode)
        return()
    endif()

    # Get all source files for the target
    get_target_property(target_sources ${target_name} SOURCES)

    # Filter for C source files (*.c)
    set(c_sources "")
    foreach(source ${target_sources})
        if(source MATCHES "\\.c$")
            list(APPEND c_sources ${source})
        endif()
    endforeach()

    if(c_sources)
        # Set these C files to be compiled as C++
        set_source_files_properties(${c_sources}
            PROPERTIES
                LANGUAGE CXX
        )
        # Use target properties for C++ standard (more portable than COMPILE_FLAGS)
        set_target_properties(${target_name}
            PROPERTIES
                CXX_STANDARD 17
                CXX_STANDARD_REQUIRED ON
                CXX_EXTENSIONS OFF
        )
        message(STATUS "C++ compatibility mode: compiling ${target_name} C sources as C++17")
    endif()
endfunction()
