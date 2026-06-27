# Checks for JPEG codec support
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


# JPEG
set(JPEG_SUPPORT FALSE)

# Option to prefer standard JPEG over TurboJPEG
option(jpeg-prefer-standard "prefer standard JPEG library over libjpeg-turbo" OFF)

# Try to find libjpeg-turbo first using CONFIG mode (more reliable, cross-platform)
# Only do this if we're not explicitly preferring standard JPEG
if(NOT jpeg-prefer-standard)
    find_package(libjpeg-turbo CONFIG QUIET)
    if(libjpeg-turbo_FOUND)
        # libjpeg-turbo was found via CONFIG
        # It provides targets like libjpeg-turbo::jpeg and libjpeg-turbo::turbojpeg
        # IMPORTANT: We need libjpeg-turbo::jpeg (standard libjpeg API), NOT turbojpeg
        # The turbojpeg target only provides the TurboJPEG API, which libtiff doesn't use
        # Create an alias to JPEG::JPEG for consistent usage throughout the project
        if(TARGET libjpeg-turbo::jpeg AND NOT TARGET JPEG::JPEG)
            add_library(JPEG::JPEG ALIAS libjpeg-turbo::jpeg)
            set(JPEG_FOUND TRUE)
            # Set JPEG_LIBRARIES to the target name
            set(JPEG_LIBRARIES libjpeg-turbo::jpeg)
            # For JPEG_INCLUDE_DIRS, try to get the property, but it's OK if it's not set
            get_target_property(_jpeg_includes libjpeg-turbo::jpeg INTERFACE_INCLUDE_DIRECTORIES)
            if(_jpeg_includes)
                set(JPEG_INCLUDE_DIRS "${_jpeg_includes}")
            else()
                set(JPEG_INCLUDE_DIRS "")
            endif()
            unset(_jpeg_includes)
        elseif(TARGET libjpeg-turbo::turbojpeg AND NOT TARGET JPEG::JPEG)
            # Only use turbojpeg as a fallback if jpeg target doesn't exist
            # This is unlikely to work correctly but better than nothing
            add_library(JPEG::JPEG ALIAS libjpeg-turbo::turbojpeg)
            set(JPEG_FOUND TRUE)
            # Set JPEG_LIBRARIES to the target name - CMake will handle includes automatically
            # when this is used in CMAKE_REQUIRED_LIBRARIES
            set(JPEG_LIBRARIES libjpeg-turbo::turbojpeg)
            # For JPEG_INCLUDE_DIRS, try to get the property, but it's OK if it's not set
            # since the target will provide the includes when used
            get_target_property(_jpeg_includes libjpeg-turbo::turbojpeg INTERFACE_INCLUDE_DIRECTORIES)
            if(_jpeg_includes)
                set(JPEG_INCLUDE_DIRS "${_jpeg_includes}")
            else()
                set(JPEG_INCLUDE_DIRS "")
            endif()
            unset(_jpeg_includes)
        endif()
    endif()
endif()

# Fall back to Find module if CONFIG didn't work
if(NOT JPEG_FOUND)
    find_package(JPEG)
endif()

option(jpeg "use libjpeg (required for JPEG compression)" ${JPEG_FOUND})
if (jpeg AND JPEG_FOUND)
    set(JPEG_SUPPORT TRUE)
endif()

# Old-jpeg
set(OJPEG_SUPPORT FALSE)
option(old-jpeg "support for Old JPEG compression (read-only)" ${JPEG_SUPPORT})
if (old-jpeg AND JPEG_SUPPORT)
    set(OJPEG_SUPPORT TRUE)
endif()

if (JPEG_SUPPORT)
    # Check for jpeg12_read_scanlines() which has been added in libjpeg-turbo 3.0
    # for dual 8/12 bit mode.
    include(CheckSymbolExists)
    include(CMakePushCheckState)
    cmake_push_check_state(RESET)

    # Set up includes and libraries for the check
    # For targets, we need to explicitly get the include directories
    if(TARGET ${JPEG_LIBRARIES})
        # It's an imported target - extract properties
        get_target_property(_jpeg_includes ${JPEG_LIBRARIES} INTERFACE_INCLUDE_DIRECTORIES)
        if(_jpeg_includes)
            set(CMAKE_REQUIRED_INCLUDES "${_jpeg_includes}")
        endif()
        unset(_jpeg_includes)
    elseif(JPEG_INCLUDE_DIRS)
        # It's a plain library - use the provided includes
        set(CMAKE_REQUIRED_INCLUDES "${JPEG_INCLUDE_DIRS}")
    endif()

    set(CMAKE_REQUIRED_LIBRARIES "${JPEG_LIBRARIES}")

    # Check if the jpeg12_read_scanlines symbol exists in jpeglib.h
    # This is more reliable than trying to compile code that calls it
    check_symbol_exists(jpeg_read_scanlines "stdio.h;jpeglib.h" HAVE_JPEGTURBO_DUAL_MODE_8)
    check_symbol_exists(jpeg12_read_scanlines "stdio.h;jpeglib.h" HAVE_JPEGTURBO_DUAL_MODE_12)
    set(HAVE_JPEGTURBO_DUAL_MODE_8_12 OFF)
    if (HAVE_JPEGTURBO_DUAL_MODE_8 AND HAVE_JPEGTURBO_DUAL_MODE_12)
        set(HAVE_JPEGTURBO_DUAL_MODE_8_12 ON)
    endif()

    cmake_pop_check_state()
endif()

if (NOT HAVE_JPEGTURBO_DUAL_MODE_8_12)

    # 12-bit jpeg mode in a dedicated libjpeg12 library
    set(JPEG12_INCLUDE_DIR JPEG12_INCLUDE_DIR-NOTFOUND CACHE PATH "Include directory for 12-bit libjpeg")
    set(JPEG12_LIBRARY JPEG12_LIBRARY-NOTFOUND CACHE FILEPATH "12-bit libjpeg library")
    set(JPEG_DUAL_MODE_8_12 FALSE)
    if (JPEG12_INCLUDE_DIR AND JPEG12_LIBRARY)
        set(JPEG12_LIBRARIES ${JPEG12_LIBRARY})
        set(JPEG12_FOUND TRUE)
    else()
        set(JPEG12_FOUND FALSE)
    endif()
    option(jpeg12 "enable libjpeg 8/12-bit dual mode (requires separate 12-bit libjpeg build)" ${JPEG12_FOUND})
    if (jpeg12 AND JPEG12_FOUND)
        set(JPEG_DUAL_MODE_8_12 TRUE)
        set(LIBJPEG_12_PATH "${JPEG12_INCLUDE_DIR}/jpeglib.h")
    endif()

endif()
