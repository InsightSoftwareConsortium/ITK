set(dcmtk3rdParty ${dcmtkSource}/CMake/3rdparty.cmake)

#
# Newer versions of TIFF depend on JPEG
file(READ ${dcmtk3rdParty} code)
string(REPLACE "SET(LIBTIFF_LIBS \${TIFF_LIBRARY})
"
"SET(LIBTIFF_LIBS \${TIFF_LIBRARY})
    list(APPEND LIBTIFF_LIBS \${JPEG_LIBRARY})
" code "${code}")
#
# we pass in the TIFF and ZLIB library stuff,
# running find_package for those libraries
# screws up using the ITK versions
if(NOT ITK_USE_SYSTEM_TIFF)
  string(REPLACE "FIND_PACKAGE(TIFF)
" "" code "${code}")
endif()

if(NOT ITK_USE_SYSTEM_ZLIB)
string(REPLACE "FIND_PACKAGE(ZLIB)
" "" code "${code}")
endif()

file(WRITE ${dcmtk3rdParty} "${code}")

#
# in order to use the ITK versions of ZLIB and TIFF
# it is necessary to use the ITK symbol-mangled versions
file(GLOB_RECURSE dcmtk_src RELATIVE ${dcmtkSource} "*.cc" "*.h")

foreach (filename ${dcmtk_src})
  set(_filename ${dcmtkSource}/${filename})
  #  message("patching ${_filename}")
  file(READ ${_filename} sourcecode)
  if(NOT ITK_USE_SYSTEM_ZLIB)
    string(REPLACE "<zlib.h>"
      "\"itk_zlib.h\"" sourcecode "${sourcecode}")
  endif()
  if(NOT ITK_USE_SYSTEM_TIFF)
    string(REPLACE "<tiffio.h>"
      "\"itk_tiff.h\"" sourcecode "${sourcecode}")
  endif()
  file(WRITE ${_filename} "${sourcecode}")
endforeach(filename)

#
# suppress building the applications
file(GLOB_RECURSE dcmtk_cmakelists RELATIVE ${dcmtkSource} "CMakeLists.txt")

foreach(cmakelists ${dcmtk_cmakelists})
  set(_cmakelists ${dcmtkSource}/${cmakelists})
  message("CMakeLists ${_cmakelists}")
  file(READ ${_cmakelists} source)
  string(REGEX REPLACE
    "SUBDIRS\\((.*) apps "
    "SUBDIRS(\\1 "
    source
    "${source}")
  file(WRITE ${_cmakelists} "${source}")
endforeach(cmakelists)

#
# To get libraries to be built in the ITK lib directory,
# have to kill the stuff in the main CMakeLists.txt that
# changes target destinations
set(dcmtkCmakeLists_txt ${dcmtkSource}/CMakeLists.txt)

file(READ ${dcmtkCmakeLists_txt} cmakelists)

string(REPLACE "# build output files in these directories
SET(CMAKE_ARCHIVE_OUTPUT_DIRECTORY \"\${CMAKE_BINARY_DIR}/lib\")
SET(CMAKE_LIBRARY_OUTPUT_DIRECTORY \"\${CMAKE_BINARY_DIR}/lib\")
SET(CMAKE_RUNTIME_OUTPUT_DIRECTORY \"\${CMAKE_BINARY_DIR}/bin\")
"
"" cmakelists "${cmakelists}")
file(WRITE ${dcmtkCmakeLists_txt} "${cmakelists}")
