# Set GIT tag here for use in multiple CMakeLists.txt
#
set(
  DCMTK_GIT_REPOSITORY
  "https://github.com/InsightSoftwareConsortium/DCMTK.git"
)
# DCMTK-3.7.0 patched
# Patch list:

#Francois Budin (1):
#COMP: Uses ITK PNG instead of system PNG

#Hans Johnson (3):
#COMP: Mods to allow using ITK libs
#COMP: Use ITK-provided JPEG/TIFF hints instead of find_package
#COMP: Consume standard *_LIBRARIES variables for TIFF/JPEG/PNG/ZLIB

#Jean-Christophe Fillion-Robin (1):
#COMP: Set 3rd party package CMake variables only if needed

#Matt McCormick (7):
#COMP: Avoid dcmjpeg use of setjmp with WASI
#COMP: Enable dcmjpeg, dcmjpls to support plastimatch
#COMP: Add missing oflog libraries for native unix
#COMP: Update __cxa_allocate_exception for WASI-SDK 22
#COMP: Do not try to make mmap calls with wasi
#COMP: Add WASI exception shim
#COMP: Do not use non-type template argument -1 in variadic template

#Shreeraj Jadhav (9):
#COMP: enable dcmfg for dcmqi seg object read-write
#COMP: Fixes for wasi compiler errors after enabling dcmpstat
#COMP: Enable dcmpstat to support GSPS/CSPS IODs
#COMP: disable use of log4cplus::tistringstream
#COMP: add fixes for Emscripten linker errors
#COMP: Enable dcm2pdf, fix WIN32 build
#COMP: disable unneeded tests and apps
#COMP: fix WASI compiler errors
#COMP: fix missing <csetjmp> header issue

set(DCMTK_GIT_TAG "7c7dc11d8e638711ac9c475210d7c7b352832cee") # for/itk-dcmtk-3.7.0-ccfd10b
