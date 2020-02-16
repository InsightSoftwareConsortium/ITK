# Set GIT tag here for use in multiple CMakeLists.txt
#
set(DCMTK_GIT_REPOSITORY "https://github.com/InsightSoftwareConsortium/DCMTK.git")
# 2018.01.16 patched
# Patch list:
# COMP: Uses ITK PNG instead of system PNG
# BUG: when using ITK_ZLIB, wasn't finding zlib header
# COMP: Set 3rd party package CMake variables only if needed
# COMP: Mods to allow using ITK libs
# Support CMAKE_CROSSCOMPILING_EMULATOR
# Support the generation of arith.h with Emscription
# Do not use non-type template argument -1 in variadic template

set(DCMTK_GIT_TAG "14393c0baa743469b5826bea724c048e221dd08b") # 20200215 master
