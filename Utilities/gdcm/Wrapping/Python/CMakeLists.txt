# Try to rebuild wrapping a little more often:
INCLUDE_REGULAR_EXPRESSION("^(gdcm).*$")
# TODO:
# SWIG is really a pain in the neck to use, a better alternative is Py++ which is using
# gccxml for the C++ parser and allow a full ANSI C++ support
# Note gcc has some issue with RTTI stuff:
# http://groups.google.com/group/comp.lang.c++.moderated/browse_thread/thread/ac889a7d9eac902f
# http://gcc.gnu.org/ml/gcc-help/2007-10/msg00239.html
# http://wiki.python.org/moin/boost.python/CrossExtensionModuleDependencies
# http://www.boost-consulting.com/writing/bpl.html
# http://gcc.gnu.org/ml/gcc/2002-05/msg00866.html
# http://mail.python.org/pipermail/c++-sig/2002-May/001021.html
# http://mail.python.org/pipermail/python-dev/2002-May/023923.html

FIND_PACKAGE(SWIG REQUIRED)
MARK_AS_ADVANCED(SWIG_DIR SWIG_EXECUTABLE SWIG_VERSION)
INCLUDE(${SWIG_USE_FILE})

#
# Do not cover this lib
#
CONFIGURE_FILE (${CMAKE_CURRENT_SOURCE_DIR}/.NoDartCoverage
                ${CMAKE_CURRENT_BINARY_DIR}/.NoDartCoverage)

# Note:
# python -c "from struct import pack; print  pack('5b', (41*len('99')), pow(8,2)+20, 4900**0.5, range(78)[-1], 10)"

INCLUDE_DIRECTORIES(
  "${GDCM_BINARY_DIR}/Source/Common"
  "${GDCM_SOURCE_DIR}/Source/Common"
  "${GDCM_SOURCE_DIR}/Source/DataStructureAndEncodingDefinition"
  "${GDCM_SOURCE_DIR}/Source/InformationObjectDefinition"
  "${GDCM_SOURCE_DIR}/Source/MediaStorageAndFileFormat"
  "${GDCM_SOURCE_DIR}/Source/DataDictionary"
  ${CMAKE_CURRENT_SOURCE_DIR}
)

FIND_PACKAGE(PythonLibs REQUIRED)
MARK_AS_ADVANCED(PYTHON_LIBRARY PYTHON_INCLUDE_PATH)
# Lamest excuse ever:
# http://mail.python.org/pipermail/python-list/2002-April/141189.html
# So here come craziest hack ever, since I cannot control the output of swig,
# I need to fake a Python.h file only for MSVC compilers...insane !
IF(MSVC)
  CONFIGURE_FILE( ${CMAKE_CURRENT_SOURCE_DIR}/Python.h.in
    ${CMAKE_CURRENT_BINARY_DIR}/Python.h @ONLY
  )
  INCLUDE_DIRECTORIES( ${CMAKE_CURRENT_BINARY_DIR} )
  # does not work quite as expected...
  #  IF(CMAKE_CONFIGURATION_TYPES)
  #SET_SOURCE_FILES_PROPERTIES(
  #        "${swig_generated_file_fullname}"
  #        PROPERTIES
  #        COMPILE_FLAGS "/U _DEBUG")
  #  ENDIF(CMAKE_CONFIGURATION_TYPES)

  ELSE(MSVC)
  # just plain including pyconfig.h is working...until one crazy python dev decide otherwise...
    INCLUDE_DIRECTORIES(
      ${PYTHON_INCLUDE_PATH}
    )
ENDIF(MSVC)
SET_SOURCE_FILES_PROPERTIES(gdcmswig.i PROPERTIES CPLUSPLUS ON)


# Some old swig 1.3 did not support this option:
#SET(CMAKE_SWIG_OUTDIR "${CMAKE_CURRENT_BINARY_DIR}")

#SET(GDCM_PYTHON_IMPLEMENTATION_NAME gdcmpython)
SET(GDCM_PYTHON_IMPLEMENTATION_NAME gdcmswig)
SET(MODULE_NAME gdcmswig)
# BUG: DO NOT USE -interface flag it is NOT supported in cmake and in cmake > 2.6 will cause infinite rebuild
# Ref: http://www.cmake.org/pipermail/cmake/2008-August/023237.html
# UseSWIG and -interface flag (was: Re: CMake 2.6.1 available for download)
#SET(CMAKE_SWIG_FLAGS "-interface _${GDCM_PYTHON_IMPLEMENTATION_NAME}")
#SEPARATE_ARGUMENTS(CMAKE_SWIG_FLAGS)

# While trying to get rid of the compilation warning in swig generated c++ code, I thought I could
# simply do the following:
#SET(CMAKE_CXX_FLAGS "")
# well no, you cannot, it get rid of some important flags, and make the _gdcm.so incompatible with
# the other gdcm lib. bad !!!
#SET (SWIG_MODULE_${MODULE_NAME}_EXTRA_DEPS ${SWIG_MODULE_${MODULE_NAME}_EXTRA_DEPS} ${CMAKE_CURRENT_SOURCE_DIR}/docstrings.i)
SWIG_ADD_MODULE(${GDCM_PYTHON_IMPLEMENTATION_NAME} python gdcmswig.i gdcmPythonFilter.cxx)
SWIG_LINK_LIBRARIES(${GDCM_PYTHON_IMPLEMENTATION_NAME} gdcmMSFF gdcmIOD ${PYTHON_LIBRARIES})

#MESSAGE(SWIG_MODULE_${MODULE_NAME}_EXTRA_DEPS)
#CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/gdcm.pth.in
#  ${CMAKE_CURRENT_BINARY_DIR}/gdcm.pth
#)

# Need to rebuild gdcmPYTHON...cxx when any header is modified...
#  FILE(GLOB_RECURSE headerfiles
#    "${GDCM_SOURCE_DIR}/Source/*.h"
#  )

#ADD_DEPENDENCIES(
#  ${SWIG_MODULE_${GDCM_PYTHON_IMPLEMENTATION_NAME}_REAL_NAME}
#  ${headerfiles}
#	)
#SET_SOURCE_FILES_PROPERTIES(
#  ${swig_generated_file_fullname}
#  PROPERTIES OBJECT_DEPENDS
#  #${headerfiles}
#  ${GDCM_SOURCE_DIR}/Source/MediaStorageAndFileFormat/gdcmDirectionCosines.h
#)
#SET_SOURCE_FILES_PROPERTIES(
#	${CMAKE_CURRENT_SOURCE_DIR}/gdcm.i
#  PROPERTIES OBJECT_DEPENDS
#  #${headerfiles}
#  ${GDCM_SOURCE_DIR}/Source/MediaStorageAndFileFormat/gdcmDirectionCosines.h
#)



# Python extension modules on Windows must have the extension ".pyd"
# instead of ".dll" as of Python 2.5.  Older python versions do support
# this suffix.
# http://docs.python.org/whatsnew/ports.html#SECTION0001510000000000000000
# <quote>
# Windows: .dll is no longer supported as a filename extension for extension modules.
# .pyd is now the only filename extension that will be searched for.
# </quote>
IF(WIN32 AND NOT CYGWIN)
  SET_TARGET_PROPERTIES(${SWIG_MODULE_${GDCM_PYTHON_IMPLEMENTATION_NAME}_REAL_NAME} PROPERTIES SUFFIX ".pyd")
ENDIF(WIN32 AND NOT CYGWIN)
#SET_TARGET_PROPERTIES(${SWIG_MODULE_${GDCM_PYTHON_IMPLEMENTATION_NAME}_REAL_NAME} PROPERTIES ${GDCM_LIBRARY_PROPERTIES})
SET_TARGET_PROPERTIES(${SWIG_MODULE_${GDCM_PYTHON_IMPLEMENTATION_NAME}_REAL_NAME} PROPERTIES LINK_INTERFACE_LIBRARIES "")

# swig generates a _gdcm.so and a gdcm.py, we need to copy gdcm.py to the proper place:
# gdcm.py is the interface name != implementation name, so we need to keep 'gdcm', so that 'import gdcm'
# from a python script always work
ADD_CUSTOM_COMMAND(
  TARGET    ${SWIG_MODULE_${GDCM_PYTHON_IMPLEMENTATION_NAME}_REAL_NAME}
  POST_BUILD
  COMMAND   ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_BINARY_DIR}/gdcmswig.py ${LIBRARY_OUTPUT_PATH}/${CMAKE_CFG_INTDIR}
  COMMAND   ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/gdcm.py ${LIBRARY_OUTPUT_PATH}/${CMAKE_CFG_INTDIR}
  DEPENDS ${swig_generated_file_fullname} ${CMAKE_CURRENT_BINARY_DIR}/gdcmswig.py ${CMAKE_CURRENT_SOURCE_DIR}/gdcm.py
  COMMENT   "Copy gdcmswig.py into ${LIBRARY_OUTPUT_PATH}"
)

#Module are always place in the library destination
#but for poor win32 user I decided to place them
# right next to the other dlls
IF(NOT GDCM_INSTALL_NO_LIBRARIES)
  INSTALL(TARGETS ${SWIG_MODULE_${GDCM_PYTHON_IMPLEMENTATION_NAME}_REAL_NAME}
    RUNTIME DESTINATION ${GDCM_INSTALL_BIN_DIR}/${python_site_package} COMPONENT PythonModule
    LIBRARY DESTINATION ${GDCM_INSTALL_LIB_DIR}/${python_site_package} COMPONENT Libraries
    ARCHIVE DESTINATION ${GDCM_INSTALL_LIB_DIR}/${python_site_package} COMPONENT Development
  )
  # the python file is not a dev file, but part of the gdcm module...
  INSTALL(FILES
    ${CMAKE_CURRENT_BINARY_DIR}/gdcmswig.py
    ${CMAKE_CURRENT_SOURCE_DIR}/gdcm.py
    #DESTINATION ${GDCM_INSTALL_LIB_DIR}/${python_site_package}/${subdir} COMPONENT Libraries
    DESTINATION ${GDCM_INSTALL_LIB_DIR}/${python_site_package} COMPONENT PythonModule
  )
  # pth file are actually a pain to use since they are *required* to be in some very specific location;
  # for instance you have a /tmp/gdcm.pth file and set PYTHONPATH=/tmp ... well believe it or not
  # this won't work ! I might actually have to go away from this solution for generic solution :(
  # only official rpm/deb could be sure of where the user will install those python module
  #INSTALL(FILES
  #  ${CMAKE_CURRENT_BINARY_DIR}/gdcm.pth
  #  DESTINATION ${GDCM_INSTALL_LIB_DIR}/${python_site_package} COMPONENT Libraries
  #)
ENDIF(NOT GDCM_INSTALL_NO_LIBRARIES)

# Test that will try to load any class in the target language: python
# it make sure swig was not broken accidentally
IF(BUILD_TESTING)
  ADD_PYTHON_TEST(TestWrapPython TestWrap.py ${GDCM_SOURCE_DIR}/Source)
  IF(GDCM_DOCUMENTATION)
    ADD_PYTHON_TEST(TestDoxy2SWIGPython doxy2swig.py ${GDCM_BINARY_DIR}/Utilities/doxygen/xml/index.xml ${GDCM_BINARY_DIR}/generated_docstrings.i)
  ENDIF(GDCM_DOCUMENTATION)
ENDIF(BUILD_TESTING)

# TODO
# python -c "from distutils import sysconfig; print sysconfig.get_python_lib()"
# /usr/lib/python2.4/site-packages
