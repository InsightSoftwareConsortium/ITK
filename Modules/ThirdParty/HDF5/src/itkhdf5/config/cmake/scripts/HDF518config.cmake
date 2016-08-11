#############################################################################################
### ${CTEST_SCRIPT_ARG} is of the form OPTION=VALUE                                       ###
### BUILD_GENERATOR required [Unix, VS2015, VS201564, VS2013, VS201364, VS2012, VS201264] ###
### ctest -S HDF518config.cmake,BUILD_GENERATOR=VS201264 -C Release -V -O hdf518.log      ###
#############################################################################################

cmake_minimum_required(VERSION 3.1.0 FATAL_ERROR)
############################################################################
# Usage:
#     ctest -S HDF518config.cmake,OPTION=VALUE -C Release -VV -O test.log
# where valid options for OPTION are:
#     BUILD_GENERATOR - The cmake build generator:
#            Unix    * Unix Makefiles
#            VS2015    * Visual Studio 14 2015
#            VS201564 * Visual Studio 14 2015 Win64
#            VS2013    * Visual Studio 12 2013
#            VS201364 * Visual Studio 12 2013 Win64
#            VS2012    * Visual Studio 11 2012
#            VS201264 * Visual Studio 11 2012 Win64
#
#     INSTALLDIR  -  root folder where hdf5 is installed
#     CTEST_BUILD_CONFIGURATION  - Release, Debug, etc
#     CTEST_SOURCE_NAME  -  source folder
#     STATIC_LIBRARIES  -  Build/use static libraries
#     FORTRAN_LIBRARIES -  Build/use fortran libraries
#     NO_MAC_FORTRAN  - Yes to be SHARED on a Mac
##############################################################################

set(CTEST_SOURCE_VERSION 1.8.16)
set(CTEST_SOURCE_VERSEXT "")

##############################################################################
# handle input parameters to script.
#BUILD_GENERATOR - which CMake generator to use, required
#INSTALLDIR - HDF5-1.8 root folder
#CTEST_BUILD_CONFIGURATION - Release, Debug, RelWithDebInfo
#CTEST_SOURCE_NAME - name of source folder; HDF5-1.8.16
#STATIC_LIBRARIES - Default is YES
#FORTRAN_LIBRARIES - Default is NO
#NO_MAC_FORTRAN - set to TRUE to allow shared libs on a Mac
if(DEFINED CTEST_SCRIPT_ARG)
    # transform ctest script arguments of the form
    # script.ctest,var1=value1,var2=value2
    # to variables with the respective names set to the respective values
    string(REPLACE "," ";" script_args "${CTEST_SCRIPT_ARG}")
    foreach(current_var ${script_args})
        if ("${current_var}" MATCHES "^([^=]+)=(.+)$")
            set("${CMAKE_MATCH_1}" "${CMAKE_MATCH_2}")
        endif()
    endforeach()
endif()

# build generator must be defined
if(NOT DEFINED BUILD_GENERATOR)
  message(FATAL_ERROR "BUILD_GENERATOR must be defined - Unix, VS2013, VS201364, VS2012, or VS201264")
else()
  if(${BUILD_GENERATOR} STREQUAL "Unix")
    set(CTEST_CMAKE_GENERATOR "Unix Makefiles")
  elseif(${BUILD_GENERATOR} STREQUAL "VS2015")
    set(CTEST_CMAKE_GENERATOR "Visual Studio 14 2015")
  elseif(${BUILD_GENERATOR} STREQUAL "VS201564")
    set(CTEST_CMAKE_GENERATOR "Visual Studio 14 2015 Win64")
  elseif(${BUILD_GENERATOR} STREQUAL "VS2013")
    set(CTEST_CMAKE_GENERATOR "Visual Studio 12 2013")
  elseif(${BUILD_GENERATOR} STREQUAL "VS201364")
    set(CTEST_CMAKE_GENERATOR "Visual Studio 12 2013 Win64")
  elseif(${BUILD_GENERATOR} STREQUAL "VS2012")
    set(CTEST_CMAKE_GENERATOR "Visual Studio 11 2012")
  elseif(${BUILD_GENERATOR} STREQUAL "VS201264")
    set(CTEST_CMAKE_GENERATOR "Visual Studio 11 2012 Win64")
  else()
    message(FATAL_ERROR "Invalid BUILD_GENERATOR must be - Unix, VS2013, VS201364, VS2012, or VS201264")
  endif()
endif()

if(NOT DEFINED INSTALLDIR)
  if(WIN32)
    set(INSTALLDIR "C:\\Program\ Files\\myhdf5")
  else()
    set(INSTALLDIR "/usr/local/myhdf5")
  endif()
endif()
if(NOT DEFINED CTEST_BUILD_CONFIGURATION)
    set(CTEST_BUILD_CONFIGURATION "Release")
endif()
if(NOT DEFINED CTEST_SOURCE_NAME)
    set(CTEST_SOURCE_NAME "hdf5-${CTEST_SOURCE_VERSION}${CTEST_SOURCE_VERSEXT}")
endif()
if(NOT DEFINED STATIC_LIBRARIES)
    set(STATICLIBRARIES "YES")
endif()
if(NOT DEFINED FORTRAN_LIBRARIES)
    set(FORTRANLIBRARIES "NO")
endif()

set(CTEST_BINARY_NAME "build")
set(CTEST_DASHBOARD_ROOT "${CTEST_SCRIPT_DIRECTORY}")
if(WIN32)
  set(CTEST_SOURCE_DIRECTORY "${CTEST_DASHBOARD_ROOT}\\${CTEST_SOURCE_NAME}")
  set(CTEST_BINARY_DIRECTORY "${CTEST_DASHBOARD_ROOT}\\${CTEST_BINARY_NAME}")
else()
  set(CTEST_SOURCE_DIRECTORY "${CTEST_DASHBOARD_ROOT}/${CTEST_SOURCE_NAME}")
  set(CTEST_BINARY_DIRECTORY "${CTEST_DASHBOARD_ROOT}/${CTEST_BINARY_NAME}")
endif()

###################################################################
#########       Following describes compiler           ############
if(WIN32)
  set(SITE_OS_NAME "Windows")
  set(SITE_OS_VERSION "WIN7")
  if(${BUILD_GENERATOR} STREQUAL "VS201564")
    set(SITE_OS_BITS "64")
    set(SITE_COMPILER_NAME "vs2015")
    set(SITE_COMPILER_VERSION "14")
  elseif(${BUILD_GENERATOR} STREQUAL "VS2015")
    set(SITE_OS_BITS "32")
    set(SITE_COMPILER_NAME "vs2015")
    set(SITE_COMPILER_VERSION "14")
  elseif(${BUILD_GENERATOR} STREQUAL "VS201364")
    set(SITE_OS_BITS "64")
    set(SITE_COMPILER_NAME "vs2013")
    set(SITE_COMPILER_VERSION "12")
  elseif(${BUILD_GENERATOR} STREQUAL "VS2013")
    set(SITE_OS_BITS "32")
    set(SITE_COMPILER_NAME "vs2013")
    set(SITE_COMPILER_VERSION "12")
  elseif(${BUILD_GENERATOR} STREQUAL "VS201264")
    set(SITE_OS_BITS "64")
    set(SITE_COMPILER_NAME "vs2012")
    set(SITE_COMPILER_VERSION "11")
  elseif(${BUILD_GENERATOR} STREQUAL "VS2012")
    set(SITE_OS_BITS "32")
    set(SITE_COMPILER_NAME "vs2012")
    set(SITE_COMPILER_VERSION "11")
  endif()
##  Set the following to unique id your computer  ##
  set(CTEST_SITE "WIN7${BUILD_GENERATOR}.XXXX")
else()
##  Set the following to unique id your computer  ##
  if(APPLE)
    set(CTEST_SITE "MAC.XXXX")
  else()
    set(CTEST_SITE "LINUX.XXXX")
  endif()
endif()
###################################################################

###################################################################
#########       Following is for submission to CDash   ############
###################################################################
set(MODEL "Experimental")
###################################################################

###################################################################
#####       Following controls CDash submission               #####
#set(LOCAL_SUBMIT "TRUE")
#####       Following controls test process                   #####
#set(LOCAL_SKIP_TEST "TRUE")
#set(LOCAL_MEMCHECK_TEST "TRUE")
#set(LOCAL_COVERAGE_TEST "TRUE")
#####       Following controls cpack command                  #####
#set(LOCAL_NO_PACKAGE "TRUE")
#####       Following controls source update                  #####
#set(LOCAL_UPDATE "TRUE")
set(REPOSITORY_URL "http://svn.hdfgroup.uiuc.edu/hdf5/branches/hdf5_1_8_16")
#uncomment to use a compressed source file: *.tar on linux or mac *.zip on windows
#set(CTEST_USE_TAR_SOURCE "${CTEST_SOURCE_VERSION}")
###################################################################

###################################################################
####  Change default configuration of options in config/cmake/cacheinit.cmake file ###
####  format: set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DXXX:YY=ZZZZ")

###################################################################
if(${STATICLIBRARIES})
  set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DBUILD_SHARED_LIBS:BOOL=OFF")
  #########       Following describes computer           ############
  ## following is optional to describe build                       ##
  set(SITE_BUILDNAME_SUFFIX "STATIC")
endif()
###################################################################

### uncomment/comment and change the following lines for other configuration options

####      ext libraries       ####
### ext libs from tgz
set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_ALLOW_EXTERNAL_SUPPORT:STRING=TGZ -DTGZPATH:PATH=${CTEST_SCRIPT_DIRECTORY}")
### ext libs from svn
#set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_ALLOW_EXTERNAL_SUPPORT:STRING=SVN")
### ext libs on system
#set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DZLIB_LIBRARY:FILEPATH=some_location/lib/zlib.lib -DZLIB_INCLUDE_DIR:PATH=some_location/include")
#set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DSZIP_LIBRARY:FILEPATH=some_location/lib/szlib.lib -DSZIP_INCLUDE_DIR:PATH=some_location/include")
### disable ext libs building
#set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_ENABLE_Z_LIB_SUPPORT:BOOL=OFF")
#set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_ENABLE_SZIP_SUPPORT:BOOL=OFF")
#set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_ENABLE_SZIP_ENCODING:BOOL=OFF")
####      fortran       ####
if(${FORTRANLIBRARIES})
  set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_BUILD_FORTRAN:BOOL=ON")
  ### enable Fortran 2003 depends on HDF5_BUILD_FORTRAN
  set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_ENABLE_F2003:BOOL=ON")
else()
  set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_BUILD_FORTRAN:BOOL=OFF")
  ### enable Fortran 2003 depends on HDF5_BUILD_FORTRAN
  set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_ENABLE_F2003:BOOL=OFF")
endif()

### disable test program builds
#set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DBUILD_TESTING:BOOL=OFF")

### disable packaging
#set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_NO_PACKAGES:BOOL=ON")
### Create install package with external libraries (szip, zlib, jpeg)
set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DHDF5_PACKAGE_EXTLIBS:BOOL=ON")

### change install prefix
set(ADD_BUILD_OPTIONS "${ADD_BUILD_OPTIONS} -DCMAKE_INSTALL_PREFIX:PATH=${INSTALLDIR}")

###################################################################

if(WIN32)
  include(${CTEST_SCRIPT_DIRECTORY}\\CTestScript.cmake)
  if(EXISTS "${CTEST_BINARY_DIRECTORY}\\HDF5-${CTEST_SOURCE_VERSION}${CTEST_SOURCE_VERSEXT}-win${SITE_OS_BITS}.exe")
    file(COPY "${CTEST_BINARY_DIRECTORY}\\HDF5-${CTEST_SOURCE_VERSION}${CTEST_SOURCE_VERSEXT}-win${SITE_OS_BITS}.exe" DESTINATION ${CTEST_SCRIPT_DIRECTORY})
  endif()
  if(EXISTS "${CTEST_BINARY_DIRECTORY}\\HDF5-${CTEST_SOURCE_VERSION}${CTEST_SOURCE_VERSEXT}-win${SITE_OS_BITS}.msi")
    file(COPY "${CTEST_BINARY_DIRECTORY}\\HDF5-${CTEST_SOURCE_VERSION}${CTEST_SOURCE_VERSEXT}-win${SITE_OS_BITS}.msi" DESTINATION  ${CTEST_SCRIPT_DIRECTORY})
  endif()
  if(EXISTS "${CTEST_BINARY_DIRECTORY}\\HDF5-${CTEST_SOURCE_VERSION}${CTEST_SOURCE_VERSEXT}-win${SITE_OS_BITS}.zip")
    file(COPY "${CTEST_BINARY_DIRECTORY}\\HDF5-${CTEST_SOURCE_VERSION}${CTEST_SOURCE_VERSEXT}-win${SITE_OS_BITS}.zip" DESTINATION  ${CTEST_SCRIPT_DIRECTORY})
  endif()
else()
  include(${CTEST_SCRIPT_DIRECTORY}/CTestScript.cmake)
  if(APPLE)
    if(EXISTS "${CTEST_BINARY_DIRECTORY}/HDF5-${CTEST_SOURCE_VERSION}${CTEST_SOURCE_VERSEXT}-Darwin.dmg")
      file(COPY "${CTEST_BINARY_DIRECTORY}/HDF5-${CTEST_SOURCE_VERSION}${CTEST_SOURCE_VERSEXT}-Darwin.dmg" DESTINATION ${CTEST_SCRIPT_DIRECTORY})
    endif()
    if(EXISTS "${CTEST_BINARY_DIRECTORY}/HDF5-${CTEST_SOURCE_VERSION}${CTEST_SOURCE_VERSEXT}-Darwin.tar.gz")
      file(COPY "${CTEST_BINARY_DIRECTORY}/HDF5-${CTEST_SOURCE_VERSION}${CTEST_SOURCE_VERSEXT}-Darwin.tar.gz" DESTINATION  ${CTEST_SCRIPT_DIRECTORY})
    endif()
    if(EXISTS "${CTEST_BINARY_DIRECTORY}/HDF5-${CTEST_SOURCE_VERSION}${CTEST_SOURCE_VERSEXT}-Darwin.sh")
      file(COPY "${CTEST_BINARY_DIRECTORY}/HDF5-${CTEST_SOURCE_VERSION}${CTEST_SOURCE_VERSEXT}-Darwin.sh" DESTINATION ${CTEST_SCRIPT_DIRECTORY})
    endif()
  else()
    if(CYGWIN)
      if(EXISTS "${CTEST_BINARY_DIRECTORY}/HDF5-${CTEST_SOURCE_VERSION}${CTEST_SOURCE_VERSEXT}-CYGWIN.sh")
        file(COPY "${CTEST_BINARY_DIRECTORY}/HDF5-${CTEST_SOURCE_VERSION}${CTEST_SOURCE_VERSEXT}-CYGWIN.sh" DESTINATION ${CTEST_SCRIPT_DIRECTORY})
      endif()
      if(EXISTS "${CTEST_BINARY_DIRECTORY}/HDF5-${CTEST_SOURCE_VERSION}${CTEST_SOURCE_VERSEXT}-CYGWIN.tar.gz")
        file(COPY "${CTEST_BINARY_DIRECTORY}/HDF5-${CTEST_SOURCE_VERSION}${CTEST_SOURCE_VERSEXT}-CYGWIN.tar.gz" DESTINATION  ${CTEST_SCRIPT_DIRECTORY})
      endif()
    else()
      if(EXISTS "${CTEST_BINARY_DIRECTORY}/HDF5-${CTEST_SOURCE_VERSION}${CTEST_SOURCE_VERSEXT}-Linux.sh")
        file(COPY "${CTEST_BINARY_DIRECTORY}/HDF5-${CTEST_SOURCE_VERSION}${CTEST_SOURCE_VERSEXT}-Linux.sh" DESTINATION ${CTEST_SCRIPT_DIRECTORY})
      endif()
      if(EXISTS "${CTEST_BINARY_DIRECTORY}/HDF5-${CTEST_SOURCE_VERSION}${CTEST_SOURCE_VERSEXT}-Linux.tar.gz")
        file(COPY "${CTEST_BINARY_DIRECTORY}/HDF5-${CTEST_SOURCE_VERSION}${CTEST_SOURCE_VERSEXT}-Linux.tar.gz" DESTINATION  ${CTEST_SCRIPT_DIRECTORY})
      endif()
    endif()
  endif()
endif()
