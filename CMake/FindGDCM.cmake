#
# This module finds if GDCM is installed and determines where the
# include files and libraries are. It also determines what the name of
# the library is. This code sets the following variables:
# For more info on this library go to:
#
#        http://www.creatis.insa-lyon.fr/Public/Gdcm/
#
#  GDCM_FOUND             = system has GDCM and it should be used
#  GDCM_LIBRARIES         = full path to the GDCM library and linker flags on unix
#  CMAKE_GDCM_CXX_FLAGS   = compiler flags for building GDCM 
#  GDCM_INCLUDE_DIR       = include path of GDCM

IF(WIN32)
  IF(NOT UNIX)

  SET (GDCM_POSSIBLE_LIB_PATHS
    /usr/lib
    /usr/local/lib
    $ENV{CREATIS}/gdcm/lib/Release
    $ENV{CREATIS}/gdcm/lib/Debug
    $ENV{CREATIS}/gdcm/lib/
    "[HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\gdcmPython-py2.2;UninstallString]\\..\\lib"
    "C:\\gdcm\\lib"
  )

  FIND_LIBRARY(GDCM_STATIC_LIBRARY
    NAMES gdcm gdcmdll
    PATHS ${GDCM_POSSIBLE_LIB_PATHS} 
  )

  FIND_LIBRARY(GDCM_SHARED_LIBRARY
    NAMES gdcm gdcmdll
    PATHS ${GDCM_POSSIBLE_LIB_PATHS} 
  )

  FIND_LIBRARY(GDCMIJPEG8_STATIC_LIBRARY
    NAMES gdcmijpeg8 libgdcmijpeg8 
    PATHS ${GDCM_POSSIBLE_LIB_PATHS} 
  )

  FIND_LIBRARY(GDCMIJPEG8_SHARED_LIBRARY
    NAMES gdcmijpeg8 libgdcmijpeg8
    PATHS ${GDCM_POSSIBLE_LIB_PATHS} 
  )

  FIND_LIBRARY(GDCMIJPEG12_STATIC_LIBRARY
    NAMES gdcmijpeg12 libgdcmijpeg12
    PATHS ${GDCM_POSSIBLE_LIB_PATHS} 
  )

  FIND_LIBRARY(GDCMIJPEG12_SHARED_LIBRARY
    NAMES gdcmijpeg12 libgdcmijpeg12
    PATHS ${GDCM_POSSIBLE_LIB_PATHS} 
  )

  FIND_LIBRARY(GDCMLJPEG_STATIC_LIBRARY
    NAMES gdcmljpeg libgdcmljpeg
    PATHS ${GDCM_POSSIBLE_LIB_PATHS} 
  )

  FIND_LIBRARY(GDCMLJPEG_SHARED_LIBRARY
    NAMES gdcmljpeg libgdcmljpeg
    PATHS ${GDCM_POSSIBLE_LIB_PATHS} 
  )

  FIND_LIBRARY(VTKGDCM_STATIC_LIBRARY
    NAMES vtkgdcm
    PATHS ${GDCM_POSSIBLE_LIB_PATHS} 
  )

  FIND_LIBRARY(VTKGDCM_SHARED_LIBRARY
    NAMES vtkgdcm
    PATHS ${GDCM_POSSIBLE_LIB_PATHS} 
  )

  SET (GDCM_POSSIBLE_INCLUDE_PATHS
    /usr/lib
    /usr/local/lib
    $ENV{CREATIS}/gdcm/src
    $ENV{CREATIS}/gdcm/vtk
    "[HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Uninstall\\gdcmPython-py2.2;UninstallString]\\..\\include"
    "C:\\GDCM\\include"
    $ENV{PUB_DICT_PATH}/../src
    $ENV{PUB_DICT_PATH}/../vtk
  )

  FIND_PATH(GDCM_INCLUDE_DIR
    gdcm.h
    ${GDCM_POSSIBLE_INCLUDE_PATHS} 
  )

  FIND_PATH(GDCM_CONFIGURE_INCLUDE_DIR
    gdcmConfigure.h
    ${GDCM_POSSIBLE_INCLUDE_PATHS}/..
  )

  FIND_PATH(VTKGDCM_INCLUDE_DIR
    vtkGdcmReader.h
    ${GDCM_POSSIBLE_INCLUDE_PATHS} 
  )
  #Merge evry path in one single :
  SET(GDCM_INCLUDE_DIR ${GDCM_INCLUDE_DIR} ${VTKGDCM_INCLUDE_DIR} ${GDCM_CONFIGURE_INCLUDE_DIR})
  
  IF(GDCM_SHARED_LIBRARY)
    OPTION(GDCM_USE_SHARED_LIBS 
           "Use shared versions of GDCM libraries" ON)
    MARK_AS_ADVANCED(GDCM_USE_SHARED_LIBS)
  ENDIF(GDCM_SHARED_LIBRARY)

  IF(GDCM_USE_SHARED_LIBS)
    SET(GDCM_LIBRARIES ${VTKGDCM_SHARED_LIBRARY} ${GDCM_SHARED_LIBRARY}
    ${GDCMIJPEG8_SHARED_LIBRARY} ${GDCMIJPEG12_SHARED_LIBRARY} ${GDCMLJPEG_SHARED_LIBRARY})
  ELSE(GDCM_USE_SHARED_LIBS)
    SET(GDCM_LIBRARIES ${VTKGDCM_STATIC_LIBRARY} ${GDCM_STATIC_LIBRARY}
    ${GDCMIJPEG8_STATIC_LIBRARY} ${GDCMIJPEG12_STATIC_LIBRARY} ${GDCMLJPEG_STATIC_LIBRARY})
  ENDIF(GDCM_USE_SHARED_LIBS)

  MARK_AS_ADVANCED(
    GDCM_STATIC_LIBRARY
    GDCM_SHARED_LIBRARY
    GDCM_INCLUDE_DIR
    VTKGDCM_STATIC_LIBRARY
    VTKGDCM_SHARED_LIBRARY
    VTKGDCM_INCLUDE_DIR
  )
  ENDIF(NOT UNIX)

ENDIF(WIN32)

IF(UNIX)

  SET (GDCM_POSSIBLE_LIB_PATHS
    $ENV{CREATIS}/gdcm/lib/
    $ENV{GDCMHOME}/lib
    /usr/local/lib
    /usr/lib
    $ENV{PUB_DICT_PATH}/../lib
  )

  SET (GDCM_POSSIBLE_INCLUDE_PATHS
    $ENV{CREATIS}/gdcm/src
    $ENV{CREATIS}/gdcm/vtk
    $ENV{GDCMHOME}/src
    $ENV{GDCMHOME}/vtk
    /usr/local/include
    /usr/include
    $ENV{PUB_DICT_PATH}/../src
    $ENV{PUB_DICT_PATH}/../vtk
  )

  FIND_PATH(GDCM_INCLUDE_DIR
    gdcm.h
    ${GDCM_POSSIBLE_INCLUDE_PATHS} 
  )

  FIND_PATH(GDCM_INCLUDE_DIR
    vtkGdcmReader.h
    ${GDCM_POSSIBLE_INCLUDE_PATHS} 
  )

  #Merge every path in one single :
  SET(GDCM_INCLUDE_DIR ${GDCM_INCLUDE_DIR} ${VTKGDCM_INCLUDE_DIR})

  FIND_LIBRARY(GDCM_LIBRARIES
    NAMES vtkgdcm gdcm 
    PATHS ${GDCM_POSSIBLE_LIB_PATHS} 
  )

ENDIF(UNIX)  

MARK_AS_ADVANCED(
  CMAKE_GDCM_CXX_FLAGS
  GDCM_INCLUDE_DIR
)

IF(GDCM_LIBRARIES)
  IF(GDCM_INCLUDE_DIR OR CMAKE_GDCM_CXX_FLAGS)
    SET(CMAKE_GDCM_CAN_COMPILE 1)
    SET(GDCM_FOUND 1)
  ENDIF(GDCM_INCLUDE_DIR OR CMAKE_GDCM_CXX_FLAGS)
ENDIF(GDCM_LIBRARIES)

