#
#  This project is intended to be 
#  built outside the Insight source tree
#


PROJECT(HelloWorld)


INCLUDE (${CMAKE_ROOT}/Modules/FindITK.cmake)
IF (USE_ITK_FILE)
  INCLUDE(${USE_ITK_FILE})
ENDIF(USE_ITK_FILE)


ADD_EXECUTABLE(HelloWorld HelloWorld.cxx )


TARGET_LINK_LIBRARIES(HelloWorld ITKCommon)


