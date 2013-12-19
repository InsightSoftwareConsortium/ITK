# This is the root ITK CMakeLists file.
CMAKE_MINIMUM_REQUIRED(VERSION 2.6)
IF(COMMAND CMAKE_POLICY)
  CMAKE_POLICY(SET CMP0003 NEW)
ENDIF(COMMAND CMAKE_POLICY)


# This project is designed to be built outside the Insight source tree.
PROJECT( PhaseSymmetryImageFilter)

# Find ITK.
FIND_PACKAGE(ITK REQUIRED)
INCLUDE(${ITK_USE_FILE})



ADD_EXECUTABLE( PhaseSymmetryImageFilter2D main2.cxx)
ADD_EXECUTABLE( PhaseSymmetryImageFilter3D main3.cxx)

TARGET_LINK_LIBRARIES( PhaseSymmetryImageFilter3D ${ITK_LIBRARIES} )
TARGET_LINK_LIBRARIES( PhaseSymmetryImageFilter2D ${ITK_LIBRARIES} )
