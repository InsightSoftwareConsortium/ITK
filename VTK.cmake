FIND_PATH(VTK_INCLUDE_PATH Common/vtkObject.h
c:/vtknew 
c:/VTK 
/VTK 
/vtknew
${PROJECT_SOURCE_DIR}/../VTK 
${PROJECT_SOURCE_DIR}/../vtknew
)

FIND_PATH(VTK_BIN_PATH vtkConfigure.h 
c:/vtkbin 
/usr/local/include
/vtknew-vc 
/VTKBin 
${PROJECT_BINARY_DIR}/../VTKBin 
${PROJECT_BINARY_DIR}/../VTKDll 
${PROJECT_BINARY_DIR}/../vtknew-vc
)

FIND_LIBRARY(VTK_CMAKE_BUILD_LIB_PATH vtkCommon
c:/vtkbin/bin/Debug
c:/vtkbin/bin/Release
/vtknew-vc/bin/Debug
/vtknew-vc/bin/Release
/VTKBin/bin/Debug 
/VTKBin/bin/Release
${VTK_BIN_PATH}/bin/Debug
${VTK_BIN_PATH}/bin/Release
${PROJECT_BINARY_DIR}/../VTKBin/bin/Debug
${PROJECT_BINARY_DIR}/../VTKBin/bin/Release
${PROJECT_BINARY_DIR}/../VTKDll/bin/Debug
${PROJECT_BINARY_DIR}/../VTKDll/bin/Release
${PROJECT_BINARY_DIR}/../vtknew-vc/bin/Debug
${PROJECT_BINARY_DIR}/../vtknew-vc/bin/Release
)

IF(VTK_CMAKE_BUILD_LIB_PATH)
  GET_FILENAME_COMPONENT(VTK_SEARCH_PATH ${VTK_CMAKE_BUILD_LIB_PATH} PATH)
  SET(VTK_LIB_PATH ${VTK_SEARCH_PATH}/.. CACHE PATH "Path to link in VTK")
ENDIF(VTK_CMAKE_BUILD_LIB_PATH)

FIND_LIBRARY(VTK_LIB_PATH 
vtkCommon
/usr/local/lib
)

