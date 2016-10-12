project(ITKLevelSetsv4Visualization)

#
# Add the third party includes and libraries
#

# Add compiler flags needed to use VTK. VTK has been found at this point.
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${VTK_REQUIRED_C_FLAGS}")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${VTK_REQUIRED_CXX_FLAGS}")
set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${VTK_REQUIRED_EXE_LINKER_FLAGS}")
set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} ${VTK_REQUIRED_SHARED_LINKER_FLAGS}")
set(CMAKE_MODULE_LINKER_FLAGS "${CMAKE_MODULE_LINKER_FLAGS} ${VTK_REQUIRED_MODULE_LINKER_FLAGS}")

# Older versions of VTK (VTK 5.5 for example) do not have VTK_VERSION, in this
# case it needs to be defined manually
if(NOT VTK_VERSION)
  set(VTK_VERSION "${VTK_MAJOR_VERSION}.${VTK_MINOR_VERSION}.${VTK_BUILD_VERSION}")
endif()
if(NOT VTK_RENDERING_BACKEND)
  set(VTK_RENDERING_BACKEND OpenGL)
endif()
if(TARGET vtkRenderingFreeType${VTK_RENDERING_BACKEND})
  set(_target_freetypeopengl vtkRenderingFreeType${VTK_RENDERING_BACKEND})
endif()

if (${VTK_VERSION} VERSION_LESS ${VERSION_MIN})
  message(ERROR " LevelSetsv4Visualization requires VTK version ${VERSION_MIN} or newer but the current version is ${VTK_VERSION}")
elseif( ${VTK_VERSION} VERSION_LESS 6.0.0 )
  set(ITKLevelSetsv4Visualization_VTK_INCLUDE_DIRS ${VTK_INCLUDE_DIRS})
  set(ITKLevelSetsv4Visualization_VTK_LIBRARIES ${VTK_LIBRARIES})
else()
  vtk_module_config(ITKLevelSetsv4Visualization_VTK
    vtkRenderingAnnotation
    vtkRendering${VTK_RENDERING_BACKEND}
    vtkRenderingFreeType
    ${_target_freetypeopengl}
    vtkInteractionStyle
    vtkIOImage
    vtkImagingSources
    )
endif()

# The VTK DICOMParser and vtkmetaio includes conflict with the ITK
# versions. Here we remove them from the include directories.
#
string(REGEX REPLACE "[^;]*MetaIO;"
         "" ITKLevelSetsv4Visualization_VTK_INCLUDE_DIRS
         "${ITKLevelSetsv4Visualization_VTK_INCLUDE_DIRS}")
string(REGEX REPLACE "[^;]*vtkmetaio;"
         "" ITKLevelSetsv4Visualization_VTK_INCLUDE_DIRS
         "${ITKLevelSetsv4Visualization_VTK_INCLUDE_DIRS}")
string(REGEX REPLACE "[^;]*DICOMParser;"
         "" ITKLevelSetsv4Visualization_VTK_INCLUDE_DIRS
         "${ITKLevelSetsv4Visualization_VTK_INCLUDE_DIRS}")

# Set up the include and library dirs needed for the module
set(ITKLevelSetsv4Visualization_SYSTEM_INCLUDE_DIRS
   ${ITKLevelSetsv4Visualization_VTK_INCLUDE_DIRS})
set(ITKLevelSetsv4Visualization_SYSTEM_LIBRARY_DIRS
  ${ITKLevelSetsv4Visualization_VTK_LIBRARY_DIRS})

set(ITKLevelSetsv4Visualization_LIBRARIES ${ITKLevelSetsv4Visualization_VTK_LIBRARIES}
  ${ITKLevelSetsv4Visualization_VTK_LIBRARIES_EXPLICIT} ITKVtkGlue )

# Add preprocessor definitions needed to use VTK.
set_property(DIRECTORY APPEND PROPERTY COMPILE_DEFINITIONS ${ITKLevelSetsv4Visualization_VTK_DEFINITIONS})

# When this module is loaded by an app, load VTK too.
set(ITKLevelSetsv4Visualization_EXPORT_CODE_INSTALL "
set(VTK_DIR \"${VTK_DIR}\")

find_package(VTK NO_MODULE REQUIRED COMPONENTS vtkCommonCore)

if(NOT VTK_VERSION)
  set(VTK_VERSION \"${VTK_MAJOR_VERSION}.${VTK_MINOR_VERSION}.${VTK_BUILD_VERSION}\")
endif()
if(NOT VTK_RENDERING_BACKEND)
  set(VTK_RENDERING_BACKEND OpenGL)
endif()
if(TARGET vtkRenderingFreeType${VTK_RENDERING_BACKEND})
  set(_target_freetypeopengl vtkRenderingFreeType${VTK_RENDERING_BACKEND})
endif()

if( ${VTK_VERSION} VERSION_LESS 6.0.0 )
  set(ITKLevelSetsv4Visualization_VTK_INCLUDE_DIRS ${VTK_INCLUDE_DIRS})
  set(ITKLevelSetsv4Visualization_VTK_LIBRARIES ${VTK_LIBRARIES})
else()
  vtk_module_config(ITKLevelSetsv4Visualization_VTK
    vtkRenderingAnnotation
    vtkRendering${VTK_RENDERING_BACKEND}
    vtkRenderingFreeType
    ${_target_freetypeopengl}
    vtkInteractionStyle
    vtkIOImage
    vtkImagingSources
   )

  set_property(DIRECTORY APPEND PROPERTY COMPILE_DEFINITIONS \${ITKLevelSetsv4Visualization_VTK_DEFINITIONS})
endif()
")
set(ITKLevelSetsv4Visualization_EXPORT_CODE_BUILD "
if(NOT ITK_BINARY_DIR)
  set(VTK_DIR \"${VTK_DIR}\")

  find_package(VTK NO_MODULE REQUIRED COMPONENTS vtkCommonCore)
  if(NOT VTK_VERSION)
    set(VTK_VERSION \"${VTK_MAJOR_VERSION}.${VTK_MINOR_VERSION}.${VTK_BUILD_VERSION}\")
  endif()
  if(NOT VTK_RENDERING_BACKEND)
    set(VTK_RENDERING_BACKEND OpenGL)
  endif()
  if(TARGET vtkRenderingFreeType${VTK_RENDERING_BACKEND})
    set(_target_freetypeopengl vtkRenderingFreeType${VTK_RENDERING_BACKEND})
  endif()

  if( ${VTK_VERSION} VERSION_LESS 6.0.0 )
    set(ITKLevelSetsv4Visualization_VTK_INCLUDE_DIRS ${VTK_INCLUDE_DIRS})
    set(ITKLevelSetsv4Visualization_VTK_LIBRARIES ${VTK_LIBRARIES})
  else()
    vtk_module_config(ITKLevelSetsv4Visualization_VTK
      vtkRenderingAnnotation
      vtkRendering${VTK_RENDERING_BACKEND}
      vtkRenderingFreeType
      ${_target_freetypeopengl}
      vtkInteractionStyle
      vtkIOImage
      vtkImagingSources
     )

    set_property(DIRECTORY APPEND PROPERTY COMPILE_DEFINITIONS \${ITKLevelSetsv4Visualization_VTK_DEFINITIONS})
  endif()
endif()
")

itk_module_impl()
