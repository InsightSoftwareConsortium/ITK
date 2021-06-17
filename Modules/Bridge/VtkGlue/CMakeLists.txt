project(ITKVtkGlue)
if(NOT VTK_RENDERING_BACKEND STREQUAL "None")
  set(ITKVtkGlue_LIBRARIES ITKVtkGlue)
endif()

#
# Add the third party includes and libraries
#

# Add compiler flags needed to use VTK. VTK has been found at this point.
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${VTK_REQUIRED_C_FLAGS}")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${VTK_REQUIRED_CXX_FLAGS}")
set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${VTK_REQUIRED_EXE_LINKER_FLAGS}")
set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} ${VTK_REQUIRED_SHARED_LINKER_FLAGS}")
set(CMAKE_MODULE_LINKER_FLAGS "${CMAKE_MODULE_LINKER_FLAGS} ${VTK_REQUIRED_MODULE_LINKER_FLAGS}")

# The VTK DICOMParser and vtkmetaio includes conflict with the ITK
# versions. Here we remove them from the include directories.
#
string(REGEX REPLACE "[^;]*MetaIO;"
         "" ITKVtkGlue_VTK_INCLUDE_DIRS "${ITKVtkGlue_VTK_INCLUDE_DIRS}")
string(REGEX REPLACE "[^;]*vtkmetaio;"
         "" ITKVtkGlue_VTK_INCLUDE_DIRS "${ITKVtkGlue_VTK_INCLUDE_DIRS}")
string(REGEX REPLACE "[^;]*DICOMParser;"
         "" ITKVtkGlue_VTK_INCLUDE_DIRS "${ITKVtkGlue_VTK_INCLUDE_DIRS}")

# Set up the include and library dirs needed for the module
set(ITKVtkGlue_SYSTEM_INCLUDE_DIRS ${ITKVtkGlue_VTK_INCLUDE_DIRS})
set(ITKVtkGlue_SYSTEM_LIBRARY_DIRS ${ITKVtkGlue_VTK_LIBRARY_DIRS})

# Add preprocessor definitions needed to use VTK.
set_property(DIRECTORY APPEND PROPERTY COMPILE_DEFINITIONS ${ITKVtkGlue_VTK_DEFINITIONS})

set(ITKVtkGlue_EXPORT_CODE_INSTALL "
set(VTK_DIR \"${VTK_DIR}\")

find_package(VTK NO_MODULE REQUIRED)

  if(NOT COMMAND vtk_module_config)
    macro(vtk_module_config ns)
       foreach(arg \${ARGN})
        if(\${arg} MATCHES \"^[Vv][Tt][Kk]\")
          string(REGEX REPLACE \"^[Vv][Tt][Kk]\" \"\" _arg \${arg})
        else()
          set(_arg \${arg})
        endif()
        set(\${ns}_LIBRARIES \${\${ns}_LIBRARIES} VTK::\${_arg})
       endforeach()
    endmacro()
  endif()

if(NOT VTK_VERSION)
  set(VTK_VERSION \"${VTK_MAJOR_VERSION}.${VTK_MINOR_VERSION}.${VTK_BUILD_VERSION}\")
endif()
if(NOT VTK_RENDERING_BACKEND)
  if(NOT COMMAND vtk_module_config)
    set(VTK_RENDERING_BACKEND OpenGL2)
  else()
    set(VTK_RENDERING_BACKEND OpenGL)
  endif()
endif()
set(_target_freetypeopengl)
if(TARGET ${_target_prefix}RenderingFreeType${VTK_RENDERING_BACKEND})
  set(_target_freetypeopengl ${_target_prefix}RenderingFreeType${VTK_RENDERING_BACKEND})
endif()

set(_required_vtk_libraries
  ${_target_prefix}IOImage
  ${_target_prefix}ImagingSources
  )
if(ITK_WRAP_PYTHON)
  list(APPEND _required_vtk_libraries ${_target_prefix}WrappingPythonCore)
endif()
if(NOT VTK_RENDERING_BACKEND STREQUAL \"None\")
  list(APPEND _required_vtk_libraries
    ${_target_prefix}Rendering${VTK_RENDERING_BACKEND}
    ${_target_prefix}RenderingFreeType
    ${_target_freetypeopengl}
    ${_target_prefix}InteractionStyle
    ${_target_prefix}InteractionWidgets
  )
endif()
vtk_module_config(ITKVtkGlue_VTK
  \${_required_vtk_libraries}
  )
set(ITKVtkGlue_VTK_LIBRARIES \${_required_vtk_libraries})
set_property(DIRECTORY APPEND PROPERTY COMPILE_DEFINITIONS \${ITKVtkGlue_VTK_DEFINITIONS})
")
set(ITKVtkGlue_EXPORT_CODE_BUILD "
if(NOT ITK_BINARY_DIR)
  set(VTK_DIR \"${VTK_DIR}\")

  if(NOT COMMAND vtk_module_config)
    macro(vtk_module_config ns)
       foreach(arg \${ARGN})
        if(\${arg} MATCHES \"^[Vv][Tt][Kk]\")
          string(REGEX REPLACE \"^[Vv][Tt][Kk]\" \"\" _arg \${arg})
        else()
          set(_arg \${arg})
        endif()
        set(\${ns}_LIBRARIES \${\${ns}_LIBRARIES} VTK::\${_arg})
       endforeach()
    endmacro()
  endif()

  find_package(VTK NO_MODULE REQUIRED)

  if(NOT VTK_VERSION)
    set(VTK_VERSION \"${VTK_MAJOR_VERSION}.${VTK_MINOR_VERSION}.${VTK_BUILD_VERSION}\")
  endif()
  if(NOT VTK_RENDERING_BACKEND)
    if(NOT COMMAND vtk_module_config)
      set(VTK_RENDERING_BACKEND OpenGL2)
    else()
      set(VTK_RENDERING_BACKEND OpenGL)
    endif()
  endif()
  if(TARGET ${_target_prefix}RenderingFreeType${VTK_RENDERING_BACKEND})
    set(_target_freetypeopengl ${_target_prefix}RenderingFreeType${VTK_RENDERING_BACKEND})
  endif()

  set(_required_vtk_libraries
    ${_target_prefix}IOImage
    ${_target_prefix}ImagingSources
    )
  if(ITK_WRAP_PYTHON)
    list(APPEND _required_vtk_libraries ${_target_prefix}WrappingPythonCore)
  endif()
  if(NOT VTK_RENDERING_BACKEND STREQUAL \"None\")
    list(APPEND _required_vtk_libraries
      ${_target_prefix}Rendering${VTK_RENDERING_BACKEND}
      ${_target_prefix}RenderingFreeType
      ${_target_freetypeopengl}
      ${_target_prefix}InteractionStyle
      ${_target_prefix}InteractionWidgets
    )
  endif()
  vtk_module_config(ITKVtkGlue_VTK
    \${_required_vtk_libraries}
    )
  set(ITKVtkGlue_VTK_LIBRARIES \${_required_vtk_libraries})
  set_property(DIRECTORY APPEND PROPERTY COMPILE_DEFINITIONS \${ITKVtkGlue_VTK_DEFINITIONS})
endif()
")

if(VTK_RENDERING_BACKEND STREQUAL "None")
  set(ITKVtkGlue_LIBRARIES ${ITKVtkGlue_VTK_LIBRARIES})
else()
  set(ITKVtkGlue_LIBRARIES ITKVtkGlue)
endif()

itk_module_impl()
