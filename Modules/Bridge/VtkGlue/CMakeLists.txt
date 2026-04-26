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
set(
  CMAKE_EXE_LINKER_FLAGS
  "${CMAKE_EXE_LINKER_FLAGS} ${VTK_REQUIRED_EXE_LINKER_FLAGS}"
)
set(
  CMAKE_SHARED_LINKER_FLAGS
  "${CMAKE_SHARED_LINKER_FLAGS} ${VTK_REQUIRED_SHARED_LINKER_FLAGS}"
)
set(
  CMAKE_MODULE_LINKER_FLAGS
  "${CMAKE_MODULE_LINKER_FLAGS} ${VTK_REQUIRED_MODULE_LINKER_FLAGS}"
)

# The VTK DICOMParser and vtkmetaio includes conflict with the ITK
# versions. Here we remove them from the include directories.
#
string(
  REGEX
  REPLACE
  "[^;]*MetaIO;"
  ""
  ITKVtkGlue_VTK_INCLUDE_DIRS
  "${ITKVtkGlue_VTK_INCLUDE_DIRS}"
)
string(
  REGEX
  REPLACE
  "[^;]*vtkmetaio;"
  ""
  ITKVtkGlue_VTK_INCLUDE_DIRS
  "${ITKVtkGlue_VTK_INCLUDE_DIRS}"
)
string(
  REGEX
  REPLACE
  "[^;]*DICOMParser;"
  ""
  ITKVtkGlue_VTK_INCLUDE_DIRS
  "${ITKVtkGlue_VTK_INCLUDE_DIRS}"
)

# Set up the include and library dirs needed for the module
set(ITKVtkGlue_SYSTEM_INCLUDE_DIRS ${ITKVtkGlue_VTK_INCLUDE_DIRS})
set(ITKVtkGlue_SYSTEM_LIBRARY_DIRS ${ITKVtkGlue_VTK_LIBRARY_DIRS})

# Add preprocessor definitions needed to use VTK.
set_property(
  DIRECTORY
  APPEND
  PROPERTY
    COMPILE_DEFINITIONS
      ${ITKVtkGlue_VTK_DEFINITIONS}
)

# Export blocks emitted into the consumer's ITKConfig.cmake. Both the
# install-tree and build-tree variants assume VTK >= 9.1 (imported
# targets, OpenGL2 only, no `vtk_module_config` shim).
#
# Note on `\$` escapes inside the quoted string bodies below:
# variables like `_target_freetypeopengl` and `_required_vtk_libraries`
# are *defined inside* these string bodies and must therefore expand
# at the consumer's `find_package(ITK)` time, not at ITK's configure
# time. The `\$` keeps the literal `${...}` in the generated config
# file. `\${VTK_RENDERING_BACKEND}` is escaped on the same timeline
# for internal consistency (its default-value `if`-block also runs at
# consumer time). Mixing escaped and unescaped references is a latent
# bug — the unescaped variant expands at ITK time when these locals
# are still empty, silently dropping content from the consumer's
# config (see PR #6144 for the empirical reproduction).
set(
  ITKVtkGlue_EXPORT_CODE_INSTALL
  "
set(VTK_DIR \"${VTK_DIR}\")
find_package(VTK 9.1 NO_MODULE REQUIRED)
if(NOT VTK_RENDERING_BACKEND)
  set(VTK_RENDERING_BACKEND OpenGL2)
endif()
set(_target_freetypeopengl)
if(TARGET VTK::RenderingFreeType\${VTK_RENDERING_BACKEND})
  set(_target_freetypeopengl VTK::RenderingFreeType\${VTK_RENDERING_BACKEND})
endif()
set(_required_vtk_libraries
  VTK::IOImage
  VTK::ImagingSources
  VTK::vtksys
  VTK::kwiml
  )
if(ITK_WRAP_PYTHON)
  list(APPEND _required_vtk_libraries
    VTK::WrappingPythonCore
    VTK::CommonCore
    VTK::CommonDataModel
    VTK::CommonExecutionModel)
endif()
if(NOT VTK_RENDERING_BACKEND STREQUAL \"None\")
  list(APPEND _required_vtk_libraries
    VTK::Rendering\${VTK_RENDERING_BACKEND}
    VTK::RenderingFreeType
    \${_target_freetypeopengl}
    VTK::InteractionStyle
    VTK::InteractionWidgets
  )
endif()
set(ITKVtkGlue_VTK_LIBRARIES \${_required_vtk_libraries})
set_property(DIRECTORY APPEND PROPERTY COMPILE_DEFINITIONS \${ITKVtkGlue_VTK_DEFINITIONS})
"
)
set(
  ITKVtkGlue_EXPORT_CODE_BUILD
  "
if(NOT ITK_BINARY_DIR)
  set(VTK_DIR \"${VTK_DIR}\")
  find_package(VTK 9.1 NO_MODULE REQUIRED)
  if(NOT VTK_RENDERING_BACKEND)
    set(VTK_RENDERING_BACKEND OpenGL2)
  endif()
  set(_target_freetypeopengl)
  if(TARGET VTK::RenderingFreeType\${VTK_RENDERING_BACKEND})
    set(_target_freetypeopengl VTK::RenderingFreeType\${VTK_RENDERING_BACKEND})
  endif()
  set(_required_vtk_libraries
    VTK::IOImage
    VTK::ImagingSources
    VTK::vtksys
    VTK::kwiml
    )
  if(ITK_WRAP_PYTHON)
    list(APPEND _required_vtk_libraries
      VTK::WrappingPythonCore
      VTK::CommonCore
      VTK::CommonDataModel
      VTK::CommonExecutionModel)
  endif()
  if(NOT VTK_RENDERING_BACKEND STREQUAL \"None\")
    list(APPEND _required_vtk_libraries
      VTK::Rendering\${VTK_RENDERING_BACKEND}
      VTK::RenderingFreeType
      \${_target_freetypeopengl}
      VTK::InteractionStyle
      VTK::InteractionWidgets
    )
  endif()
  set(ITKVtkGlue_VTK_LIBRARIES \${_required_vtk_libraries})
  set_property(DIRECTORY APPEND PROPERTY COMPILE_DEFINITIONS \${ITKVtkGlue_VTK_DEFINITIONS})
endif()
"
)

if(VTK_RENDERING_BACKEND STREQUAL "None")
  set(ITKVtkGlue_LIBRARIES ${ITKVtkGlue_VTK_LIBRARIES})
else()
  set(ITKVtkGlue_LIBRARIES ITKVtkGlue)
endif()

itk_module_impl()
