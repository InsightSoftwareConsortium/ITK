#
# Find the packages required by this module
#

# Needed VTK version
set(VERSION_MIN "5.10.0")

# Look for VTK
find_package(VTK NO_MODULE REQUIRED)

if(NOT COMMAND vtk_module_config)
  macro(vtk_module_config ns)
     foreach(arg ${ARGN})
      if(${arg} MATCHES "^[Vv][Tt][Kk]")
        string(REGEX REPLACE "^[Vv][Tt][Kk]" "" _arg ${arg})
      else()
        set(_arg ${arg})
      endif()
      set(${ns}_LIBRARIES ${${ns}_LIBRARIES} VTK::${_arg})
     endforeach()
  endmacro()

  if(NOT VTK_RENDERING_BACKEND)
    set(VTK_RENDERING_BACKEND OpenGL2)
  endif()
endif()

# Older versions of VTK (VTK 5.5 for example) do not have VTK_VERSION, in this
# case it needs to be defined manually
if(NOT VTK_VERSION)
  set(VTK_VERSION "${VTK_MAJOR_VERSION}.${VTK_MINOR_VERSION}.${VTK_BUILD_VERSION}")
endif()
if(NOT VTK_RENDERING_BACKEND)
  if(NOT COMMAND vtk_module_config)
    set(VTK_RENDERING_BACKEND OpenGL2)
  else()
    set(VTK_RENDERING_BACKEND OpenGL)
  endif()
endif()
set(_target_prefix "vtk")
if(VTK_VERSION VERSION_GREATER_EQUAL 8.90.0)
  set(_target_prefix "VTK::")
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
  list(APPEND _required_vtk_libraries ${_target_prefix}WrappingPythonCore
    ${_target_prefix}CommonCore ${_target_prefix}CommonDataModel ${_target_prefix}kwiml ${_target_prefix}CommonExecutionModel)
endif()
if(NOT VTK_RENDERING_BACKEND STREQUAL "None")
  list(APPEND _required_vtk_libraries
    ${_target_prefix}Rendering${VTK_RENDERING_BACKEND}
    ${_target_prefix}RenderingFreeType
    ${_target_freetypeopengl}
    ${_target_prefix}InteractionStyle
    ${_target_prefix}InteractionWidgets
  )
endif()
if (${VTK_VERSION} VERSION_LESS ${VERSION_MIN})
  message(ERROR " VtkGlue requires VTK version ${VERSION_MIN} or newer but the current version is ${VTK_VERSION}")
elseif( ${VTK_VERSION} VERSION_LESS 6.0.0 )
  set(ITKVtkGlue_VTK_INCLUDE_DIRS ${VTK_INCLUDE_DIRS})
  set(ITKVtkGlue_VTK_LIBRARIES ${VTK_LIBRARIES})
else()
  vtk_module_config(ITKVtkGlue_VTK
    ${_required_vtk_libraries}
    )
  set(ITKVtkGlue_VTK_LIBRARIES ${_required_vtk_libraries})
endif()
