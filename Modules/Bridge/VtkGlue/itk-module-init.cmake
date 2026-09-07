#
# Find the packages required by this module
#

# Required VTK version. ITK 6 requires VTK 9.1 or newer because:
#   * VTK 9.0 was the first stable release of the new module system, which
#     replaces the legacy `vtk_module_config` macro with imported targets
#     (e.g. VTK::CommonCore).
#   * VTK 9.0 ships only the OpenGL2 rendering backend.
#   * VTK 9.1 stabilised the imported-target names and the
#     `vtk_module_autoinit` helper that ITKVtkGlue relies on
#     unconditionally.
# VTK 9.4+ is recommended (current security/CVE patches and bug fixes)
# but not required.
set(VERSION_MIN "9.1.0")

find_package(VTK ${VERSION_MIN} NO_MODULE REQUIRED)

# VTK 9 omits VTK_RENDERING_BACKEND; infer rendering support from its OpenGL2 target.
if(NOT VTK_RENDERING_BACKEND)
  if(TARGET VTK::RenderingOpenGL2)
    set(VTK_RENDERING_BACKEND OpenGL2)
  else()
    set(VTK_RENDERING_BACKEND None)
  endif()
endif()

set(_target_freetypeopengl)
if(TARGET VTK::RenderingFreeType${VTK_RENDERING_BACKEND})
  set(_target_freetypeopengl VTK::RenderingFreeType${VTK_RENDERING_BACKEND})
endif()

set(
  _required_vtk_libraries
  VTK::IOImage
  VTK::ImagingSources
  VTK::vtksys
  VTK::kwiml
)

if(ITK_WRAP_PYTHON)
  list(
    APPEND
    _required_vtk_libraries
    VTK::CommonCore
    VTK::CommonDataModel
    VTK::CommonExecutionModel
  )
endif()
if(NOT VTK_RENDERING_BACKEND STREQUAL "None")
  list(
    APPEND
    _required_vtk_libraries
    VTK::Rendering${VTK_RENDERING_BACKEND}
    VTK::RenderingFreeType
    ${_target_freetypeopengl}
    VTK::InteractionStyle
    VTK::InteractionWidgets
  )
endif()

if(ITK_WRAP_PYTHON)
  if(NOT VTK_WRAP_PYTHON)
    message(
      FATAL_ERROR
      "ITK_WRAP_PYTHON is ON and Module_ITKVtkGlue is enabled, but the VTK at\n"
      "  ${VTK_DIR}\n"
      "was built with VTK_WRAP_PYTHON=OFF. Rebuild VTK with VTK_WRAP_PYTHON=ON,\n"
      "or set Module_ITKVtkGlue=OFF."
    )
  endif()
  # VTK exports no shared/static variable, so ask an imported target directly.
  if(TARGET VTK::CommonCore)
    get_target_property(_vtk_common_core_type VTK::CommonCore TYPE)
    if(_vtk_common_core_type STREQUAL "STATIC_LIBRARY")
      message(
        FATAL_ERROR
        "ITK_WRAP_PYTHON is ON and Module_ITKVtkGlue is enabled, but the VTK at\n"
        "  ${VTK_DIR}\n"
        "is a static build. Its Python wrapping collapses into a single\n"
        "_vtkmodules_static module that ITK's wrapping cannot import. Rebuild VTK\n"
        "with BUILD_SHARED_LIBS=ON, or set Module_ITKVtkGlue=OFF."
      )
    endif()
    unset(_vtk_common_core_type)
  endif()
endif()

set(ITKVtkGlue_VTK_LIBRARIES ${_required_vtk_libraries})
