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

if(NOT VTK_RENDERING_BACKEND)
  set(VTK_RENDERING_BACKEND OpenGL2)
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
    VTK::WrappingPythonCore
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

set(ITKVtkGlue_VTK_LIBRARIES ${_required_vtk_libraries})
