# Modules/Beta/
#
# In-tree home for modules that were formerly configure-time remote
# fetches (Modules/Remote/*.remote.cmake). Each module lives at
# Modules/Beta/<Name>/ with its own itk-module.cmake and is discovered
# automatically by the normal ITK module DAG scan (see
# CMake/ITKModuleEnablement.cmake).
#
# Alongside each ingested module, an optional manifest
# Modules/Beta/<Name>.beta.cmake records upstream provenance
# (UPSTREAM_URL, UPSTREAM_SHA at last ingest, INGEST_DATE, license,
# compliance level). The manifest files are inert at build time --
# they are passive metadata consumed by tooling and human readers.

# itk_beta_module_manifest() is a no-op that makes the *.beta.cmake
# files valid CMake so they can be include()-d by tooling without
# producing build-time side effects.
function(itk_beta_module_manifest _name)
  # arguments are intentionally discarded; parsed by external tooling.
endfunction()

# Include every *.beta.cmake so CMake at least syntactically validates
# them at configure time. This does not enable, build, or fetch anything.
# CONFIGURE_DEPENDS asks CMake to re-glob whenever a manifest is added,
# removed, or renamed, so a Beta-module ingest does not require a manual
# `cmake .` reconfigure.
file(GLOB _beta_manifests CONFIGURE_DEPENDS "${CMAKE_CURRENT_SOURCE_DIR}/*.beta.cmake")
foreach(_manifest ${_beta_manifests})
  include(${_manifest})
endforeach()
unset(_beta_manifests)
unset(_manifest)
