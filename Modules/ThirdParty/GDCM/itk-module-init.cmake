option(ITK_USE_SYSTEM_GDCM "Use an outside build of GDCM." OFF)
mark_as_advanced(ITK_USE_SYSTEM_GDCM)

if(ITK_USE_SYSTEM_GDCM)
  find_package(GDCM REQUIRED)
  # Avoid UseGDCM.cmake calling include(${VTK_USE_FILE}), which include's
  # VTK's include paths before ITK and causes a MetaIO conflict.
  set(GDCM_USE_VTK FALSE)
  include(${GDCM_USE_FILE})
endif()
