option(ITK_USE_SYSTEM_GDCM "Use an outside build of GDCM." OFF)
mark_as_advanced(ITK_USE_SYSTEM_GDCM)

if(ITK_USE_SYSTEM_GDCM)
  find_package(GDCM REQUIRED)
  include(${GDCM_USE_FILE})
endif()
