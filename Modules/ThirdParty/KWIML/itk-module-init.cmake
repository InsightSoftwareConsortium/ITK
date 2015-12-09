option(ITK_USE_SYSTEM_KWIML "Use an outside build of KWIML." OFF)
mark_as_advanced(ITK_USE_SYSTEM_KWIML)

if(ITK_USE_SYSTEM_KWIML)
  find_package(KWIML 1.0 REQUIRED)
endif()
