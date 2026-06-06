option(ITK_USE_SYSTEM_proxTV "Use an outside build of proxTV." OFF)
mark_as_advanced(ITK_USE_SYSTEM_proxTV)

if(ITK_USE_SYSTEM_proxTV)
  find_package(proxTV REQUIRED CONFIG)
endif()
