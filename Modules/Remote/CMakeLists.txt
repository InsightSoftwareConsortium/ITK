# Functions to fetch remote modules.
include(ITKModuleRemote)

set(ITK_MINIMUM_COMPLIANCE_LEVEL "3" CACHE STRING "Minimum compliance level [5,4,3,2,1,0] for selecting ITK modules.")
set_property(CACHE PROPERTY STRINGS ITK_MINIMUM_COMPLIANCE_LEVEL "5" "4" "3" "2" "1" "0")
mark_as_advanced(CLEAR ITK_MINIMUM_COMPLIANCE_LEVEL)

file(GLOB remotes "*.remote.cmake")
foreach(remote_module ${remotes})
  include(${remote_module})
endforeach()

# Remote modules that are deprecated are moved to
# a separate subdirectory to assist with distinguishing
# different remote module types.
if(Module_ITKDeprecated)
  file(GLOB remotes "Deprecated/*.remote.cmake")
  foreach(remote_module ${remotes})
    include(${remote_module})
  endforeach()
endif()
