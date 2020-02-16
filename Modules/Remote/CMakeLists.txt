# Functions to fetch remote modules.
include(ITKModuleRemote)

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
