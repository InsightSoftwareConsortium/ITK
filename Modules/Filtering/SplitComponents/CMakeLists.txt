project(SplitComponents)

if(POLICY CMP135)
  cmake_policy(SET CMP135 NEW)
endif()

# set(SplitComponents_LIBRARIES SplitComponents) # header-only module, so no libraries

itk_module_impl()
