if(COMMAND CMAKE_POLICY)
  cmake_policy(SET CMP0003 NEW)
endif(COMMAND CMAKE_POLICY)

set(PCA PrincipalComponentsAnalysis)
project(${PCA})

itk_module_impl()

itk_module_examples()

cmake_dependent_option(
  Module_${PCA}_BUILD_DOCUMENTATION
  "Generate documentation from LaTeX files, source code examples and result screenshots"
  OFF
  "BUILD_DOCUMENTATION"
  OFF
)
if(Module_${PCA}_BUILD_DOCUMENTATION)
  add_subdirectory(doc)
endif()
