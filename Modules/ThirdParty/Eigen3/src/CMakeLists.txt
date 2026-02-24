set(ITK3P_INSTALL_EXPORT_NAME "${ITKEigen3-targets}")
set(ITK3P_INSTALL_INCLUDE_DIR "${ITKEigen3_INSTALL_INCLUDE_DIR}")
set(ITK3P_INSTALL_RUNTIME_DIR "${ITKEigen3_INSTALL_RUNTIME_DIR}")

add_subdirectory(itkeigen)
itk_module_target(eigen_internal NO_INSTALL)
