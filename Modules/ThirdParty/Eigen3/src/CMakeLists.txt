set(ITK3P_INSTALL_EXPORT_NAME "${ITKEigen3-targets}")
set(ITK3P_INSTALL_INCLUDE_DIR "${ITKEigen3_INSTALL_INCLUDE_DIR}")
set(ITK3P_INSTALL_RUNTIME_DIR "${ITKEigen3_INSTALL_RUNTIME_DIR}")

# Install Eigen3Config.cmake alongside other ITK module files,
# matching the path expected by ITKInternalEigen3_DIR.
set(ITK3P_INSTALL_DATADIR "${ITK_INSTALL_PACKAGE_DIR}/Modules")

add_subdirectory(itkeigen)
