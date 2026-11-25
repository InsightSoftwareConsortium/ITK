itk_wrap_module(RANSAC)
itk_auto_load_submodules()
itk_end_wrap_module()

# The itk package runs __init_ransac__.py after binding the itk.RANSAC submodule.
configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/__init_ransac__.py"
  "${ITK_PYTHON_PACKAGE_DIR}/__init_ransac__.py"
  COPYONLY
)
wrap_itk_python_bindings_install(/itk RANSAC "${ITK_PYTHON_PACKAGE_DIR}/__init_ransac__.py")
