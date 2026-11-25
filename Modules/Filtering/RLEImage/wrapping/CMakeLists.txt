itk_wrap_module(RLEImage)

set(
  WRAPPER_SUBMODULE_ORDER
  itkRLEImage
  itkRLERegionOfInterestImageFilter
)

itk_auto_load_submodules()
itk_end_wrap_module()

# The itk package runs __init_rleimage__.py after binding the itk.RLEImage submodule.
configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/__init_rleimage__.py"
  "${ITK_PYTHON_PACKAGE_DIR}/__init_rleimage__.py"
  COPYONLY
)
wrap_itk_python_bindings_install(/itk RLEImage "${ITK_PYTHON_PACKAGE_DIR}/__init_rleimage__.py")
