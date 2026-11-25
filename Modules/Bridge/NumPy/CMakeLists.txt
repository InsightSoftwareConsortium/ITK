project(ITKBridgeNumPy)

set(
  ITKBridgeNumPy_SYSTEM_GENEX_INCLUDE_DIRS
  "$<BUILD_INTERFACE:${Python3_INCLUDE_DIRS}>"
)

itk_module_impl()
