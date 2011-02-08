set(itk-template-module_SRC
LIST_OF_CXX_FILES
)

add_library(itk-template-module ${itk-template-module_SRC})
target_link_libraries(itk-template-module @DEPEND_MODULE_LIBRARIES@)
itk_module_target(itk-template-module)
