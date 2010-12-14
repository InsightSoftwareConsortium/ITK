set(MODULE_TEST itk-module-name)
create_test_sourcelist(Tests ${MODULE_TEST}CxxTest.cxx
LIST_OF_CXX_FILES
)
add_executable(${MODULE_TEST}CxxTest ${Tests})

target_link_libraries(${MODULE_TEST}CxxTest itk-module-name)

include(itk-module-name-tests.cmake)
