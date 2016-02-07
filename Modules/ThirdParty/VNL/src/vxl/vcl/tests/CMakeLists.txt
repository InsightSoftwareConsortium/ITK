# vcl/tests/CMakeLists.txt

# Avoid adding more test executables here, in order to keep the number
# of projects small. (This is a concern for IDEs like Visual Studio.)
# Try to follow one of the other tests, like test_algorithm.cxx,
# and add the test into test_driver.cxx.

add_executable( vcl_test_all
 # Driver
     test_driver.cxx
 # The actual tests
     test_algorithm.cxx
     test_cctype.cxx
     test_cmath.cxx
     test_compiler.cxx
     test_complex.cxx
     test_deque.cxx
     test_exception.cxx
     test_fstream.cxx
     test_iostream.cxx
     test_iterator.cxx
     test_list.cxx
     test_limits.cxx
     test_map.cxx
     test_memory.cxx
     test_multimap.cxx
     test_new.cxx
     test_set.cxx
     test_stlfwd.cxx
     test_string.cxx
     test_sstream.cxx
     test_vector.cxx
     test_cstdio.cxx
     test_preprocessor.cxx
     test_atomic_count.cxx
     test_typename.cxx
)
target_link_libraries( vcl_test_all ${VXL_LIB_PREFIX}vcl )

add_test( NAME vcl_test_algorithm COMMAND $<TARGET_FILE:vcl_test_all> test_algorithm )
add_test( NAME vcl_test_cctype COMMAND $<TARGET_FILE:vcl_test_all> test_cctype )
add_test( NAME vcl_test_cmath COMMAND $<TARGET_FILE:vcl_test_all> test_cmath )
add_test( NAME vcl_test_compiler COMMAND $<TARGET_FILE:vcl_test_all> test_compiler )
add_test( NAME vcl_test_complex COMMAND $<TARGET_FILE:vcl_test_all> test_complex )
add_test( NAME vcl_test_deque COMMAND $<TARGET_FILE:vcl_test_all> test_deque )
add_test( NAME vcl_test_exception COMMAND $<TARGET_FILE:vcl_test_all> test_exception )
add_test( NAME vcl_test_fstream COMMAND $<TARGET_FILE:vcl_test_all> test_fstream )
add_test( NAME vcl_test_iostream COMMAND $<TARGET_FILE:vcl_test_all> test_iostream )
add_test( NAME vcl_test_iterator COMMAND $<TARGET_FILE:vcl_test_all> test_iterator )
add_test( NAME vcl_test_list COMMAND $<TARGET_FILE:vcl_test_all> test_list )
add_test( NAME vcl_test_limits COMMAND $<TARGET_FILE:vcl_test_all> test_limits )
add_test( NAME vcl_test_map COMMAND $<TARGET_FILE:vcl_test_all> test_map )
add_test( NAME vcl_test_memory COMMAND $<TARGET_FILE:vcl_test_all> test_memory )
add_test( NAME vcl_test_multimap COMMAND $<TARGET_FILE:vcl_test_all> test_multimap )
add_test( NAME vcl_test_new COMMAND $<TARGET_FILE:vcl_test_all> test_new )
add_test( NAME vcl_test_set COMMAND $<TARGET_FILE:vcl_test_all> test_set )
add_test( NAME vcl_test_string COMMAND $<TARGET_FILE:vcl_test_all> test_string )
add_test( NAME vcl_test_sstream COMMAND $<TARGET_FILE:vcl_test_all> test_sstream )
add_test( NAME vcl_test_vector COMMAND $<TARGET_FILE:vcl_test_all> test_vector )
add_test( NAME vcl_test_cstdio COMMAND $<TARGET_FILE:vcl_test_all> test_cstdio ${CMAKE_CURRENT_LIST_DIR}/test_cstdio.txt )
add_test( NAME vcl_test_preprocessor COMMAND $<TARGET_FILE:vcl_test_all> test_preprocessor )
add_test( NAME vcl_test_atomic_count COMMAND $<TARGET_FILE:vcl_test_all> test_atomic_count )
# Only a compiler test:
#add_test( NAME vcl_test_stlfwd COMMAND $<TARGET_FILE:vcl_test_all> test_stlfwd )
#add_test( NAME vcl_test_typename COMMAND $<TARGET_FILE:vcl_test_all> test_typename )

add_executable( vcl_test_include test_include.cxx )
target_link_libraries( vcl_test_include ${VXL_LIB_PREFIX}vcl )
