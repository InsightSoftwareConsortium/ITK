cmake_minimum_required(VERSION 2.8)

project( ANALYZE_OBJECT_MAP )

# Add the testing directories
option(ANALYZE_OBJECT_MAP_BUILD_TESTING "Test" ON)
if(ANALYZE_OBJECT_MAP_BUILD_TESTING)
  enable_testing()
endif(ANALYZE_OBJECT_MAP_BUILD_TESTING)

set(CXX_TEST_PATH ${EXECUTABLE_OUTPUT_PATH})


#  FIND ITK
find_package(ITK)
if(USE_ITK_FILE)
  include(${USE_ITK_FILE})
else(USE_ITK_FILE)
  message( FATAL_ERROR "This application requires ITK. One of these components is missing. Please verify configuration")
endif(USE_ITK_FILE)


subdirs(Source)

# Add the testing directories
if(ANALYZE_OBJECT_MAP_BUILD_TESTING)
   subdirs(Test)
endif(ANALYZE_OBJECT_MAP_BUILD_TESTING)

# Add the examples directories
option(ANALYZE_OBJECT_MAP_BUILD_EXAMPLES "Examples" ON)
if(ANALYZE_OBJECT_MAP_BUILD_EXAMPLES)
  subdirs(Examples)
endif(ANALYZE_OBJECT_MAP_BUILD_EXAMPLES)
