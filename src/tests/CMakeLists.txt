if (NOT ITK_TEST_OUTPUT_DIR) # we are not part of ITK source tree
  set(ITK_TEST_OUTPUT_DIR ${CMAKE_BINARY_DIR}/Testing/Temporary/)
  if(NOT EXISTS ${ITK_TEST_OUTPUT_DIR})
    file(MAKE_DIRECTORY ${ITK_TEST_OUTPUT_DIR})
    message(STATUS "Created temporary test folder:\n  ${ITK_TEST_OUTPUT_DIR}")
  endif()
endif()

function(MetaAddTest ExeName)
  ADD_EXECUTABLE( ${ExeName} ${ExeName}.cxx )
  TARGET_LINK_LIBRARIES( ${ExeName} ${METAIO_NAMESPACE} ${ZLIB_LIBRARIES})
  ADD_TEST(NAME ${ExeName}
           COMMAND ${ExeName}
           WORKING_DIRECTORY ${ITK_TEST_OUTPUT_DIR})
endfunction()

MetaAddTest(testMeta1Utils)
MetaAddTest(testMeta2Object)
MetaAddTest(testMeta3Image)
MetaAddTest(testMeta4Tube)
MetaAddTest(testMeta5Blob)
MetaAddTest(testMeta6Surface)
MetaAddTest(testMeta7Line)
MetaAddTest(testMeta8Scene)
MetaAddTest(testMeta9Landmark)
MetaAddTest(testMeta10Contour)
MetaAddTest(testMeta11Form)
MetaAddTest(testMeta12Array)
