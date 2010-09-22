
# configure the test driver
find_program(ITK_TEST_DRIVER itkTestDriver)

if(WIN32)
  set(JAVA_DRIVER "${ITK_TEST_DRIVER}"
    "${JAVA_RUNTIME}"
    -cp "${CMAKE_CURRENT_BINARY_DIR}\;${WrapITK_BINARY_DIR}/Java/\${CTEST_CONFIGURATION_TYPE}/InsightToolkit.jar"
  )
else(WIN32)
  set(JAVA_DRIVER "${ITK_TEST_DRIVER}"
    "${JAVA_RUNTIME}"
    -cp "${CMAKE_CURRENT_BINARY_DIR}:${WrapITK_BINARY_DIR}/Java/\${CTEST_CONFIGURATION_TYPE}/InsightToolkit.jar"
  )
endif(WIN32)


# put all generated files in this list
set(targets "")

macro(BUILD_JAVA_TEST name)
  # a convenient macro to build java tests
  #
  add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${name}.class
    DEPENDS ${WrapITK_BINARY_DIR}/Java/${CMAKE_CFG_INTDIR}/InsightToolkit.jar
    COMMAND ${JAVA_COMPILE}
    ARGS -classpath "${WrapITK_BINARY_DIR}/Java/${CMAKE_CFG_INTDIR}/InsightToolkit.jar"
    -d "${CMAKE_CURRENT_BINARY_DIR}" ${CMAKE_CURRENT_SOURCE_DIR}/${name}.java
    )
  set(targets ${targets} ${CMAKE_CURRENT_BINARY_DIR}/${name}.class)

endmacro(BUILD_JAVA_TEST)




# configure the tests




UNIQUE(types "${WRAP_ITK_SCALAR};UC")
# signed char can't be used to store an image with values up to 255
list(REMOVE_ITEM types SC)
foreach(JAVA_TEST_IMAGE_DIMENSION ${WRAP_ITK_DIMS})
  foreach(JAVA_TEST_PIXEL_TYPE ${types})

    set(TEST_FILE_NAME simplePipeline${JAVA_TEST_PIXEL_TYPE}${JAVA_TEST_IMAGE_DIMENSION})

    configure_file("${CMAKE_CURRENT_SOURCE_DIR}/simplePipeline.java.in"
                  "${CMAKE_CURRENT_BINARY_DIR}/${TEST_FILE_NAME}.java"
                  @ONLY IMMEDIATE)

    add_custom_command(
      OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${TEST_FILE_NAME}.class
      DEPENDS ${WrapITK_BINARY_DIR}/Java/${CMAKE_CFG_INTDIR}/InsightToolkit.jar ${WrapITK_BINARY_DIR}/Java/Tests/${TEST_FILE_NAME}.java
      COMMAND ${JAVA_COMPILE}
      ARGS -classpath "${WrapITK_BINARY_DIR}/Java/${CMAKE_CFG_INTDIR}/InsightToolkit.jar" -d "${CMAKE_CURRENT_BINARY_DIR}" ${CMAKE_CURRENT_BINARY_DIR}/${TEST_FILE_NAME}.java
    )

    add_test(JavaSimplePipeline${JAVA_TEST_PIXEL_TYPE}${JAVA_TEST_IMAGE_DIMENSION}
      ${JAVA_DRIVER}
      ${TEST_FILE_NAME}
      ${WrapITK_SOURCE_DIR}/images/cthead1.png
      ${TEST_FILE_NAME}.nrrd
      --compare ${TEST_FILE_NAME}.nrrd ${WrapITK_SOURCE_DIR}/images/cthead1.png
    )

    set(targets ${targets} ${CMAKE_CURRENT_BINARY_DIR}/${TEST_FILE_NAME}.class)

  endforeach(JAVA_TEST_PIXEL_TYPE)
endforeach(JAVA_TEST_IMAGE_DIMENSION)




add_custom_target(JavaTests ALL DEPENDS ${targets})
add_dependencies(JavaTests ITKJavaJar)


