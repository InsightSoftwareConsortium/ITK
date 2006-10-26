
# configure the test driver
FIND_PROGRAM(ITK_TEST_DRIVER itkTestDriver)

SET(JAVA_DRIVER "${ITK_TEST_DRIVER}"
  "${JAVA_RUNTIME}"
  -cp ".:${WrapITK_BINARY_DIR}/Java/${CMAKE_CFG_INTDIR}/InsightToolkit.jar"
)
  

# put all generated files in this list
SET(targets "")

MACRO(BUILD_JAVA_TEST name)
  # a convenient macro to build java tests
  # 
  ADD_CUSTOM_COMMAND(
    OUTPUT ${name}.class
    DEPENDS ${WrapITK_BINARY_DIR}/Java/${CMAKE_CFG_INTDIR}/InsightToolkit.jar
    COMMAND ${JAVA_COMPILE}
    ARGS -classpath "${WrapITK_BINARY_DIR}/Java/${CMAKE_CFG_INTDIR}/InsightToolkit.jar"
    -d "${CMAKE_CURRENT_BINARY_DIR}" ${CMAKE_CURRENT_SOURCE_DIR}/${name}.java
    )
  SET(targets ${targets} ${name}.class)
  
ENDMACRO(BUILD_JAVA_TEST)




# configure the tests




UNIQUE(types "${WRAP_ITK_SCALAR};UC")
# signed char can't be used to store an image with values up to 255
LIST(REMOVE_ITEM types SC)
FOREACH(JAVA_TEST_IMAGE_DIMENSION ${WRAP_ITK_DIMS})
  FOREACH(JAVA_TEST_PIXEL_TYPE ${types})

    SET(TEST_FILE_NAME simplePipeline${JAVA_TEST_PIXEL_TYPE}${JAVA_TEST_IMAGE_DIMENSION})

    CONFIGURE_FILE("${CMAKE_CURRENT_SOURCE_DIR}/simplePipeline.java.in"
                  "${CMAKE_CURRENT_BINARY_DIR}/${TEST_FILE_NAME}.java"
                  @ONLY IMMEDIATE)

    ADD_CUSTOM_COMMAND(
      OUTPUT ${TEST_FILE_NAME}.class
      DEPENDS ${WrapITK_BINARY_DIR}/Java/${CMAKE_CFG_INTDIR}/InsightToolkit.jar ${WrapITK_BINARY_DIR}/Java/Tests/${TEST_FILE_NAME}.java
      COMMAND ${JAVA_COMPILE}
      ARGS -classpath "${WrapITK_BINARY_DIR}/Java/${CMAKE_CFG_INTDIR}/InsightToolkit.jar" -d "${CMAKE_CURRENT_BINARY_DIR}" ${CMAKE_CURRENT_BINARY_DIR}/${TEST_FILE_NAME}.java
    )

    ADD_TEST(JavaSimplePipeline${JAVA_TEST_PIXEL_TYPE}${JAVA_TEST_IMAGE_DIMENSION}
      ${JAVA_DRIVER}
      ${TEST_FILE_NAME}
      ${WrapITK_SOURCE_DIR}/images/cthead1.png
      ${TEST_FILE_NAME}.nrrd
      --compare ${TEST_FILE_NAME}.nrrd ${WrapITK_SOURCE_DIR}/images/cthead1.png
    )
    
    SET(targets ${targets} ${TEST_FILE_NAME}.class)

  ENDFOREACH(JAVA_TEST_PIXEL_TYPE)
ENDFOREACH(JAVA_TEST_IMAGE_DIMENSION)




ADD_CUSTOM_TARGET(JavaTests ALL DEPENDS ${targets} ITKJavaJar)

