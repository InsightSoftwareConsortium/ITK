set(TEMP ${ITK_BINARY_DIR}/Testing/Temporary)

if(ITK_WRAP_PYTHON)
  INTERSECTION(WRAP_2 2 "${ITK_WRAP_IMAGE_DIMS}")

  if(ITK_WRAP_signed_short AND ITK_WRAP_unsigned_char AND WRAP_2)
    itk_python_add_test(NAME DicomSliceReadPythonTest
      TEST_DRIVER_ARGS
        --compare ${TEMP}/DicomSliceReadPythonOutput.png
          DATA{${ITK_SOURCE_DIR}/Examples/IO/test/Baseline/DicomSliceReadPythonOutputBaseline.png}
      COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/../DicomSliceRead.py
        DATA{${ITK_DATA_ROOT}/Input/dicom-sc_cs-1.dcm}
        ${TEMP}/DicomSliceReadPythonOutput.png
      )
  endif()
endif()

if(ITK_BUILD_SHARED_LIBS)
  itk_add_test( NAME ExampleIOPluginTest
    COMMAND itkTestDriver  --add-before-env ITK_AUTOLOAD_PATH "$<TARGET_FILE_DIR:FileFreeTestLib>"
      "$<TARGET_FILE:IOPlugin>"
      "FileFreeIO::Size=128,256:Spacing=.5,.8:Origin=5,6:Direction=-1,0,0,-1"
      "${ITK_TEST_OUTPUT_DIR}ExampleIOPluginTest.png"
      )
  itk_add_test( NAME ExampleIOPluginTest2
    COMMAND itkTestDriver
      "$<TARGET_FILE:IOPlugin>"
      "FileFreeIO::Size=128,256:Spacing=.5,.8:Origin=5,6:Direction=-1,0,0,-1"
      "${ITK_TEST_OUTPUT_DIR}ExampleIOPluginTest2.png"
      "$<TARGET_FILE_DIR:FileFreeTestLib>"
      )

endif()

# Test ITK ImageIO factory registrations.
# Please refer to CMake/UseITK.cmake for all the image IO registartion handeling.

# Collecting image format's description into a string for verification
set(REGEX_IO_Factory_Description_First_Word  "")
foreach(ImageFormat ${LIST_OF_IMAGEIO_FORMATS})
  if (${${ImageFormat}_imageio_module_name}_ENABLED)
    if (NOT REGEX_IO_Factory_Description_First_Word)
      set (REGEX_IO_Factory_Description_First_Word ${ImageFormat})
    else()
      set (REGEX_IO_Factory_Description_First_Word"${REGEX_IO_Factory_Description_First_Word} ${ImageFormat}")
    endif()
  endif()
endforeach()

itk_add_test( NAME ImageIOFactoryRegisterTest
    COMMAND ${ITK_TEST_DRIVER}
    $<TARGET_FILE:IOFactoryRegistration>
    )
set_tests_properties (ImageIOFactoryRegisterTest
  PROPERTIES PASS_REGULAR_EXPRESSION  ${REGEX_IO_Factory_Description_First_Word})


if(ITK_VISIBLEHUMAN_DATA_ROOT)
  itk_add_test(NAME VisibleHumanStreamReadWriteTest
    COMMAND ${ITK_TEST_DRIVER}
   $<TARGET_FILE:VisibleHumanStreamReadWrite>
    ${ITK_VISIBLEHUMAN_DATA_ROOT}/Male/Fullcolor/fullbody/
    ${TEMP}/VisibleHumanStreamReadWrite.mha )
  itk_add_test(NAME VisibleHumanPasteWriteTest
    COMMAND ${ITK_TEST_DRIVER}
   $<TARGET_FILE:VisibleHumanPasteWrite>
    ${TEMP}/VisibleHumanStreamReadWrite.mha
    ${TEMP}/VisibleHumanPasteWrite.mha)
  # this test must be run sequentially, be cause the output of the first is used by the second
  set_tests_properties(
    VisibleHumanStreamReadWriteTest
    PROPERTIES
    LABELS BigIO
    )
  set_tests_properties(
    VisibleHumanPasteWriteTest
    PROPERTIES
    DEPENDS VisibleHumanStreamReadWriteTest
    )
endif()

itk_add_test(NAME TransformReadWriteTest
  COMMAND TransformReadWrite ${TEMP}/TransformReadWrite.h5 )
