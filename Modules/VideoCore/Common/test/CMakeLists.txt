itk_module_test()
set(Video-CommonTests
  itkRingBufferImageSetTest.cxx
  itkVideoImageSetTest.cxx
)

# For Midas-stored testing data
include(MIDAS.cmake)
set( MIDAS_REST_URL "http://midas.kitware.com/api/rest")
set( MIDAS_KEY_DIR "${CMAKE_CURRENT_SOURCE_DIR}/data_hashes" )

CreateTestDriver(Video-Common  "${Video-Common-Test_LIBRARIES}" "${Video-CommonTests}")

# RingBufferImageSet:
midas_add_test( NAME RingBufferImageSetTest
          COMMAND Video-CommonTestDriver itkRingBufferImageSetTest
          # Input:
          "MIDAS{frame0.jpg.md5}\;MIDAS{frame1.jpg.md5}\;MIDAS{frame2.jpg.md5}\;MIDAS{frame3.jpg.md5}\;MIDAS{frame4.jpg.md5}"
          # Char Output:
          "${ITK_TEST_OUTPUT_DIR}/RingBuffer_charOut0.png\;${ITK_TEST_OUTPUT_DIR}/RingBuffer_charOut1.png\;${ITK_TEST_OUTPUT_DIR}/RingBuffer_charOut2.png\;${ITK_TEST_OUTPUT_DIR}/RingBuffer_charOut3.png\;${ITK_TEST_OUTPUT_DIR}/RingBuffer_charOut4.png"
          # Float Output:
          "${ITK_TEST_OUTPUT_DIR}/RingBuffer_floatOut0.mha\;${ITK_TEST_OUTPUT_DIR}/RingBuffer_floatOut1.mha\;${ITK_TEST_OUTPUT_DIR}/RingBuffer_floatOut2.mha\;${ITK_TEST_OUTPUT_DIR}/RingBuffer_floatOut3.mha\;${ITK_TEST_OUTPUT_DIR}/RingBuffer_floatOut4.mha"
        )

# VideoImageSet:
midas_add_test( NAME VideoImageSetTest
          COMMAND Video-CommonTestDriver itkVideoImageSetTest
          # Input:
          "MIDAS{frame0.jpg.md5}\;MIDAS{frame1.jpg.md5}\;MIDAS{frame2.jpg.md5}\;MIDAS{frame3.jpg.md5}\;MIDAS{frame4.jpg.md5}"
          # Char Output:
          "${ITK_TEST_OUTPUT_DIR}/VideoImageSet_charOut0.png\;${ITK_TEST_OUTPUT_DIR}/VideoImageSet_charOut1.png\;${ITK_TEST_OUTPUT_DIR}/VideoImageSet_charOut2.png\;${ITK_TEST_OUTPUT_DIR}/VideoImageSet_charOut3.png\;${ITK_TEST_OUTPUT_DIR}/VideoImageSet_charOut4.png"
          # Float Output:
          "${ITK_TEST_OUTPUT_DIR}/VideoImageSet_floatOut0.mha\;${ITK_TEST_OUTPUT_DIR}/VideoImageSet_floatOut1.mha\;${ITK_TEST_OUTPUT_DIR}/VideoImageSet_floatOut2.mha\;${ITK_TEST_OUTPUT_DIR}/VideoImageSet_floatOut3.mha\;${ITK_TEST_OUTPUT_DIR}/VideoImageSet_floatOut4.mha"
        )
