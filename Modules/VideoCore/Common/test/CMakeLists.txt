itk_module_test()
set(Video-CommonTests
  itkRingBufferImageSetTest.cxx
  itkVideoImageSetTest.cxx
)

CreateTestDriver(Video-Common  "${Video-Common-Test_LIBRARIES}" "${Video-CommonTests}")

#DEBUG
set(ITKVideo_Data_DIR /home/gabe/Projects/itk4/Video/ITKVideo/Video/Data)


# RingBufferImageSet:
add_test( NAME RingBufferImageSetTest
          COMMAND Video-CommonTestDriver itkRingBufferImageSetTest
          # Input:
          "${ITKVideo_Data_DIR}/Input/FileList/frame0.png\;${ITKVideo_Data_DIR}/Input/FileList/frame1.png\;${ITKVideo_Data_DIR}/Input/FileList/frame2.png\;${ITKVideo_Data_DIR}/Input/FileList/frame3.png\;${ITKVideo_Data_DIR}/Input/FileList/frame4.png"
          # Char Output:
          "${ITKVideo_Data_DIR}/RingBuffer_charOut0.png\;${ITKVideo_Data_DIR}/RingBuffer_charOut1.png\;${ITKVideo_Data_DIR}/RingBuffer_charOut2.png\;${ITKVideo_Data_DIR}/RingBuffer_charOut3.png\;${ITKVideo_Data_DIR}/RingBuffer_charOut4.png"
          # Float Output:
          "${ITKVideo_Data_DIR}/RingBuffer_floatOut0.mha\;${ITKVideo_Data_DIR}/RingBuffer_floatOut1.mha\;${ITKVideo_Data_DIR}/RingBuffer_floatOut2.mha\;${ITKVideo_Data_DIR}/RingBuffer_floatOut3.mha\;${ITKVideo_Data_DIR}/RingBuffer_floatOut4.mha"
        )

# VideoImageSet:
add_test( NAME VideoImageSetTest
          COMMAND Video-CommonTestDriver itkVideoImageSetTest
          # Input:
          "${ITKVideo_Data_DIR}/Input/FileList/frame0.png\;${ITKVideo_Data_DIR}/Input/FileList/frame1.png\;${ITKVideo_Data_DIR}/Input/FileList/frame2.png\;${ITKVideo_Data_DIR}/Input/FileList/frame3.png\;${ITKVideo_Data_DIR}/Input/FileList/frame4.png"
          # Char Output:
          "${ITKVideo_Data_DIR}/VideoImageSet_charOut0.png\;${ITKVideo_Data_DIR}/VideoImageSet_charOut1.png\;${ITKVideo_Data_DIR}/VideoImageSet_charOut2.png\;${ITKVideo_Data_DIR}/VideoImageSet_charOut3.png\;${ITKVideo_Data_DIR}/VideoImageSet_charOut4.png"
          # Float Output:
          "${ITKVideo_Data_DIR}/VideoImageSet_floatOut0.mha\;${ITKVideo_Data_DIR}/VideoImageSet_floatOut1.mha\;${ITKVideo_Data_DIR}/VideoImageSet_floatOut2.mha\;${ITKVideo_Data_DIR}/VideoImageSet_floatOut3.mha\;${ITKVideo_Data_DIR}/VideoImageSet_floatOut4.mha"
        )
