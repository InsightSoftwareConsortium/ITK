# some tests will fail if dim=2 and unsigned short are not wrapped
list(FIND ITK_WRAP_IMAGE_DIMS 2 wrap_2_index)
if(ITK_WRAP_unsigned_short AND wrap_2_index GREATER -1)
    itk_python_add_test(NAME LabelImageToStatisticsLabelMapFilterTest2
                        COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/LabelImageToStatisticsLabelMapFilterTest2.py)
endif()
