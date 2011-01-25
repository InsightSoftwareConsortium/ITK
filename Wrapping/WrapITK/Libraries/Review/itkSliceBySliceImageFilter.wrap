WRAP_CLASS("itk::SliceBySliceImageFilter" POINTER)
  foreach(d ${WRAP_ITK_DIMS})
    math(EXPR d_1 "${d} - 1")
    FILTER_DIMS(has_d_1 ${d_1})
    if(has_d_1)
      # avoid the vector and covariant vector types: the type
      # Image< Vector< T, N >, N-1 > is not wrapped
      WRAP_IMAGE_FILTER_SCALAR(2 ${d})
      WRAP_IMAGE_FILTER_RGB(2 ${d})
      WRAP_IMAGE_FILTER_COMPLEX_REAL(2 ${d})
    endif(has_d_1)
  endforeach(d)
END_WRAP_CLASS()
