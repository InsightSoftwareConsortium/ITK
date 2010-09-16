# auto include feature must be disable because all the following classes are in the same file
set(WRAPPER_AUTO_INCLUDE_HEADERS OFF)
WRAP_INCLUDE("itkSparseFieldFourthOrderLevelSetImageFilter.h")

WRAP_CLASS("itk::NormalBandNode")
  WRAP_IMAGE_FILTER_REAL(1)
END_WRAP_CLASS()

WRAP_CLASS("itk::Image" POINTER)
  foreach(d ${WRAP_ITK_DIMS})
    foreach(t ${WRAP_ITK_REAL})
      WRAP_TEMPLATE("NBN${ITKM_I${t}${d}}${d}" "itk::NormalBandNode< ${ITKT_I${t}${d}} >*, ${d}")
    endforeach(t)
  endforeach(d)
END_WRAP_CLASS()

WRAP_CLASS("itk::SparseFieldFourthOrderLevelSetImageFilter" POINTER)
  # WRAP_IMAGE_FILTER_USIGN_INT(2)
  # WRAP_IMAGE_FILTER_SIGN_INT(2)
  WRAP_IMAGE_FILTER_REAL(2)
END_WRAP_CLASS()
