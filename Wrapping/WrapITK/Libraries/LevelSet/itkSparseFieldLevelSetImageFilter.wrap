WRAP_INCLUDE("itkIndex.h")

WRAP_CLASS("itk::SparseFieldLevelSetImageFilter" POINTER)
  WRAP_IMAGE_FILTER_REAL(2)
END_WRAP_CLASS()

set(WRAPPER_AUTO_INCLUDE_HEADERS OFF)
WRAP_CLASS("itk::SparseFieldLevelSetNode")
  foreach(d ${WRAP_ITK_DIMS})
    WRAP_TEMPLATE("I${d}" "itk::Index< ${d} >")
  endforeach(d)
END_WRAP_CLASS()
set(WRAPPER_AUTO_INCLUDE_HEADERS ON)

WRAP_CLASS("itk::SparseFieldLayer" POINTER)
  foreach(d ${WRAP_ITK_DIMS})
    WRAP_TEMPLATE("SFLSNI${d}" "itk::SparseFieldLevelSetNode< itk::Index< ${d} > >")
  endforeach(d)
END_WRAP_CLASS()
