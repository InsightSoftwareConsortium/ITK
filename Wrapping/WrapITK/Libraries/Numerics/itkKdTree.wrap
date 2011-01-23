WRAP_INCLUDE("itkListSample.h")

WRAP_CLASS("itk::Statistics::KdTree" POINTER)
  foreach(d ${WRAP_ITK_DIMS})
    WRAP_TEMPLATE("LS${ITKM_VF${d}}"  "itk::Statistics::ListSample< ${ITKT_VF${d}} >")
  endforeach(d)
END_WRAP_CLASS()

set(WRAPPER_AUTO_INCLUDE_HEADERS OFF)
WRAP_CLASS("itk::Statistics::KdTreeNode")
  foreach(d ${WRAP_ITK_DIMS})
    WRAP_TEMPLATE("LS${ITKM_VF${d}}"  "itk::Statistics::ListSample< ${ITKT_VF${d}} >")
  endforeach(d)
END_WRAP_CLASS()
