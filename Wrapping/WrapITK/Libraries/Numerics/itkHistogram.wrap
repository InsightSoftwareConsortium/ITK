WRAP_CLASS("itk::Statistics::Histogram" POINTER)
  set(WRAPPER_TEMPLATES "${itk_Wrap_Histogram}")
END_WRAP_CLASS()

# also wrap the decorator in the same file to avoid file collision with the
# one in Base
WRAP_CLASS("itk::SimpleDataObjectDecorator" POINTER)
  WRAP_TEMPLATE("${ITKM_HF}"  "${ITKT_HF} *")
  WRAP_TEMPLATE("${ITKM_HD}"  "${ITKT_HD} *")
END_WRAP_CLASS()
