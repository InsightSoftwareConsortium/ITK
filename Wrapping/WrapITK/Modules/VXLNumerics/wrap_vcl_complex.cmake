WRAP_CLASS("vcl_complex" FORCE_INSTANTIATE)

  FOREACH(t F D LD)
    WRAP_TEMPLATE("${ITKM_${t}}" "${ITKT_${t}}")
  
    # std::complex and vcl_complex are the same classes, but python don't know that
    LANGUAGE_SUPPORT_ADD_CLASS("complex" "std::complex" "vcl_complex${ITKM_${t}}" "${ITKT_${t}}")

  ENDFOREACH(t)

END_WRAP_CLASS()
