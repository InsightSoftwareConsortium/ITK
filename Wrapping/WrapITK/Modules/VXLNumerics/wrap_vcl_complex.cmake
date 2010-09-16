WRAP_CLASS("vcl_complex" FORCE_INSTANTIATE)

  foreach(t F D LD)
    WRAP_TEMPLATE("${ITKM_${t}}" "${ITKT_${t}}")

    # std::complex and vcl_complex are the same classes, but python don't know that
    LANGUAGE_SUPPORT_ADD_CLASS("complex" "std::complex" "vcl_complex${ITKM_${t}}" "${ITKT_${t}}")

  endforeach(t)

END_WRAP_CLASS()
