### NOTE:  This assumes taht the 3rd and 4th template arguments are both the same floating point type.
### NOTE:  Removed scalar types for the vector image type, because it must match the 3rd lement type.
### NOTE:  This change was necessary to avoid itk BUG number 0010205 that demonstrates that the only valid
###        vector type is one that has floating point voxel elements that are same as the PriorType.
###        To preserve backwards compatibility, the types are re-used multiple times in an unconventional way.
WRAP_CLASS("itk::BayesianClassifierImageFilter" POINTER)
  FOREACH(d ${WRAP_ITK_DIMS})
      FOREACH(t2 ${WRAP_ITK_INT})
        FOREACH(t3 ${WRAP_ITK_REAL})
          WRAP_TEMPLATE("${ITKM_VI${t3}${d}}${ITKM_${t2}}${ITKM_${t3}}" "${ITKT_VI${t3}${d}}, ${ITKT_${t2}}, ${ITKT_${t3}}, ${ITKT_${t3}}")
        ENDFOREACH(t3)
      ENDFOREACH(t2)
  ENDFOREACH(d)
END_WRAP_CLASS()
