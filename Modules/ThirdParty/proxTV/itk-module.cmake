set(
  DOCUMENTATION
  "This module builds the proxTV library
<a href=\"https://github.com/albarji/proxTV\">proxTV</a>, a toolbox of fast
total variation proximity operators, for use by the TotalVariation module."
)

if(ITK_USE_SYSTEM_proxTV)
  itk_module(ITKproxTV DESCRIPTION "${DOCUMENTATION}" EXCLUDE_FROM_DEFAULT)
else()
  itk_module(
    ITKproxTV
    DEPENDS
      ITKEigen3
    DESCRIPTION "${DOCUMENTATION}"
    EXCLUDE_FROM_DEFAULT
  )
endif()
