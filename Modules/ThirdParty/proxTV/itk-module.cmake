set(
  DOCUMENTATION
  "This module builds the proxTV library
<a href=\"https://github.com/albarji/proxTV\">proxTV</a>, a toolbox of fast
total variation proximity operators, for use by the TotalVariation module."
)

itk_module(
  ITKproxTV
  DEPENDS
    ITKEigen3
  DESCRIPTION "${DOCUMENTATION}"
  EXCLUDE_FROM_DEFAULT
)
