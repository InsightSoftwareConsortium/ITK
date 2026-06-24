set(
  DOCUMENTATION
  "This module contains the third party <a
href=\"http://www.netlib.org/slatec/\">netlib slatec</a> routines.  They are
used by the probability distributions in ITK."
)

itk_module(ITKNetlib PRIVATE_DEPENDS ITKVNL DESCRIPTION "${DOCUMENTATION}")
