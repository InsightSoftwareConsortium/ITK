set(DOCUMENTATION "This module contains the third party <a
href=\"https://www.threadingbuildingblocks.org/\">TBB</a> library.
TBB is Intel TBB threading library.")

# ITKTBB module needs to be defined even if ITK_USE_TBB
# is OFF, otherwise ITK cannot compile.
itk_module(ITKTBB
  DESCRIPTION
    "${DOCUMENTATION}"
  EXCLUDE_FROM_DEFAULT
  )
