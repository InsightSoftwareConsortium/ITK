set(DOCUMENTATION "This module contains the third party <a
href=\"http://niftilib.sourceforge.net/\">NIFTI</a> library.
Neuroimaging Informatics Technology Initiative provides an Analyze-style MRI
file format.")

itk_module(ITKNIFTI
  DEPENDS
    ITKZLIB
  DESCRIPTION
    "${DOCUMENTATION}"
)
