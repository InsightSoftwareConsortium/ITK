set(DOCUMENTATION "This module contains the third party <a
href=\"http://www.nitrc.org/projects/gifti/\">GIFTI</a> library.
Geometry format under the Neuroimaging Informatics Technology Initiative")

itk_module(ITKGIFTI
  DEPENDS
    ITKZLIB
    ITKExpat
    ITKNIFTI
  DESCRIPTION
    "${DOCUMENTATION}"
)
