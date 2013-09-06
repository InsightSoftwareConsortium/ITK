set(DOCUMENTATION "This module contains the third party <a
href=\"http://www.openjpeg.org/\">OpenJPEG</a> library.
The OpenJPEG library is an open-source JPEG 2000 codec written in C language. It
has been developed in order to promote the use of JPEG 2000, the new still-image
compression standard from the Joint Photographic Experts Group (JPEG).")

itk_module(ITKOpenJPEG
  # Since GDCM is the only Module that depends on ITKOpenJPEG
  # EXCLUDE_FROM_DEFAULT to prevent building if not required. If
  # ITKGDCM is build, it will automatically enable this module.
  EXCLUDE_FROM_DEFAULT
  DESCRIPTION
    "${DOCUMENTATION}"
)
