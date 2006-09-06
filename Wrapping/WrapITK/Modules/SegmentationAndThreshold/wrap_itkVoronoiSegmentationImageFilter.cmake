
# build seems to succeed only with unsigned char
# also, Superclass typedef is wrong in VoronoiSegmentationImageFilter
# and so we can't use POINTER_WITH_SUPERCLASS to wrap
# VoronoiSegmentationImageFilterBase

WRAP_CLASS("itk::VoronoiSegmentationImageFilterBase" POINTER)
  IF(WRAP_usigned_char)
    WRAP_IMAGE_FILTER(UC 3 2)
  ENDIF(WRAP_usigned_char)
#   WRAP_IMAGE_FILTER_USIGN_INT(3 2)
END_WRAP_CLASS()

WRAP_CLASS("itk::VoronoiSegmentationImageFilter" POINTER)
  IF(WRAP_usigned_char)
    WRAP_IMAGE_FILTER(UC 3 2)
  ENDIF(WRAP_usigned_char)
#   WRAP_IMAGE_FILTER_USIGN_INT(3 2)
END_WRAP_CLASS()

