# auto include feature must be disable because the class is not in the file
# with the same name
SET(WRAPPER_AUTO_INCLUDE_HEADERS OFF)
WRAP_INCLUDE("itkLevelSet.h")

WRAP_CLASS("itk::LevelSetNode")
  SET(WRAPPER_TEMPLATES "${itk_Wrap_LevelSetNode}")
END_WRAP_CLASS()
