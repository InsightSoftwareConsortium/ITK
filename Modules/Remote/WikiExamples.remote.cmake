#
# ITK WikiExamples
#  This remote module require a VTK build
#  The following CMake variable must be set for the ITK build
#
# Check for requires cmake variables

itk_fetch_module(WikiExamples
  "A collection of examples that illustrate how to use ITK."
  GIT_REPOSITORY https://github.com/InsightSoftwareConsortium/ITKWikiExamples.git
  # January 05, 2015
  GIT_TAG 42c129a898641934a84339c9a0ab69c5295f7dbe
  )
