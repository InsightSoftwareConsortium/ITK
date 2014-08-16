#
# ITK WikiExamples
#  This remote module require a VTK build
#  The following CMake variable must be set for the ITK build
#
# Check for requires cmake variables

itk_fetch_module(WikiExamples
  "A collection of examples that illustrate how to use ITK."
  GIT_REPOSITORY https://github.com/InsightSoftwareConsortium/ITKWikiExamples.git
# August 16, 2014
  GIT_TAG 2ebaf5f3f78270d6c7ea8cb4532c1d41c52cec26
  )
