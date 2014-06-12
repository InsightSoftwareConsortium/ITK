#
# ITK WikiExamples
#  This remote module require a VTK build
#  The following CMake variable must be set for the ITK build
#
# Check for requires cmake variables

itk_fetch_module(WikiExamples
  "A collection of examples that illustrate how to use ITK."
  GIT_REPOSITORY https://github.com/InsightSoftwareConsortium/ITKWikiExamples.git
# June 17, 2014
  GIT_TAG 3d2213fb7475adbd1522f0e3a5e1747aa4dfe4f4
  )
