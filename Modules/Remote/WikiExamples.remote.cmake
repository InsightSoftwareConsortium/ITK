#
# ITK WikiExamples
#

# If the environment var WikiExamplesTag exists, use it
if (NOT DEFINED ENV{WikiExamplesTag})
  set(GIT_TAG 8ee6ea7c5f55d0749c22d2327fa4b7e62f38a494)
else()
  set(GIT_TAG $ENV{WikiExamplesTag})
endif()

itk_fetch_module(WikiExamples
  "A collection of examples that illustrate how to use ITK."
  GIT_REPOSITORY ${git_protocol}://github.com/InsightSoftwareConsortium/ITKWikiExamples.git
  GIT_TAG ${GIT_TAG}
  )
