#
# ITK WikiExamples
#

# If the environment var WikiExamplesTag exists, use it
if (NOT DEFINED ENV{WikiExamplesTag})
  set(GIT_TAG 7eed9b0b11d015f9aba2aacd5a41ac1a2dbdf637)
else()
  set(GIT_TAG $ENV{WikiExamplesTag})
endif()

itk_fetch_module(WikiExamples
  "A collection of examples that illustrate how to use ITK."
  GIT_REPOSITORY ${git_protocol}://github.com/InsightSoftwareConsortium/ITKWikiExamples.git
  GIT_TAG ${GIT_TAG}
  )
