#
# ITK WikiExamples
#

# If the environment var WikiExamplesTag exists, use it
if (NOT DEFINED ENV{WikiExamplesTag})
  # December 26, 2016
  set(GIT_TAG 93b266b8b8a1443ed1dfd47b9b427fce4aebf3b0)
else()
  set(GIT_TAG $ENV{WikiExamplesTag})
endif()

itk_fetch_module(WikiExamples
  "A collection of examples that illustrate how to use ITK."
  GIT_REPOSITORY ${git_protocol}://github.com/InsightSoftwareConsortium/ITKWikiExamples.git
  GIT_TAG ${GIT_TAG}
  )
