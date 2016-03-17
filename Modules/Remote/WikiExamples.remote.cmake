#
# ITK WikiExamples
#

# If the environment var RemoteGitTag exists, use it
if (NOT DEFINED ENV{RemoteGitTag})
  # February 18, 2016
  # Additional tests and baselines
  set(GIT_TAG 5b474c63f04d17c7773abd1e33f9ea295d291e59)
else()
  set(GIT_TAG $ENV{RemoteGitTag})
endif()

itk_fetch_module(WikiExamples
  "A collection of examples that illustrate how to use ITK."
  GIT_REPOSITORY ${git_protocol}://github.com/InsightSoftwareConsortium/ITKWikiExamples.git
  GIT_TAG ${GIT_TAG}
  )
