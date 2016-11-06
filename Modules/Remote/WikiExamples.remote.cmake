#
# ITK WikiExamples
#

# If the environment var RemoteGitTag exists, use it
if (NOT DEFINED ENV{RemoteGitTag})
  # October 19, 2016
  set(GIT_TAG 8958c5b37a7c16794e69aafa9389253098021809)
else()
  set(GIT_TAG $ENV{RemoteGitTag})
endif()

itk_fetch_module(WikiExamples
  "A collection of examples that illustrate how to use ITK."
  GIT_REPOSITORY ${git_protocol}://github.com/InsightSoftwareConsortium/ITKWikiExamples.git
  GIT_TAG ${GIT_TAG}
  )
