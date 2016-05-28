#
# ITK WikiExamples
#

# If the environment var RemoteGitTag exists, use it
if (NOT DEFINED ENV{RemoteGitTag})
  # May 22, 2016
  set(GIT_TAG 1a2a68ca98966bff35ba9332619ffbb9a278d0e9)
else()
  set(GIT_TAG $ENV{RemoteGitTag})
endif()

itk_fetch_module(WikiExamples
  "A collection of examples that illustrate how to use ITK."
  GIT_REPOSITORY ${git_protocol}://github.com/InsightSoftwareConsortium/ITKWikiExamples.git
  GIT_TAG ${GIT_TAG}
  )
