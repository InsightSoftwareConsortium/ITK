#
# ITK WikiExamples
#

# If the environment var WikiExamplesTag exists, use it
if (NOT DEFINED ENV{WikiExamplesTag})
  set(GIT_TAG 34dcc22d5e943130288a1b23a36c477a1ad1566a
     )
else()
  set(GIT_TAG 34dcc22d5e943130288a1b23a36c477a1ad1566a
     )
endif()

itk_fetch_module(WikiExamples
  "A collection of examples that illustrate how to use ITK."
  GIT_REPOSITORY ${git_protocol}://github.com/InsightSoftwareConsortium/ITKWikiExamples.git
  GIT_TAG 34dcc22d5e943130288a1b23a36c477a1ad1566a
  )
