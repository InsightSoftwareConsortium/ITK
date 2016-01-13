set(
  DOCUMENTATION
  "This module implements a minimal path extraction framework
based on Fast Marching arrival functions.

A more detailed description can be found in the Insight Journal article::

  Mueller, D. \"Fast Marching Minimal Path Extraction in ITK\"
  http://hdl.handle.net/1926/1332
  http://www.insight-journal.org/browse/publication/213
  March, 2008.
"
)

itk_module(
  MinimalPathExtraction
  ENABLE_SHARED
  DEPENDS
    ITKCommon
    ITKOptimizers
    ITKPath
    ITKFastMarching
  TEST_DEPENDS
    ITKTestKernel
    ITKIOSpatialObjects
  EXCLUDE_FROM_DEFAULT
  DESCRIPTION "${DOCUMENTATION}"
)
