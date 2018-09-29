# Contact: Matt McCormick <matt.mccormick@kitware.com>
itk_fetch_module(HigherOrderAccurateGradient
  "This module contains a filter to compute higher order
  accurate numerical derivatives and gradients from an input scalar image.
  field from a displacement field image.
  Higher Order Accurate Derivative and Gradient Calculation in ITK
  http://www.insight-journal.org/browse/publication/775
  https://hdl.handle.net/10380/3231"
  GIT_REPOSITORY ${git_protocol}://github.com/InsightSoftwareConsortium/ITKHigherOrderAccurateGradient.git
  GIT_TAG adf8fa601a8d886c841d0ae43e72b77f59f022c8
  )
