# Contact: Matt McCormick <matt.mccormick@kitware.com>
itk_fetch_module(SplitComponents
"This module contains filter called
itk::SplitComponentsImageFilter.  This filter generates component images from an
itk::Image of, for example, itk::Vector, itk::CovariantVector, or
itk::SymmetricSecondRankTensor. https://hdl.handle.net/10380/3230"
  GIT_REPOSITORY ${git_protocol}://github.com/thewtex/ITKSplitComponents/
  GIT_TAG ITKv5
  )
