itk_fetch_module(SimpleITKFilters
  "A Collection of filters designed for SimpleITK.
  The filters in this module may be wrappers of composites ITK
  filters, adapters of existing ITK filters or alternative
  implementations designed for SimpleITK's requirements. Currently it
  contains a discrete hessian, and a composite filter to compute objectness."
  GIT_REPOSITORY ${git_protocol}://github.com/SimpleITK/ITKSimpleITKFilters.git
  GIT_TAG 369cb8ee905d9c4253ff8b690394bdbf02a4e951
  )
