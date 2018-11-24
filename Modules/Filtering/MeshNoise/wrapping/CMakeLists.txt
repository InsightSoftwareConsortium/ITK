itk_wrap_module(MeshNoise)

set(WRAPPER_SUBMODULE_ORDER
   itkAdditiveGaussianNoiseMeshFilter
   itkAdditiveGaussianNoiseQuadEdgeMeshFilter)

itk_auto_load_submodules()
itk_end_wrap_module()
