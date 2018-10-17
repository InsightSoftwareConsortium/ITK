itk_wrap_module(DVMeshNoise)

set(WRAPPER_SUBMODULE_ORDER
   AdditiveGaussianNoiseMeshFilter
   AdditiveGaussianNoiseQuadEdgeMeshFilter)

itk_auto_load_submodules()
itk_end_wrap_module()
