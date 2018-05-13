# Contact: Pablo Hernandez-Cerdan <pablo.hernandez.cerdan@outlook.com>
itk_fetch_module(IsotropicWavelets
"An ITK-based implementation of steerable isotropic wavelet transforms for multiscale phase analysis.
A more detailed description can be found in the Insight Journal article::

Cerdan, P.H. \"Steerable Isotropic Wavelets for Multiscale and Phase Analysis\".
  http://hdl.handle.net/10380/3558
  November, 2016.
"
  #UPSTREAM_GIT_REPOSITORY ${git_protocol}://github.com/phcerdan/ITKIsotropicWavelets.git
  GIT_REPOSITORY ${git_protocol}://github.com/InsightSoftwareConsortium/ITKIsotropicWavelets.git
  GIT_TAG 21678dfe6e99963511b8dfd2b407b16da4fd5a95
)
