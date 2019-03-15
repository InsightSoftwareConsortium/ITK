# Contact: Pablo Hernandez-Cerdan <pablo.hernandez.cerdan@outlook.com>
itk_fetch_module(IsotropicWavelets
"An ITK-based implementation of steerable isotropic wavelet transforms for multiscale phase analysis.
A more detailed description can be found in the Insight Journal article::

Cerdan, P.H. \"Steerable Isotropic Wavelets for Multiscale and Phase Analysis\".
  http://hdl.handle.net/10380/3558
  November, 2016.
"
  # Upstream repository was transfered from phcerdan to InsightSoftwareConsortium
  GIT_REPOSITORY ${git_protocol}://github.com/InsightSoftwareConsortium/ITKIsotropicWavelets.git
  GIT_TAG 99d527a46085af6933aac608a3934912829232df
)
