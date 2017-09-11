itk_fetch_module(AnisotropicDiffusionLBR
  "Anisotropic Non-Linear Diffusion is a powerful image processing technique,
  which allows to simultaneously remove the noise and enhance sharp features
  in two or three dimensional images. Anisotropic Diffusion is understood here
  in the sense of Weickert, meaning that diffusion tensors are anisotropic and
  reflect the local orientation of image features. This is in contrast with
  the non-linear diffusion filter of Perona and Malik, which only involves
  scalar diffusion coefficients, in other words isotropic diffusion tensors.

  In this module, an anisotropic non-linear diffusion technique based on a recent
  adaptive scheme making the diffusion stable and requiring limited numerical resources
  is available.

  From the Insight Journal Article:

    \"Anisotropic Diffusion in ITK\"
    Mirebeau J., Fehrenbach J., Risser L., Tobji S.
    The Insight Journal. 2014 January-December.
    https://hdl.handle.net/10380/3505
    http://insight-journal.org/browse/publication/953
  "
  GIT_REPOSITORY ${git_protocol}://github.com/InsightSoftwareConsortium/ITKAnisotropicDiffusionLBR.git
  GIT_TAG 50acf46da9fa1480eb502a0bb550c36d518893eb
  )
