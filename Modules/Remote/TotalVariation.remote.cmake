itk_fetch_module(TotalVariation
"An ITK-based implementation of fast total variation methods used for image denoising,
image deconvolution, and other applications.

The class itkProxTVImageFilter wraps the third party library proxTV for 2D and 3D images.
Please refer to the documentation upstream for a detailed description:
https://github.com/albarji/proxTV
"
  GIT_REPOSITORY ${git_protocol}://github.com/InsightSoftwareConsortium/ITKTotalVariation.git
  GIT_TAG e47fb5b1e2eef32cddb131d8ba47b78dbde9fbbd
)
