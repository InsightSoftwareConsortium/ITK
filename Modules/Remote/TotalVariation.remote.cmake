itk_fetch_module(TotalVariation
"An ITK-based implementation of fast total variation methods used for image denoising,
image deconvolution, and other applications.

The class itkProxTVImageFilter wraps the third party library proxTV for 2D and 3D images.
Please refer to the documentation upstream for a detailed description:
https://github.com/albarji/proxTV
"
  GIT_REPOSITORY ${git_protocol}://github.com/InsightSoftwareConsortium/ITKTotalVariation.git
  GIT_TAG 4a4b80e8583f443b4fdaf957ea4e20c41b3b89b5
)
