project(
  AnisotropicDiffusionLBR
  VERSION
    5.1.0 #Version should track with ITK
  LANGUAGES
    CXX
)

# Header only module, no libraries set(AnisotropicDiffusionLBR_LIBRARIES AnisotropicDiffusionLBR)

set(ITK_DIR ${CMAKE_BINARY_DIR})
itk_module_impl()
