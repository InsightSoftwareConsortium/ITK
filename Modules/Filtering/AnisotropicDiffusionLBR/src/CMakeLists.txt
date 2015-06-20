set(AnisotropicDiffusionLBR_SRCS
CoherenceEnhancingDiffusion.cxx
  )

add_library(AnisotropicDiffusionLBR ${AnisotropicDiffusionLBR_SRCS})

target_link_libraries(AnisotropicDiffusionLBR  ${AnisotropicDiffusionLBR_LIBRARIES})
itk_module_target(AnisotropicDiffusionLBR)
