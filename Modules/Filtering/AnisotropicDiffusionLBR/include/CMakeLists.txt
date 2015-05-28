set(ADLBR_Headers 
   ${CMAKE_CURRENT_SOURCE_DIR}/AnisotropicDiffusionLBRImageFilter.h
   ${CMAKE_CURRENT_SOURCE_DIR}/AnisotropicDiffusionLBRImageFilter.hxx
   ${CMAKE_CURRENT_SOURCE_DIR}/LinearAnisotropicDiffusionLBRImageFilter.h
   ${CMAKE_CURRENT_SOURCE_DIR}/LinearAnisotropicDiffusionLBRImageFilter.hxx
   ${CMAKE_CURRENT_SOURCE_DIR}/StructureTensorImageFilter.h
   ${CMAKE_CURRENT_SOURCE_DIR}/StructureTensorImageFilter.hxx
   ${CMAKE_CURRENT_SOURCE_DIR}/CoherenceEnhancingDiffusionFilter.h
   ${CMAKE_CURRENT_SOURCE_DIR}/LinearAnisotropicDiffusionCommandLine.h
   ${CMAKE_CURRENT_SOURCE_DIR}/CoherenceEnhancingDiffusionCommandLine.h
   ${CMAKE_CURRENT_SOURCE_DIR}/UnaryFunctorWithIndexImageFilter.h
   ${CMAKE_CURRENT_SOURCE_DIR}/Macro.h
   PARENT_SCOPE
)

set(PROJECT_DIR ${PROJECT_DIR} ${CMAKE_CURRENT_SOURCE_DIR} PARENT_SCOPE)

