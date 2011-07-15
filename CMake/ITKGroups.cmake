# Set a list of group names
set(group_list Core IO Filtering Registration Segmentation Numerics Utilities Bridge Nonunit)

#------------------------------------------------
# Set a module name list for each group
set(Core_module_list
ITKCommon
ITKFiniteDifference
ITKImageAdaptors
ITKImageFunction
ITKImageGrid
ITKImageStatistics
ITKMesh
ITKQuadEdgeMesh
ITKSpatialObjects
ITKTestKernel
ITKTransform
)


set(IO_module_list
ITKIOBase
ITKIOBioRad
ITKIOBMP
ITKIOGDCM
ITKIOGE
ITKIOGIPL
ITKIOIPL
ITKIOJPEG
ITKIOLSM
ITKIOMeta
ITKIONIFTI
ITKIONRRD
ITKIOPhilipsREC
ITKIOPNG
ITKIORAW
ITKIOSiemens
ITKIOSpatialObjects
ITKIOStimulate
ITKIOTIFF
ITKIOVTK
ITKIOXML
)

set(Filtering_module_list
ITKAnisotropicSmoothing
ITKAntiAlias
ITKBiasCorrection
ITKConnectedComponents
ITKCurvatureFlow
ITKDeformationField
ITKDiffusionTensorImage
ITKDistanceMap
ITKFFT
ITKFastMarching
ITKImageCompare
ITKImageCompose
ITKImageFeature
ITKImageFilterBase
ITKImageGradient
ITKImageGrid
ITKImageIntensity
ITKImageLabel
ITKImageStatistics
ITKLabelVoting
ITKMathematicalMorphology
ITKPath
ITKQuadEdgeMeshFiltering
ITKSmoothing
ITKSpatialFunction
ITKThresholding
)

set(Registration_module_list
ITKFEMRegistration
ITKPDEDeformableRegistration
ITKRegistrationCommon
)

set(Segmentation_module_list
ITKBioCell
ITKClassifiers
ITKConnectedComponents
ITKDeformableMesh
ITKKLMRegionGrowing
ITKLabelVoting
ITKLevelSets
ITKMarkovRandomFieldsClassifier
ITKRegionGrowing
ITKSignedDistanceFunction
ITKVoronoi
ITKWatersheds
)

set(Numerics_module_list
ITKEigen
ITKFEM
ITKNarrowBand
ITKNeuralNetworks
ITKOptimizers
ITKPolynomials
ITKStatistics
)


set(Bridge_module_list
ITKVtkGlue
ITKVTK)

set(Utilities_module_list
ITKKWSys
ITKVNL
ITKPNG
ITKJPEG
ITKExpat
ITKNrrdIO
ITKNIFTI
ITKMetaIO
ITKGDCM
ITKOpenJPEG
ITKZLIB
ITKVNLInstantiation
ITKTIFF
)

set(Nonunit_module_list
ITKIntegratedTest
ITKReview
)
#------------------------------------------------
# Turn on the ITK_BUILD option for each group
if("$ENV{DASHBOARD_TEST_FROM_CTEST}" STREQUAL "")
  # developer build
  option(ITKGroup_Core "Request building core modules" ON)
endif()
foreach( group ${group_list})
    option(ITKGroup_${group} "Request building ${group} modules" OFF)
    if (ITKGroup_${group})
      foreach (itk-module ${${group}_module_list} )
         list(APPEND ITK_MODULE_${itk-module}_REQUEST_BY ITKGroup_${group})
      endforeach()
    endif()
    # Hide group options if building all modules anyway.
    if(ITK_BUILD_ALL_MODULES)
      set_property(CACHE ITKGroup_${group} PROPERTY TYPE INTERNAL)
    else()
      set_property(CACHE ITKGroup_${group} PROPERTY TYPE BOOL)
    endif()
endforeach()
