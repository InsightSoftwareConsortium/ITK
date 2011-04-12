# Set a list of group names
set(group_list Core IO Filtering Registration Segmentation Numerics Utilities Bridge Nonunit)

#------------------------------------------------
# Set a module name list for each group
set(Core_module_list
ITK-Common
ITK-FiniteDifference
ITK-ImageAdaptors
ITK-ImageFunction
ITK-ImageGrid
ITK-ImageStatistics
ITK-Mesh
ITK-QuadEdgeMesh
ITK-SpatialObjects
ITK-TestKernel
ITK-Transform
)


set(IO_module_list
ITK-IO-Base
ITK-IO-BioRad
ITK-IO-BMP
ITK-IO-GDCM
ITK-IO-GE
ITK-IO-GIPL
ITK-IO-IPL
ITK-IO-JPEG
ITK-IO-LSM
ITK-IO-Meta
ITK-IO-NIFTI
ITK-IO-NRRD
ITK-IO-PNG
ITK-IO-RAW
ITK-IO-Siemens
ITK-IO-SpatialObjects
ITK-IO-Stimulate
ITK-IO-TIFF
ITK-IO-VTK
ITK-IO-XML
)

set(Filtering_module_list
ITK-AnisotropicSmoothing
ITK-AntiAlias
ITK-BiasCorrection
ITK-ConnectedComponents
ITK-CurvatureFlow
ITK-DeformationField
ITK-DiffusionTensorImage
ITK-DistanceMap
ITK-FFT
ITK-FastMarching
ITK-ImageCompare
ITK-ImageCompose
ITK-ImageFeature
ITK-ImageFilterBase
ITK-ImageGradient
ITK-ImageGrid
ITK-ImageIntensity
ITK-ImageLabel
ITK-ImageStatistics
ITK-LabelVoting
ITK-MathematicalMorphology
ITK-Path
ITK-QuadEdgeMeshFiltering
ITK-Smoothing
ITK-SpatialFunction
ITK-Thresholding
)

set(Registration_module_list
ITK-FEMRegistration
ITK-PDEDeformableRegistration
ITK-RegistrationCommon
)

set(Segmentation_module_list
ITK-BioCell
ITK-Blox
ITK-Classifiers
ITK-ConnectedComponents
ITK-DeformableMesh
ITK-KLMRegionGrowing
ITK-LabelVoting
ITK-LevelSets
ITK-MarkovRandomFieldsClassifier
ITK-RegionGrowing
ITK-SignedDistanceFunction
ITK-Voronoi
ITK-Watersheds
)

set(Numerics_module_list
ITK-Eigen
ITK-FEM
ITK-NarrowBand
ITK-NeuralNetworks
ITK-Optimizers
ITK-Polynomials
ITK-Statistics
)


set(Bridge_module_list
ITK-VTK)

set(Utilities_module_list
ITK-KWSys
ITK-VNL
ITK-PNG
ITK-JPEG
ITK-Expat
ITK-NrrdIO
ITK-NIFTI
ITK-MetaIO
ITK-GDCM
ITK-OpenJPEG
ITK-ZLIB
ITK-VNLInstantiation
ITK-TIFF
)

set(Nonunit_module_list
ITK-IntegratedTest
ITK-Review
)
#------------------------------------------------
# Turn on the ITK_BUILD option for each group
option(ITKGroup_Core "Request building core modules" ON)
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
