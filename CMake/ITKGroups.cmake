# Set a list of group names
set(group_list Core IO Filtering Registration Segmentation Numerics ThirdParty Bridge Nonunit)

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

set(Core_documentation "This group of modules contain the toolkit framework used
by other modules.  There are common base classes for data objects and process
objects, basic data structures such as Image, Mesh, QuadEdgeMesh, and
SpatialObjects, and common functionality for operations such as finite
differences, image adaptors, or image transforms.")

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

set(IO_documentation "This group of modules contains classes for reading and
writing images and other data objects.")

set(Filtering_module_list
ITKAnisotropicSmoothing
ITKAntiAlias
ITKBiasCorrection
ITKConnectedComponents
ITKConvolution
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
ITKBinaryMathematicalMorphology
ITKPath
ITKQuadEdgeMeshFiltering
ITKSmoothing
ITKSpatialFunction
ITKThresholding
ITKLabelMap
ITKImageFusion
)

set(Filtering_documentation "This group of modules are filters that modify data
in the ITK pipeline framework.  These filters take an input object, such as an
Image, and modify it to create an output.  Filters can be chained together to
create a processing pipeline.")

set(Registration_module_list
ITKFEMRegistration
ITKPDEDeformableRegistration
ITKRegistrationCommon
)

set(Registration_documentation "This group of modules address the registration
problem: find the spatial transformation between two images.  This is a high
level group that makes use of many lower level modules such as \\ref
ITKTransform, \\ref ITKOptimizers, \\ref ITKFiniteDifference, and \\ref
ITKFEM.")

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

set(Segmentation_documentation "This group of modules address the segmentation
problem: partition the image into classified regions (labels).  This is a high
level group that makes use of many lower level modules such as \\ref
ITKQuadEdgeMesh and \\ref ITKNarrowBand.")

set(Numerics_module_list
ITKEigen
ITKFEM
ITKNarrowBand
ITKNeuralNetworks
ITKOptimizers
ITKPolynomials
ITKStatistics
)

set(Numerics_documentation "This group of modules are basic numerical tools and
algorithms that have general applications outside of imaging.")

set(Bridge_module_list
ITKVtkGlue
ITKVTK)

set(Bridge_documentation "This group of modules are intended to bridge ITK to
other toolkits as libraries such as visualization toolkits.")

set(ThirdParty_module_list
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

set(ThirdParty_documentation "This group of modules are third party libraries
used by other ITK modules.")

set(Nonunit_module_list
ITKDeprecated
ITKIntegratedTest
ITKReview
)

set(Nonunit_documentation "This group of modules are intended to make use of an
extensive set of the toolkit modules.")

#------------------------------------------------
#------------------------------------------------
set( group_list_dox )
foreach(group ${group_list} )
  set( group_list_dox
"${group_list_dox}
// -----------------------------------------------
// Group ${group}
/** \\defgroup Group-${group} Group ${group}
${${group}_documentation} */\n"
    )

  foreach(mod ${${group}_module_list} )
    set( group_list_dox
"${group_list_dox}
/** \\defgroup ${mod} Module ${mod}
\\ingroup Group-${group} */\n"
      )
  endforeach()
endforeach()

set( _content ${group_list_dox} )
configure_file(
  "${ITK_SOURCE_DIR}/Utilities/Doxygen/Module.dox.in"
  "${ITK_BINARY_DIR}/Utilities/Doxygen/Modules/ITK-AllGroups.dox"
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
