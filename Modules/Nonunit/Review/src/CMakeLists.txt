set(ITKReview_SRC
itkBruker2DSEQImageIO.cxx
itkBruker2DSEQImageIOFactory.cxx
itkGaborKernelFunction.cxx
itkGeometryUtilities.cxx
itkJPEG2000ImageIO.cxx
itkJPEG2000ImageIOFactory.cxx
itkMRCHeaderObject.cxx
itkMRCImageIO.cxx
itkMRCImageIOFactory.cxx
itkVoxBoCUBImageIO.cxx
itkVoxBoCUBImageIOFactory.cxx
)

add_library(ITKReview ${ITKReview_SRC})
target_link_libraries(ITKReview  ${ITKVTK_LIBRARIES} ${ITKCommon_LIBRARIES} ${ITKFiniteDifference_LIBRARIES} ${ITKImageAdaptors_LIBRARIES} ${ITKImageFunction_LIBRARIES} ${ITKImageGrid_LIBRARIES} ${ITKImageStatistics_LIBRARIES} ${ITKMesh_LIBRARIES} ${ITKQuadEdgeMesh_LIBRARIES} ${ITKSpatialObjects_LIBRARIES} ${ITKTestKernel_LIBRARIES} ${ITKTransform_LIBRARIES} ${ITKAnisotropicSmoothing_LIBRARIES} ${ITKAntiAlias_LIBRARIES} ${ITKBiasCorrection_LIBRARIES} ${ITKConnectedComponents_LIBRARIES} ${ITKCurvatureFlow_LIBRARIES} ${ITKDeformationField_LIBRARIES} ${ITKDiffusionTensorImage_LIBRARIES} ${ITKDistanceMap_LIBRARIES} ${ITKFFT_LIBRARIES} ${ITKImageCompare_LIBRARIES} ${ITKImageCompose_LIBRARIES} ${ITKImageFeature_LIBRARIES} ${ITKImageFilterBase_LIBRARIES} ${ITKImageGradient_LIBRARIES} ${ITKImageGrid_LIBRARIES} ${ITKImageIntensity_LIBRARIES} ${ITKImageLabel_LIBRARIES} ${ITKImageStatistics_LIBRARIES} ${ITKLabelVoting_LIBRARIES} ${ITKLevelSets_LIBRARIES} ${ITKMathematicalMorphology_LIBRARIES} ${ITKPath_LIBRARIES} ${ITKQuadEdgeMeshFiltering_LIBRARIES} ${ITKSmoothing_LIBRARIES} ${ITKSpatialFunction_LIBRARIES} ${ITKThresholding_LIBRARIES} ${ITKIOBase_LIBRARIES} ${ITKIOBioRad_LIBRARIES} ${ITKIOBMP_LIBRARIES} ${ITKIOGDCM_LIBRARIES} ${ITKIOGE_LIBRARIES} ${ITKIOGIPL_LIBRARIES} ${ITKIOIPL_LIBRARIES} ${ITKIOJPEG_LIBRARIES} ${ITKIOLSM_LIBRARIES} ${ITKIOMeta_LIBRARIES} ${ITKIONIFTI_LIBRARIES} ${ITKIONRRD_LIBRARIES} ${ITKIOPNG_LIBRARIES} ${ITKIORAW_LIBRARIES} ${ITKIOSiemens_LIBRARIES} ${ITKIOSpatialObjects_LIBRARIES} ${ITKIOStimulate_LIBRARIES} ${ITKIOTIFF_LIBRARIES} ${ITKIOVTK_LIBRARIES} ${ITKIOXML_LIBRARIES} ${ITKEigen_LIBRARIES} ${ITKFEM_LIBRARIES} ${ITKNarrowBand_LIBRARIES} ${ITKNeuralNetworks_LIBRARIES} ${ITKOptimizers_LIBRARIES} ${ITKPolynomials_LIBRARIES} ${ITKStatistics_LIBRARIES} ${ITKFEMRegistration_LIBRARIES} ${ITKPDEDeformableRegistration_LIBRARIES} ${ITKRegistrationCommon_LIBRARIES} ${ITKBioCell_LIBRARIES} ${ITKClassifiers_LIBRARIES} ${ITKConnectedComponents_LIBRARIES} ${ITKDeformableMesh_LIBRARIES} ${ITKKLMRegionGrowing_LIBRARIES} ${ITKLabelVoting_LIBRARIES} ${ITKLevelSets_LIBRARIES} ${ITKMarkovRandomFieldsClassifiers_LIBRARIES} ${ITKRegionGrowing_LIBRARIES} ${ITKSignedDistanceFunction_LIBRARIES} ${ITKVoronoi_LIBRARIES} ${ITKWatersheds_LIBRARIES} itkopenjpeg)
itk_module_target(ITKReview)

if(ITK_USE_MINC2 AND MINC2_FOUND)
  set(ITKMINC2_SRCS
    itkMINC2ImageIO.cxx
    itkMINC2ImageIOFactory.cxx
    )
  add_library(ITKIOMINC2 ${ITKMINC2_SRCS})
  target_link_libraries(ITKIOMINC2 ${ITKIOBase_LIBRARIES} ${MINC2_LIB} ${HDF5_LIB} ${NETCDF_LIB})
  itk_module_target(ITKIOMINC2)
endif(ITK_USE_MINC2 AND MINC2_FOUND)
