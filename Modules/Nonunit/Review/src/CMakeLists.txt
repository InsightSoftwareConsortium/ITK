set(ITK-Review_SRC
itkBruker2DSEQImageIO.cxx
itkBruker2DSEQImageIOFactory.cxx
itkGaborKernelFunction.cxx
itkGeometryUtilities.cxx
itkJPEG2000ImageIO.cxx
itkJPEG2000ImageIOFactory.cxx
itkMRCHeaderObject.cxx
itkMRCImageIO.cxx
itkMRCImageIOFactory.cxx
itkPhilipsPAR.cxx
itkPhilipsRECImageIO.cxx
itkPhilipsRECImageIOFactory.cxx
itkVoxBoCUBImageIO.cxx
itkVoxBoCUBImageIOFactory.cxx
)

add_library(ITK-Review ${ITK-Review_SRC})
target_link_libraries(ITK-Review  ${ITK-VTK_LIBRARIES} ${ITK-Common_LIBRARIES} ${ITK-FiniteDifference_LIBRARIES} ${ITK-ImageAdaptors_LIBRARIES} ${ITK-ImageFunction_LIBRARIES} ${ITK-ImageGrid_LIBRARIES} ${ITK-ImageStatistics_LIBRARIES} ${ITK-Mesh_LIBRARIES} ${ITK-QuadEdgeMesh_LIBRARIES} ${ITK-SpatialObjects_LIBRARIES} ${ITK-TestKernel_LIBRARIES} ${ITK-Transform_LIBRARIES} ${ITK-AnisotropicSmoothing_LIBRARIES} ${ITK-AntiAlias_LIBRARIES} ${ITK-BiasCorrection_LIBRARIES} ${ITK-ConnectedComponents_LIBRARIES} ${ITK-CurvatureFlow_LIBRARIES} ${ITK-DeformationField_LIBRARIES} ${ITK-DiffusionTensorImage_LIBRARIES} ${ITK-DistanceMap_LIBRARIES} ${ITK-FFT_LIBRARIES} ${ITK-ImageCompare_LIBRARIES} ${ITK-ImageCompose_LIBRARIES} ${ITK-ImageFeature_LIBRARIES} ${ITK-ImageFilterBase_LIBRARIES} ${ITK-ImageGradient_LIBRARIES} ${ITK-ImageGrid_LIBRARIES} ${ITK-ImageIntensity_LIBRARIES} ${ITK-ImageLabel_LIBRARIES} ${ITK-ImageStatistics_LIBRARIES} ${ITK-LabelVoting_LIBRARIES} ${ITK-LevelSets_LIBRARIES} ${ITK-MathematicalMorphology_LIBRARIES} ${ITK-Path_LIBRARIES} ${ITK-QuadEdgeMeshFiltering_LIBRARIES} ${ITK-Smoothing_LIBRARIES} ${ITK-SpatialFunction_LIBRARIES} ${ITK-Thresholding_LIBRARIES} ${ITK-IO-Base_LIBRARIES} ${ITK-IO-BioRad_LIBRARIES} ${ITK-IO-BMP_LIBRARIES} ${ITK-IO-GDCM_LIBRARIES} ${ITK-IO-GE_LIBRARIES} ${ITK-IO-GIPL_LIBRARIES} ${ITK-IO-IPL_LIBRARIES} ${ITK-IO-JPEG_LIBRARIES} ${ITK-IO-LSM_LIBRARIES} ${ITK-IO-Meta_LIBRARIES} ${ITK-IO-NIFTI_LIBRARIES} ${ITK-IO-NRRD_LIBRARIES} ${ITK-IO-PNG_LIBRARIES} ${ITK-IO-RAW_LIBRARIES} ${ITK-IO-Siemens_LIBRARIES} ${ITK-IO-SpatialObjects_LIBRARIES} ${ITK-IO-Stimulate_LIBRARIES} ${ITK-IO-TIFF_LIBRARIES} ${ITK-IO-VTK_LIBRARIES} ${ITK-IO-XML_LIBRARIES} ${ITK-Eigen_LIBRARIES} ${ITK-FEM_LIBRARIES} ${ITK-NarrowBand_LIBRARIES} ${ITK-NeuralNetworks_LIBRARIES} ${ITK-Optimizers_LIBRARIES} ${ITK-Polynomials_LIBRARIES} ${ITK-Statistics_LIBRARIES} ${ITK-FEMRegistration_LIBRARIES} ${ITK-PDEDeformableRegistration_LIBRARIES} ${ITK-RegistrationCommon_LIBRARIES} ${ITK-BioCell_LIBRARIES} ${ITK-Classifiers_LIBRARIES} ${ITK-ConnectedComponents_LIBRARIES} ${ITK-DeformableMesh_LIBRARIES} ${ITK-KLMRegionGrowing_LIBRARIES} ${ITK-LabelVoting_LIBRARIES} ${ITK-LevelSets_LIBRARIES} ${ITK-MarkovRandomFieldsClassifiers_LIBRARIES} ${ITK-RegionGrowing_LIBRARIES} ${ITK-SignedDistanceFunction_LIBRARIES} ${ITK-Voronoi_LIBRARIES} ${ITK-Watersheds_LIBRARIES} itkopenjpeg)
itk_module_target(ITK-Review)

if(ITK_USE_MINC2 AND MINC2_FOUND)
  set(ITKMINC2_SRCS
    itkMINC2ImageIO.cxx
    itkMINC2ImageIOFactory.cxx
    )
  add_library(ITKIOMINC2 ${ITKMINC2_SRCS})
  target_link_libraries(ITKIOMINC2 ${ITK-IO-Base_LIBRARIES} ${MINC2_LIB} ${HDF5_LIB} ${NETCDF_LIB})
  itk_module_target(ITKIOMINC2)
endif(ITK_USE_MINC2 AND MINC2_FOUND)
