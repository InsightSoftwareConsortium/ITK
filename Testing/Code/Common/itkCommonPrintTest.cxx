/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCommonPrintTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkImage.h"
#include "itkVector.h"
#include "itkPoint.h"
#include "itkMesh.h"

#include "itkAcosImageAdaptor.h"
#include "itkAddImageAdaptor.h"
#include "itkAffineTransform.h"
#include "itkAsinImageAdaptor.h"
#include "itkAtanImageAdaptor.h"
#include "itkAzimuthElevationToCartesianTransform.h"
#include "itkBSplineDeformableTransform.h"
#include "itkBSplineDerivativeKernelFunction.h"
#include "itkBSplineInterpolationWeightFunction.h"
#include "itkBSplineKernelFunction.h"
#include "itkBinaryThresholdImageFunction.h"
#include "itkBloxBoundaryPointImage.h"
#include "itkBloxBoundaryProfileImage.h"
#include "itkBloxCoreAtomImage.h"
#include "itkBloxImage.h"
#include "itkBoundingBox.h"
#include "itkCellInterfaceVisitor.h"
#include "itkCenteredAffineTransform.h"
#include "itkCenteredRigid2DTransform.h"
#include "itkCenteredTransformInitializer.h"
#include "itkCentralDifferenceImageFunction.h"
#include "itkColorTable.h"
#include "itkCommand.h"
#include "itkConicShellInteriorExteriorSpatialFunction.h"
#include "itkCosImageAdaptor.h"
#include "itkCreateObjectFunction.h"
#include "itkDifferenceImageFilter.h"
#include "itkDynamicLoader.h"
#include "itkElasticBodyReciprocalSplineKernelTransform.h"
#include "itkElasticBodySplineKernelTransform.h"
#include "itkEllipsoidInteriorExteriorSpatialFunction.h"
#include "itkEuler2DTransform.h"
#include "itkEuler3DTransform.h"
#include "itkExpImageAdaptor.h"
#include "itkExpNegativeImageAdaptor.h"
#include "itkFastMutexLock.h"
#include "itkFileOutputWindow.h"
#include "itkFrustumSpatialFunction.h"
#include "itkGaussianKernelFunction.h"
#include "itkGaussianSpatialFunction.h"
#include "itkIdentityTransform.h"
#include "itkImage.h"
#include "itkImageAdaptor.h"
#include "itkImageBase.h"
#include "itkImageRegionMultidimensionalSplitter.h"
#include "itkImageRegionSplitter.h"
#include "itkImportImageContainer.h"
#include "itkKLMSegmentationBorder.h"
#include "itkKLMSegmentationRegion.h"
#include "itkKernelTransform.h"
#include "itkLevelSetFunction.h"
#include "itkLightProcessObject.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkLog10ImageAdaptor.h"
#include "itkLogImageAdaptor.h"
#include "itkMapContainer.h"
#include "itkMaximumDecisionRule.h"
#include "itkMaximumRatioDecisionRule.h"
#include "itkMeanImageFunction.h"
#include "itkMedianImageFunction.h"
#include "itkMesh.h"
#include "itkMeshSource.h"
#include "itkMeshToMeshFilter.h"
#include "itkMetaDataObject.h"
#include "itkMinimumDecisionRule.h"
#include "itkMultiThreader.h"
#include "itkMutexLock.h"
#include "itkNearestNeighborInterpolateImageFunction.h"
#include "itkNeighborhoodBinaryThresholdImageFunction.h"
#include "itkNthElementImageAdaptor.h"
#include "itkObjectStore.h"
#include "itkPointLocator.h"
#include "itkPointSet.h"
#include "itkQuaternionRigidTransform.h"
#include "itkRGBToVectorImageAdaptor.h"
#include "itkRigid2DTransform.h"
#include "itkRigid3DPerspectiveTransform.h"
#include "itkRigid3DTransform.h"
#include "itkScaleTransform.h"
#include "itkSegmentationBorder.h"
#include "itkSegmentationRegion.h"
#include "itkSimilarity2DTransform.h"
#include "itkSinImageAdaptor.h"
#include "itkSphereSpatialFunction.h"
#include "itkSqrtImageAdaptor.h"
#include "itkSymmetricEllipsoidInteriorExteriorSpatialFunction.h"
#include "itkTanImageAdaptor.h"
#include "itkTextOutput.h"
#include "itkThinPlateR2LogRSplineKernelTransform.h"
#include "itkThinPlateSplineKernelTransform.h"
#include "itkTorusInteriorExteriorSpatialFunction.h"
#include "itkTransform.h"
#include "itkTranslationTransform.h"
#include "itkValarrayImageContainer.h"
#include "itkVarianceImageFunction.h"
#include "itkVectorContainer.h"
#include "itkVectorLinearInterpolateImageFunction.h"
#include "itkVectorToRGBImageAdaptor.h"
#include "itkVersion.h"
#include "itkVersorRigid3DTransform.h"
#include "itkVersorTransform.h"
#include "itkVolumeSplineKernelTransform.h"
#include "itkWin32OutputWindow.h"
#include "itkXMLFileOutputWindow.h"

int itkCommonPrintTest(int , char* [])
{
  typedef itk::Image<float,2> InputType;
  typedef itk::Image<unsigned char,2> CharType;
  typedef itk::Image<float,2> OutputType;
  typedef itk::Point<float,2> MeshPixelType;
  typedef itk::Mesh<MeshPixelType>  MeshType;
  typedef itk::Vector<float,2> VectorType;
  typedef itk::Image<VectorType, 2> VectorImageType;
    itk::AcosImageAdaptor<InputType,InputType>::Pointer AcosImageAdaptorObj =
      itk::AcosImageAdaptor<InputType,InputType>::New();
    std::cout << "------------AcosImageAdaptor" << AcosImageAdaptorObj;

    itk::AddImageAdaptor<InputType>::Pointer AddImageAdaptorObj =
      itk::AddImageAdaptor<InputType>::New();
    std::cout << "------------AddImageAdaptor" << AddImageAdaptorObj;

    itk::AffineTransform<float,3>::Pointer AffineTransformObj =
      itk::AffineTransform<float,3>::New();
    std::cout << "------------AffineTransform" << AffineTransformObj;

    itk::AsinImageAdaptor<InputType,InputType>::Pointer AsinImageAdaptorObj =
      itk::AsinImageAdaptor<InputType,InputType>::New();
    std::cout << "------------AsinImageAdaptor" << AsinImageAdaptorObj;

    itk::AtanImageAdaptor<InputType,InputType>::Pointer AtanImageAdaptorObj =
      itk::AtanImageAdaptor<InputType,InputType>::New();
    std::cout << "------------AtanImageAdaptor" << AtanImageAdaptorObj;

    itk::AzimuthElevationToCartesianTransform<float>::Pointer AzimuthElevationToCartesianTransformObj =
      itk::AzimuthElevationToCartesianTransform<float>::New();
    std::cout << "------------AzimuthElevationToCartesianTransform" << AzimuthElevationToCartesianTransformObj;

    itk::BSplineDeformableTransform<float,3,3>::Pointer BSplineDeformableTransformObj =
      itk::BSplineDeformableTransform<float,3,3>::New();
    std::cout << "------------BSplineDeformableTransform" << BSplineDeformableTransformObj;

    itk::BSplineDerivativeKernelFunction<3>::Pointer BSplineDerivativeKernelFunctionObj =
      itk::BSplineDerivativeKernelFunction<3>::New();
    std::cout << "------------BSplineDerivativeKernelFunction" << BSplineDerivativeKernelFunctionObj;

    itk::BSplineInterpolationWeightFunction<float,2,3>::Pointer BSplineInterpolationWeightFunctionObj =
      itk::BSplineInterpolationWeightFunction<float,2,3>::New();
    std::cout << "------------BSplineInterpolationWeightFunction" << BSplineInterpolationWeightFunctionObj;

    itk::BSplineKernelFunction<3>::Pointer BSplineKernelFunctionObj =
      itk::BSplineKernelFunction<3>::New();
    std::cout << "------------BSplineKernelFunction" << BSplineKernelFunctionObj;

    itk::BinaryThresholdImageFunction<InputType>::Pointer BinaryThresholdImageFunctionObj =
      itk::BinaryThresholdImageFunction<InputType>::New();
    std::cout << "------------BinaryThresholdImageFunction" << BinaryThresholdImageFunctionObj;

#if 0
    itk::BloxBoundaryPointImage<foo>::Pointer BloxBoundaryPointImageObj =
      itk::BloxBoundaryPointImage<foo>::New();
    std::cout << "------------BloxBoundaryPointImage" << BloxBoundaryPointImageObj;

    itk::BloxBoundaryProfileImage<foo>::Pointer BloxBoundaryProfileImageObj =
      itk::BloxBoundaryProfileImage<foo>::New();
    std::cout << "------------BloxBoundaryProfileImage" << BloxBoundaryProfileImageObj;

    itk::BloxCoreAtomImage<foo>::Pointer BloxCoreAtomImageObj =
      itk::BloxCoreAtomImage<foo>::New();
    std::cout << "------------BloxCoreAtomImage" << BloxCoreAtomImageObj;

    itk::BloxImage<foo>::Pointer BloxImageObj =
      itk::BloxImage<foo>::New();
    std::cout << "------------BloxImage" << BloxImageObj;
#endif

    itk::BoundingBox<unsigned long>::Pointer BoundingBoxObj =
      itk::BoundingBox<unsigned long>::New();
    std::cout << "------------BoundingBox" << BoundingBoxObj;

    itk::CenteredAffineTransform<double,3>::Pointer CenteredAffineTransformObj =
      itk::CenteredAffineTransform<double,3>::New();
    std::cout << "------------CenteredAffineTransform" << CenteredAffineTransformObj;

    itk::CenteredRigid2DTransform<double>::Pointer CenteredRigid2DTransformObj =
      itk::CenteredRigid2DTransform<double>::New();
    std::cout << "------------CenteredRigid2DTransform" << CenteredRigid2DTransformObj;

    typedef itk::CenteredRigid2DTransform<float> TransformType;
    itk::CenteredTransformInitializer<TransformType,InputType,InputType>::Pointer CenteredTransformInitializerObj =
      itk::CenteredTransformInitializer<TransformType,InputType,InputType>::New();
    std::cout << "------------CenteredTransformInitializer" << CenteredTransformInitializerObj;

    itk::CentralDifferenceImageFunction<InputType>::Pointer CentralDifferenceImageFunctionObj =
      itk::CentralDifferenceImageFunction<InputType>::New();
    std::cout << "------------CentralDifferenceImageFunction" << CentralDifferenceImageFunctionObj;

    itk::ColorTable<float>::Pointer ColorTableObj =
      itk::ColorTable<float>::New();
    std::cout << "------------ColorTable" << ColorTableObj;

    itk::ConicShellInteriorExteriorSpatialFunction<3>::Pointer ConicShellInteriorExteriorSpatialFunctionObj =
      itk::ConicShellInteriorExteriorSpatialFunction<3>::New();
    std::cout << "------------ConicShellInteriorExteriorSpatialFunction" << ConicShellInteriorExteriorSpatialFunctionObj;

    itk::CosImageAdaptor<InputType,InputType>::Pointer CosImageAdaptorObj =
      itk::CosImageAdaptor<InputType,InputType>::New();
    std::cout << "------------CosImageAdaptor" << CosImageAdaptorObj;

    itk::DifferenceImageFilter<InputType,OutputType>::Pointer DifferenceImageFilterObj =
      itk::DifferenceImageFilter<InputType,OutputType>::New();
    std::cout << "------------DifferenceImageFilter" << DifferenceImageFilterObj;

    itk::DynamicLoader::Pointer DynamicLoaderObj =
      itk::DynamicLoader::New();
    std::cout << "------------DynamicLoader" << DynamicLoaderObj;

#if 0
    itk::ElasticBodyReciprocalSplineKernelTransform<foo>::Pointer ElasticBodyReciprocalSplineKernelTransformObj =
      itk::ElasticBodyReciprocalSplineKernelTransform<foo>::New();
    std::cout << "------------ElasticBodyReciprocalSplineKernelTransform" << ElasticBodyReciprocalSplineKernelTransformObj;

    itk::ElasticBodySplineKernelTransform<foo>::Pointer ElasticBodySplineKernelTransformObj =
      itk::ElasticBodySplineKernelTransform<foo>::New();
    std::cout << "------------ElasticBodySplineKernelTransform" << ElasticBodySplineKernelTransformObj;

    itk::EllipsoidInteriorExteriorSpatialFunction<foo>::Pointer EllipsoidInteriorExteriorSpatialFunctionObj =
      itk::EllipsoidInteriorExteriorSpatialFunction<foo>::New();
    std::cout << "------------EllipsoidInteriorExteriorSpatialFunction" << EllipsoidInteriorExteriorSpatialFunctionObj;

    itk::Euler2DTransform<foo>::Pointer Euler2DTransformObj =
      itk::Euler2DTransform<foo>::New();
    std::cout << "------------Euler2DTransform" << Euler2DTransformObj;

    itk::Euler3DTransform<foo>::Pointer Euler3DTransformObj =
      itk::Euler3DTransform<foo>::New();
    std::cout << "------------Euler3DTransform" << Euler3DTransformObj;

    itk::ExpImageAdaptor<foo>::Pointer ExpImageAdaptorObj =
      itk::ExpImageAdaptor<foo>::New();
    std::cout << "------------ExpImageAdaptor" << ExpImageAdaptorObj;

    itk::ExpNegativeImageAdaptor<foo>::Pointer ExpNegativeImageAdaptorObj =
      itk::ExpNegativeImageAdaptor<foo>::New();
    std::cout << "------------ExpNegativeImageAdaptor" << ExpNegativeImageAdaptorObj;

    itk::FastMutexLock<foo>::Pointer FastMutexLockObj =
      itk::FastMutexLock<foo>::New();
    std::cout << "------------FastMutexLock" << FastMutexLockObj;

    itk::FileOutputWindow<foo>::Pointer FileOutputWindowObj =
      itk::FileOutputWindow<foo>::New();
    std::cout << "------------FileOutputWindow" << FileOutputWindowObj;

    itk::FrustumSpatialFunction<foo>::Pointer FrustumSpatialFunctionObj =
      itk::FrustumSpatialFunction<foo>::New();
    std::cout << "------------FrustumSpatialFunction" << FrustumSpatialFunctionObj;

    itk::GaussianKernelFunction<foo>::Pointer GaussianKernelFunctionObj =
      itk::GaussianKernelFunction<foo>::New();
    std::cout << "------------GaussianKernelFunction" << GaussianKernelFunctionObj;

    itk::GaussianSpatialFunction<foo>::Pointer GaussianSpatialFunctionObj =
      itk::GaussianSpatialFunction<foo>::New();
    std::cout << "------------GaussianSpatialFunction" << GaussianSpatialFunctionObj;

    itk::IdentityTransform<foo>::Pointer IdentityTransformObj =
      itk::IdentityTransform<foo>::New();
    std::cout << "------------IdentityTransform" << IdentityTransformObj;

    itk::Image<foo>::Pointer ImageObj =
      itk::Image<foo>::New();
    std::cout << "------------Image" << ImageObj;

    itk::ImageAdaptor<foo>::Pointer ImageAdaptorObj =
      itk::ImageAdaptor<foo>::New();
    std::cout << "------------ImageAdaptor" << ImageAdaptorObj;

    itk::ImageBase<foo>::Pointer ImageBaseObj =
      itk::ImageBase<foo>::New();
    std::cout << "------------ImageBase" << ImageBaseObj;

    itk::ImageRegionMultidimensionalSplitter<foo>::Pointer ImageRegionMultidimensionalSplitterObj =
      itk::ImageRegionMultidimensionalSplitter<foo>::New();
    std::cout << "------------ImageRegionMultidimensionalSplitter" << ImageRegionMultidimensionalSplitterObj;

    itk::ImageRegionSplitter<foo>::Pointer ImageRegionSplitterObj =
      itk::ImageRegionSplitter<foo>::New();
    std::cout << "------------ImageRegionSplitter" << ImageRegionSplitterObj;

    itk::ImportImageContainer<foo>::Pointer ImportImageContainerObj =
      itk::ImportImageContainer<foo>::New();
    std::cout << "------------ImportImageContainer" << ImportImageContainerObj;

    itk::KLMSegmentationBorder<foo>::Pointer KLMSegmentationBorderObj =
      itk::KLMSegmentationBorder<foo>::New();
    std::cout << "------------KLMSegmentationBorder" << KLMSegmentationBorderObj;

    itk::KLMSegmentationRegion<foo>::Pointer KLMSegmentationRegionObj =
      itk::KLMSegmentationRegion<foo>::New();
    std::cout << "------------KLMSegmentationRegion" << KLMSegmentationRegionObj;

    itk::KernelTransform<foo>::Pointer KernelTransformObj =
      itk::KernelTransform<foo>::New();
    std::cout << "------------KernelTransform" << KernelTransformObj;

    itk::LevelSetFunction<foo>::Pointer LevelSetFunctionObj =
      itk::LevelSetFunction<foo>::New();
    std::cout << "------------LevelSetFunction" << LevelSetFunctionObj;

    itk::LightProcessObject<foo>::Pointer LightProcessObjectObj =
      itk::LightProcessObject<foo>::New();
    std::cout << "------------LightProcessObject" << LightProcessObjectObj;

    itk::LinearInterpolateImageFunction<foo>::Pointer LinearInterpolateImageFunctionObj =
      itk::LinearInterpolateImageFunction<foo>::New();
    std::cout << "------------LinearInterpolateImageFunction" << LinearInterpolateImageFunctionObj;

    itk::Log10ImageAdaptor<foo>::Pointer Log10ImageAdaptorObj =
      itk::Log10ImageAdaptor<foo>::New();
    std::cout << "------------Log10ImageAdaptor" << Log10ImageAdaptorObj;

    itk::LogImageAdaptor<foo>::Pointer LogImageAdaptorObj =
      itk::LogImageAdaptor<foo>::New();
    std::cout << "------------LogImageAdaptor" << LogImageAdaptorObj;

    itk::MapContainer<foo>::Pointer MapContainerObj =
      itk::MapContainer<foo>::New();
    std::cout << "------------MapContainer" << MapContainerObj;

    itk::MaximumDecisionRule<foo>::Pointer MaximumDecisionRuleObj =
      itk::MaximumDecisionRule<foo>::New();
    std::cout << "------------MaximumDecisionRule" << MaximumDecisionRuleObj;

    itk::MaximumRatioDecisionRule<foo>::Pointer MaximumRatioDecisionRuleObj =
      itk::MaximumRatioDecisionRule<foo>::New();
    std::cout << "------------MaximumRatioDecisionRule" << MaximumRatioDecisionRuleObj;

    itk::MeanImageFunction<foo>::Pointer MeanImageFunctionObj =
      itk::MeanImageFunction<foo>::New();
    std::cout << "------------MeanImageFunction" << MeanImageFunctionObj;

    itk::MedianImageFunction<foo>::Pointer MedianImageFunctionObj =
      itk::MedianImageFunction<foo>::New();
    std::cout << "------------MedianImageFunction" << MedianImageFunctionObj;

    itk::Mesh<foo>::Pointer MeshObj =
      itk::Mesh<foo>::New();
    std::cout << "------------Mesh" << MeshObj;

    itk::MeshSource<foo>::Pointer MeshSourceObj =
      itk::MeshSource<foo>::New();
    std::cout << "------------MeshSource" << MeshSourceObj;

    itk::MeshToMeshFilter<foo>::Pointer MeshToMeshFilterObj =
      itk::MeshToMeshFilter<foo>::New();
    std::cout << "------------MeshToMeshFilter" << MeshToMeshFilterObj;

    itk::MetaDataObject<foo>::Pointer MetaDataObjectObj =
      itk::MetaDataObject<foo>::New();
    std::cout << "------------MetaDataObject" << MetaDataObjectObj;

    itk::MinimumDecisionRule<foo>::Pointer MinimumDecisionRuleObj =
      itk::MinimumDecisionRule<foo>::New();
    std::cout << "------------MinimumDecisionRule" << MinimumDecisionRuleObj;

    itk::MultiThreader<foo>::Pointer MultiThreaderObj =
      itk::MultiThreader<foo>::New();
    std::cout << "------------MultiThreader" << MultiThreaderObj;

    itk::MutexLock<foo>::Pointer MutexLockObj =
      itk::MutexLock<foo>::New();
    std::cout << "------------MutexLock" << MutexLockObj;

    itk::NearestNeighborInterpolateImageFunction<foo>::Pointer NearestNeighborInterpolateImageFunctionObj =
      itk::NearestNeighborInterpolateImageFunction<foo>::New();
    std::cout << "------------NearestNeighborInterpolateImageFunction" << NearestNeighborInterpolateImageFunctionObj;

    itk::NeighborhoodBinaryThresholdImageFunction<foo>::Pointer NeighborhoodBinaryThresholdImageFunctionObj =
      itk::NeighborhoodBinaryThresholdImageFunction<foo>::New();
    std::cout << "------------NeighborhoodBinaryThresholdImageFunction" << NeighborhoodBinaryThresholdImageFunctionObj;

    itk::NthElementImageAdaptor<foo>::Pointer NthElementImageAdaptorObj =
      itk::NthElementImageAdaptor<foo>::New();
    std::cout << "------------NthElementImageAdaptor" << NthElementImageAdaptorObj;

    itk::ObjectStore<foo>::Pointer ObjectStoreObj =
      itk::ObjectStore<foo>::New();
    std::cout << "------------ObjectStore" << ObjectStoreObj;

    itk::PointLocator<foo>::Pointer PointLocatorObj =
      itk::PointLocator<foo>::New();
    std::cout << "------------PointLocator" << PointLocatorObj;

    itk::PointSet<foo>::Pointer PointSetObj =
      itk::PointSet<foo>::New();
    std::cout << "------------PointSet" << PointSetObj;

    itk::QuaternionRigidTransform<foo>::Pointer QuaternionRigidTransformObj =
      itk::QuaternionRigidTransform<foo>::New();
    std::cout << "------------QuaternionRigidTransform" << QuaternionRigidTransformObj;

    itk::RGBToVectorImageAdaptor<foo>::Pointer RGBToVectorImageAdaptorObj =
      itk::RGBToVectorImageAdaptor<foo>::New();
    std::cout << "------------RGBToVectorImageAdaptor" << RGBToVectorImageAdaptorObj;

    itk::Rigid2DTransform<foo>::Pointer Rigid2DTransformObj =
      itk::Rigid2DTransform<foo>::New();
    std::cout << "------------Rigid2DTransform" << Rigid2DTransformObj;

    itk::Rigid3DPerspectiveTransform<foo>::Pointer Rigid3DPerspectiveTransformObj =
      itk::Rigid3DPerspectiveTransform<foo>::New();
    std::cout << "------------Rigid3DPerspectiveTransform" << Rigid3DPerspectiveTransformObj;

    itk::Rigid3DTransform<foo>::Pointer Rigid3DTransformObj =
      itk::Rigid3DTransform<foo>::New();
    std::cout << "------------Rigid3DTransform" << Rigid3DTransformObj;

    itk::ScaleTransform<foo>::Pointer ScaleTransformObj =
      itk::ScaleTransform<foo>::New();
    std::cout << "------------ScaleTransform" << ScaleTransformObj;

    itk::SegmentationBorder<foo>::Pointer SegmentationBorderObj =
      itk::SegmentationBorder<foo>::New();
    std::cout << "------------SegmentationBorder" << SegmentationBorderObj;

    itk::SegmentationRegion<foo>::Pointer SegmentationRegionObj =
      itk::SegmentationRegion<foo>::New();
    std::cout << "------------SegmentationRegion" << SegmentationRegionObj;

    itk::Similarity2DTransform<foo>::Pointer Similarity2DTransformObj =
      itk::Similarity2DTransform<foo>::New();
    std::cout << "------------Similarity2DTransform" << Similarity2DTransformObj;

    itk::SinImageAdaptor<foo>::Pointer SinImageAdaptorObj =
      itk::SinImageAdaptor<foo>::New();
    std::cout << "------------SinImageAdaptor" << SinImageAdaptorObj;

    itk::SphereSpatialFunction<foo>::Pointer SphereSpatialFunctionObj =
      itk::SphereSpatialFunction<foo>::New();
    std::cout << "------------SphereSpatialFunction" << SphereSpatialFunctionObj;

    itk::SqrtImageAdaptor<foo>::Pointer SqrtImageAdaptorObj =
      itk::SqrtImageAdaptor<foo>::New();
    std::cout << "------------SqrtImageAdaptor" << SqrtImageAdaptorObj;

    itk::SymmetricEllipsoidInteriorExteriorSpatialFunction<foo>::Pointer SymmetricEllipsoidInteriorExteriorSpatialFunctionObj =
      itk::SymmetricEllipsoidInteriorExteriorSpatialFunction<foo>::New();
    std::cout << "------------SymmetricEllipsoidInteriorExteriorSpatialFunction" << SymmetricEllipsoidInteriorExteriorSpatialFunctionObj;

    itk::TanImageAdaptor<foo>::Pointer TanImageAdaptorObj =
      itk::TanImageAdaptor<foo>::New();
    std::cout << "------------TanImageAdaptor" << TanImageAdaptorObj;

    itk::TextOutput<foo>::Pointer TextOutputObj =
      itk::TextOutput<foo>::New();
    std::cout << "------------TextOutput" << TextOutputObj;

    itk::ThinPlateR2LogRSplineKernelTransform<foo>::Pointer ThinPlateR2LogRSplineKernelTransformObj =
      itk::ThinPlateR2LogRSplineKernelTransform<foo>::New();
    std::cout << "------------ThinPlateR2LogRSplineKernelTransform" << ThinPlateR2LogRSplineKernelTransformObj;

    itk::ThinPlateSplineKernelTransform<foo>::Pointer ThinPlateSplineKernelTransformObj =
      itk::ThinPlateSplineKernelTransform<foo>::New();
    std::cout << "------------ThinPlateSplineKernelTransform" << ThinPlateSplineKernelTransformObj;

    itk::TorusInteriorExteriorSpatialFunction<foo>::Pointer TorusInteriorExteriorSpatialFunctionObj =
      itk::TorusInteriorExteriorSpatialFunction<foo>::New();
    std::cout << "------------TorusInteriorExteriorSpatialFunction" << TorusInteriorExteriorSpatialFunctionObj;

    itk::Transform<foo>::Pointer TransformObj =
      itk::Transform<foo>::New();
    std::cout << "------------Transform" << TransformObj;

    itk::TranslationTransform<foo>::Pointer TranslationTransformObj =
      itk::TranslationTransform<foo>::New();
    std::cout << "------------TranslationTransform" << TranslationTransformObj;

    itk::ValarrayImageContainer<foo>::Pointer ValarrayImageContainerObj =
      itk::ValarrayImageContainer<foo>::New();
    std::cout << "------------ValarrayImageContainer" << ValarrayImageContainerObj;

    itk::VarianceImageFunction<foo>::Pointer VarianceImageFunctionObj =
      itk::VarianceImageFunction<foo>::New();
    std::cout << "------------VarianceImageFunction" << VarianceImageFunctionObj;

    itk::VectorContainer<foo>::Pointer VectorContainerObj =
      itk::VectorContainer<foo>::New();
    std::cout << "------------VectorContainer" << VectorContainerObj;

    itk::VectorLinearInterpolateImageFunction<foo>::Pointer VectorLinearInterpolateImageFunctionObj =
      itk::VectorLinearInterpolateImageFunction<foo>::New();
    std::cout << "------------VectorLinearInterpolateImageFunction" << VectorLinearInterpolateImageFunctionObj;

    itk::VectorToRGBImageAdaptor<foo>::Pointer VectorToRGBImageAdaptorObj =
      itk::VectorToRGBImageAdaptor<foo>::New();
    std::cout << "------------VectorToRGBImageAdaptor" << VectorToRGBImageAdaptorObj;

    itk::Version<foo>::Pointer VersionObj =
      itk::Version<foo>::New();
    std::cout << "------------Version" << VersionObj;

    itk::VersorRigid3DTransform<foo>::Pointer VersorRigid3DTransformObj =
      itk::VersorRigid3DTransform<foo>::New();
    std::cout << "------------VersorRigid3DTransform" << VersorRigid3DTransformObj;

    itk::VersorTransform<foo>::Pointer VersorTransformObj =
      itk::VersorTransform<foo>::New();
    std::cout << "------------VersorTransform" << VersorTransformObj;

    itk::VolumeSplineKernelTransform<foo>::Pointer VolumeSplineKernelTransformObj =
      itk::VolumeSplineKernelTransform<foo>::New();
    std::cout << "------------VolumeSplineKernelTransform" << VolumeSplineKernelTransformObj;

    itk::Win32OutputWindow<foo>::Pointer Win32OutputWindowObj =
      itk::Win32OutputWindow<foo>::New();
    std::cout << "------------Win32OutputWindow" << Win32OutputWindowObj;

    itk::XMLFileOutputWindow<foo>::Pointer XMLFileOutputWindowObj =
      itk::XMLFileOutputWindow<foo>::New();
    std::cout << "------------XMLFileOutputWindow" << XMLFileOutputWindowObj;
#endif

  return 0;
}
