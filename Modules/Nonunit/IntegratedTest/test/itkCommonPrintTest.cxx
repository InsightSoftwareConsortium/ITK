/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/


#include "itkAcosImageAdaptor.h"
#include "itkAddImageAdaptor.h"
#include "itkAsinImageAdaptor.h"
#include "itkAtanImageAdaptor.h"
#include "itkAzimuthElevationToCartesianTransform.h"
#include "itkBSplineTransform.h"
#include "itkBSplineDerivativeKernelFunction.h"
#include "itkCenteredAffineTransform.h"
#include "itkCenteredRigid2DTransform.h"
#include "itkCenteredTransformInitializer.h"
#include "itkCentralDifferenceImageFunction.h"
#include "itkColorTable.h"
#include "itkConicShellInteriorExteriorSpatialFunction.h"
#include "itkCosImageAdaptor.h"
#include "itkElasticBodyReciprocalSplineKernelTransform.h"
#include "itkElasticBodySplineKernelTransform.h"
#include "itkEllipsoidInteriorExteriorSpatialFunction.h"
#include "itkEquivalencyTable.h"
#include "itkEuler2DTransform.h"
#include "itkEuler3DTransform.h"
#include "itkExpImageAdaptor.h"
#include "itkExpNegativeImageAdaptor.h"
#include "itkFiniteCylinderSpatialFunction.h"
#include "itkFrustumSpatialFunction.h"
#include "itkGaussianDerivativeImageFunction.h"
#include "itkGaussianKernelFunction.h"
#include "itkIdentityTransform.h"
#include "itkImageRegionSplitterMultidimensional.h"
#include "itkKLMSegmentationRegion.h"
#include "itkLevelSetFunction.h"
#include "itkLightProcessObject.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkLog10ImageAdaptor.h"
#include "itkLogImageAdaptor.h"
#include "itkMatrixResizeableDataObject.h"
#include "itkMaximumDecisionRule.h"
#include "itkMaximumRatioDecisionRule.h"
#include "itkMeanImageFunction.h"
#include "itkMedianImageFunction.h"
#include "itkMesh.h"
#include "itkMeshToMeshFilter.h"
#include "itkMinimumDecisionRule.h"
#include "itkMultiThreaderBase.h"
#include "itkNearestNeighborInterpolateImageFunction.h"
#include "itkNeighborhoodBinaryThresholdImageFunction.h"
#include "itkNthElementImageAdaptor.h"
#include "itkObjectStore.h"
#include "itkOneWayEquivalencyTable.h"
#include "itkProgressAccumulator.h"
#include "itkQuaternionRigidTransform.h"
#include "itkRGBToVectorImageAdaptor.h"
#include "itkRigid3DPerspectiveTransform.h"
#include "itkScaleSkewVersor3DTransform.h"
#include "itkScaleTransform.h"
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
#include "itkTranslationTransform.h"
#include "itkValarrayImageContainer.h"
#include "itkVarianceImageFunction.h"
#include "itkVectorLinearInterpolateImageFunction.h"
#include "itkVectorToRGBImageAdaptor.h"
#include "itkVectorImage.h"
#include "itkVersion.h"
#include "itkVolumeSplineKernelTransform.h"
#include "itkXMLFileOutputWindow.h"
#include "itkRedPixelAccessor.h"

struct TestObject
{
  float vector[3];
  int   counter;
};


int
itkCommonPrintTest(int, char *[])
{
  using InputType = itk::Image<float, 2>;
  using OutputType = itk::Image<float, 2>;

  using Point3DType = itk::Point<float, 3>;
  using PointType = itk::Point<float, 2>;
  using MeshType = itk::Mesh<PointType>;

  using VectorType = itk::Vector<float, 2>;
  using VectorImageType = itk::Image<VectorType, 2>;

  using RGBPixelType = itk::RGBPixel<unsigned short>;
  using RGBImageType = itk::Image<RGBPixelType, 2>;

  // Used for CenteredTransformInitializer
  using TransformType = itk::CenteredRigid2DTransform<float>;


  // Used for ImageAdaptor
  using RedAccessorType = itk::RedPixelAccessor<float>;

  const itk::AcosImageAdaptor<InputType, InputType>::Pointer AcosImageAdaptorObj =
    itk::AcosImageAdaptor<InputType, InputType>::New();
  std::cout << "------------AcosImageAdaptor" << AcosImageAdaptorObj;

  const itk::AddImageAdaptor<InputType>::Pointer AddImageAdaptorObj = itk::AddImageAdaptor<InputType>::New();
  std::cout << "------------AddImageAdaptor" << AddImageAdaptorObj;

  const itk::AffineTransform<float, 3>::Pointer AffineTransformObj = itk::AffineTransform<float, 3>::New();
  std::cout << "------------AffineTransform" << AffineTransformObj;

  const itk::AsinImageAdaptor<InputType, InputType>::Pointer AsinImageAdaptorObj =
    itk::AsinImageAdaptor<InputType, InputType>::New();
  std::cout << "------------AsinImageAdaptor" << AsinImageAdaptorObj;

  const itk::AtanImageAdaptor<InputType, InputType>::Pointer AtanImageAdaptorObj =
    itk::AtanImageAdaptor<InputType, InputType>::New();
  std::cout << "------------AtanImageAdaptor" << AtanImageAdaptorObj;

  const itk::AzimuthElevationToCartesianTransform<float, 3>::Pointer AzimuthElevationToCartesianTransformObj =
    itk::AzimuthElevationToCartesianTransform<float, 3>::New();
  std::cout << "------------AzimuthElevationToCartesianTransform" << AzimuthElevationToCartesianTransformObj;

  const itk::BSplineTransform<float, 3, 3>::Pointer BSplineTransformObj = itk::BSplineTransform<float, 3, 3>::New();
  std::cout << "------------BSplineTransform" << BSplineTransformObj;

  const itk::BSplineDerivativeKernelFunction<3>::Pointer BSplineDerivativeKernelFunctionObj =
    itk::BSplineDerivativeKernelFunction<3>::New();
  std::cout << "------------BSplineDerivativeKernelFunction" << BSplineDerivativeKernelFunctionObj;

  const itk::BSplineInterpolationWeightFunction<float, 2, 3>::Pointer BSplineInterpolationWeightFunctionObj =
    itk::BSplineInterpolationWeightFunction<float, 2, 3>::New();
  std::cout << "------------BSplineInterpolationWeightFunction" << BSplineInterpolationWeightFunctionObj;

  const itk::BSplineKernelFunction<3>::Pointer BSplineKernelFunctionObj = itk::BSplineKernelFunction<3>::New();
  std::cout << "------------BSplineKernelFunction" << BSplineKernelFunctionObj;

  const itk::BinaryThresholdImageFunction<InputType>::Pointer BinaryThresholdImageFunctionObj =
    itk::BinaryThresholdImageFunction<InputType>::New();
  std::cout << "------------BinaryThresholdImageFunction" << BinaryThresholdImageFunctionObj;

  const itk::BoundingBox<unsigned long>::Pointer BoundingBoxObj = itk::BoundingBox<unsigned long>::New();
  std::cout << "------------BoundingBox" << BoundingBoxObj;

  const itk::CenteredAffineTransform<double, 3>::Pointer CenteredAffineTransformObj =
    itk::CenteredAffineTransform<double, 3>::New();
  std::cout << "------------CenteredAffineTransform" << CenteredAffineTransformObj;

  const itk::CenteredRigid2DTransform<double>::Pointer CenteredRigid2DTransformObj =
    itk::CenteredRigid2DTransform<double>::New();
  std::cout << "------------CenteredRigid2DTransform" << CenteredRigid2DTransformObj;

  const itk::CenteredTransformInitializer<TransformType, InputType, InputType>::Pointer
    CenteredTransformInitializerObj = itk::CenteredTransformInitializer<TransformType, InputType, InputType>::New();
  std::cout << "------------CenteredTransformInitializer" << CenteredTransformInitializerObj;

  const itk::CentralDifferenceImageFunction<InputType>::Pointer CentralDifferenceImageFunctionObj =
    itk::CentralDifferenceImageFunction<InputType>::New();
  std::cout << "------------CentralDifferenceImageFunction" << CentralDifferenceImageFunctionObj;

  const itk::ColorTable<float>::Pointer ColorTableObj = itk::ColorTable<float>::New();
  std::cout << "------------ColorTable" << ColorTableObj;

  const itk::ConicShellInteriorExteriorSpatialFunction<3>::Pointer ConicShellInteriorExteriorSpatialFunctionObj =
    itk::ConicShellInteriorExteriorSpatialFunction<3>::New();
  std::cout << "------------ConicShellInteriorExteriorSpatialFunction" << ConicShellInteriorExteriorSpatialFunctionObj;

  const itk::CosImageAdaptor<InputType, InputType>::Pointer CosImageAdaptorObj =
    itk::CosImageAdaptor<InputType, InputType>::New();
  std::cout << "------------CosImageAdaptor" << CosImageAdaptorObj;

  const itk::ElasticBodyReciprocalSplineKernelTransform<double, 3>::Pointer
    ElasticBodyReciprocalSplineKernelTransformObj = itk::ElasticBodyReciprocalSplineKernelTransform<double, 3>::New();
  std::cout << "------------ElasticBodyReciprocalSplineKernelTransform"
            << ElasticBodyReciprocalSplineKernelTransformObj;

  const itk::ElasticBodySplineKernelTransform<double, 3>::Pointer ElasticBodySplineKernelTransformObj =
    itk::ElasticBodySplineKernelTransform<double, 3>::New();
  std::cout << "------------ElasticBodySplineKernelTransform" << ElasticBodySplineKernelTransformObj;

  const itk::EllipsoidInteriorExteriorSpatialFunction<2, PointType>::Pointer
    EllipsoidInteriorExteriorSpatialFunctionObj = itk::EllipsoidInteriorExteriorSpatialFunction<2, PointType>::New();
  std::cout << "------------EllipsoidInteriorExteriorSpatialFunction" << EllipsoidInteriorExteriorSpatialFunctionObj;

  const itk::EquivalencyTable::Pointer EquivalencyTableObj = itk::EquivalencyTable::New();
  std::cout << "-------------EquivalencyTable " << EquivalencyTableObj;

  const itk::Euler2DTransform<double>::Pointer Euler2DTransformObj = itk::Euler2DTransform<double>::New();
  std::cout << "------------Euler2DTransform" << Euler2DTransformObj;

  const itk::Euler3DTransform<double>::Pointer Euler3DTransformObj = itk::Euler3DTransform<double>::New();
  std::cout << "------------Euler3DTransform" << Euler3DTransformObj;

  const itk::ExpImageAdaptor<InputType, OutputType>::Pointer ExpImageAdaptorObj =
    itk::ExpImageAdaptor<InputType, OutputType>::New();
  std::cout << "------------ExpImageAdaptor" << ExpImageAdaptorObj;

  const itk::ExpNegativeImageAdaptor<InputType, OutputType>::Pointer ExpNegativeImageAdaptorObj =
    itk::ExpNegativeImageAdaptor<InputType, OutputType>::New();
  std::cout << "------------ExpNegativeImageAdaptor" << ExpNegativeImageAdaptorObj;

  const itk::FileOutputWindow::Pointer FileOutputWindowObj = itk::FileOutputWindow::New();
  std::cout << "------------FileOutputWindow" << FileOutputWindowObj;

  const itk::FiniteCylinderSpatialFunction<3, Point3DType>::Pointer FiniteCylinderSpatialFunctionObj =
    itk::FiniteCylinderSpatialFunction<3, Point3DType>::New();
  std::cout << "------------FiniteCylinderSpatialFunction" << FiniteCylinderSpatialFunctionObj;

  const itk::FrustumSpatialFunction<3, Point3DType>::Pointer FrustumSpatialFunctionObj =
    itk::FrustumSpatialFunction<3, Point3DType>::New();
  std::cout << "------------FrustumSpatialFunction" << FrustumSpatialFunctionObj;

  const itk::GaussianKernelFunction<double>::Pointer GaussianKernelFunctionObj =
    itk::GaussianKernelFunction<double>::New();
  std::cout << "------------GaussianKernelFunction" << GaussianKernelFunctionObj;

  const itk::GaussianSpatialFunction<float, 2, PointType>::Pointer GaussianSpatialFunctionObj =
    itk::GaussianSpatialFunction<float, 2, PointType>::New();
  std::cout << "------------GaussianSpatialFunction" << GaussianSpatialFunctionObj;

  const itk::IdentityTransform<double, 3>::Pointer IdentityTransformObj = itk::IdentityTransform<double, 3>::New();
  std::cout << "------------IdentityTransform" << IdentityTransformObj;

  const itk::Image<VectorType, 2>::Pointer ImageObj = itk::Image<VectorType, 2>::New();
  std::cout << "------------Image" << ImageObj;

  const itk::ImageAdaptor<RGBImageType, RedAccessorType>::Pointer ImageAdaptorObj =
    itk::ImageAdaptor<RGBImageType, RedAccessorType>::New();
  std::cout << "------------ImageAdaptor" << ImageAdaptorObj;

  const itk::ImageBase<3>::Pointer ImageBaseObj = itk::ImageBase<3>::New();
  std::cout << "------------ImageBase" << ImageBaseObj;

  const itk::ImageRegionSplitterMultidimensional::Pointer ImageRegionSplitterMultidimensionalObj =
    itk::ImageRegionSplitterMultidimensional::New();
  std::cout << "------------ImageRegionSplitterMultidimensional" << ImageRegionSplitterMultidimensionalObj;

  const itk::ImportImageContainer<unsigned long, PointType>::Pointer ImportImageContainerObj =
    itk::ImportImageContainer<unsigned long, PointType>::New();
  std::cout << "------------ImportImageContainer" << ImportImageContainerObj;

  const itk::KLMSegmentationBorder::Pointer KLMSegmentationBorderObj = itk::KLMSegmentationBorder::New();
  std::cout << "------------KLMSegmentationBorder" << KLMSegmentationBorderObj;

  const itk::KLMSegmentationRegion::Pointer KLMSegmentationRegionObj = itk::KLMSegmentationRegion::New();
  std::cout << "------------KLMSegmentationRegion" << KLMSegmentationRegionObj;

  const itk::KernelTransform<double, 3>::Pointer KernelTransformObj = itk::KernelTransform<double, 3>::New();
  std::cout << "------------KernelTransform" << KernelTransformObj;

  const itk::LevelSetFunction<InputType>::Pointer LevelSetFunctionObj = itk::LevelSetFunction<InputType>::New();
  std::cout << "------------LevelSetFunction" << LevelSetFunctionObj;

  const itk::LightProcessObject::Pointer LightProcessObjectObj = itk::LightProcessObject::New();
  std::cout << "------------LightProcessObject" << LightProcessObjectObj;

  const itk::LinearInterpolateImageFunction<InputType, float>::Pointer LinearInterpolateImageFunctionObj =
    itk::LinearInterpolateImageFunction<InputType, float>::New();
  std::cout << "------------LinearInterpolateImageFunction" << LinearInterpolateImageFunctionObj;

  const itk::Log10ImageAdaptor<InputType, OutputType>::Pointer Log10ImageAdaptorObj =
    itk::Log10ImageAdaptor<InputType, OutputType>::New();
  std::cout << "------------Log10ImageAdaptor" << Log10ImageAdaptorObj;

  const itk::LogImageAdaptor<InputType, OutputType>::Pointer LogImageAdaptorObj =
    itk::LogImageAdaptor<InputType, OutputType>::New();
  std::cout << "------------LogImageAdaptor" << LogImageAdaptorObj;

  const itk::MapContainer<unsigned long, PointType>::Pointer MapContainerObj =
    itk::MapContainer<unsigned long, PointType>::New();
  std::cout << "------------MapContainer" << MapContainerObj;

  const itk::MatrixResizeableDataObject<double>::Pointer MatrixResizeableDataObjectObj =
    itk::MatrixResizeableDataObject<double>::New();
  std::cout << "------------MatrixResizeableDataObject" << MatrixResizeableDataObjectObj;

  const itk::Statistics::MaximumDecisionRule::Pointer MaximumDecisionRuleObj =
    itk::Statistics::MaximumDecisionRule::New();
  std::cout << "------------MaximumDecisionRule" << MaximumDecisionRuleObj;

  const itk::Statistics::MaximumRatioDecisionRule::Pointer MaximumRatioDecisionRuleObj =
    itk::Statistics::MaximumRatioDecisionRule::New();
  std::cout << "------------MaximumRatioDecisionRule" << MaximumRatioDecisionRuleObj;

  const itk::MeanImageFunction<InputType, float>::Pointer MeanImageFunctionObj =
    itk::MeanImageFunction<InputType, float>::New();
  std::cout << "------------MeanImageFunction" << MeanImageFunctionObj;

  const itk::MedianImageFunction<InputType, float>::Pointer MedianImageFunctionObj =
    itk::MedianImageFunction<InputType, float>::New();
  std::cout << "------------MedianImageFunction" << MedianImageFunctionObj;

  const itk::Mesh<PointType, 2>::Pointer MeshObj = itk::Mesh<PointType, 2>::New();
  std::cout << "------------Mesh" << MeshObj;

  const itk::MeshSource<MeshType>::Pointer MeshSourceObj = itk::MeshSource<MeshType>::New();
  std::cout << "------------MeshSource" << MeshSourceObj;

  const itk::MeshToMeshFilter<MeshType, MeshType>::Pointer MeshToMeshFilterObj =
    itk::MeshToMeshFilter<MeshType, MeshType>::New();
  std::cout << "------------MeshToMeshFilter" << MeshToMeshFilterObj;

  const itk::Statistics::MinimumDecisionRule::Pointer MinimumDecisionRuleObj =
    itk::Statistics::MinimumDecisionRule::New();
  std::cout << "------------MinimumDecisionRule" << MinimumDecisionRuleObj;

  const itk::MultiThreaderBase::Pointer MultiThreaderObj = itk::MultiThreaderBase::New();
  std::cout << "------------MultiThreaderBase" << MultiThreaderObj;

  const itk::NearestNeighborInterpolateImageFunction<InputType, float>::Pointer
    NearestNeighborInterpolateImageFunctionObj = itk::NearestNeighborInterpolateImageFunction<InputType, float>::New();
  std::cout << "------------NearestNeighborInterpolateImageFunction" << NearestNeighborInterpolateImageFunctionObj;

  const itk::NeighborhoodBinaryThresholdImageFunction<InputType, float>::Pointer
    NeighborhoodBinaryThresholdImageFunctionObj =
      itk::NeighborhoodBinaryThresholdImageFunction<InputType, float>::New();
  std::cout << "------------NeighborhoodBinaryThresholdImageFunction" << NeighborhoodBinaryThresholdImageFunctionObj;

  const itk::NthElementImageAdaptor<InputType, PointType>::Pointer NthElementImageAdaptorObj =
    itk::NthElementImageAdaptor<InputType, PointType>::New();
  std::cout << "------------NthElementImageAdaptor" << NthElementImageAdaptorObj;

  const itk::ObjectStore<TestObject>::Pointer ObjectStoreObj = itk::ObjectStore<TestObject>::New();
  std::cout << "------------ObjectStore" << ObjectStoreObj;

  const itk::OneWayEquivalencyTable::Pointer OneWayEquivalencyTableObj = itk::OneWayEquivalencyTable::New();
  std::cout << "-------------OneWayEquivalencyTable " << OneWayEquivalencyTableObj;

  const itk::PointSet<PointType, 2>::Pointer PointSetObj = itk::PointSet<PointType, 2>::New();
  std::cout << "------------PointSet" << PointSetObj;

  const itk::ProgressAccumulator::Pointer ProgressAccumulatorObj = itk::ProgressAccumulator::New();
  std::cout << "------------ProgressAccumulator" << ProgressAccumulatorObj;

  const itk::QuaternionRigidTransform<double>::Pointer QuaternionRigidTransformObj =
    itk::QuaternionRigidTransform<double>::New();
  std::cout << "------------QuaternionRigidTransform" << QuaternionRigidTransformObj;

  const itk::RGBToVectorImageAdaptor<RGBImageType>::Pointer RGBToVectorImageAdaptorObj =
    itk::RGBToVectorImageAdaptor<RGBImageType>::New();
  std::cout << "------------RGBToVectorImageAdaptor" << RGBToVectorImageAdaptorObj;

  const itk::Rigid2DTransform<double>::Pointer Rigid2DTransformObj = itk::Rigid2DTransform<double>::New();
  std::cout << "------------Rigid2DTransform" << Rigid2DTransformObj;

  const itk::Rigid3DPerspectiveTransform<double>::Pointer Rigid3DPerspectiveTransformObj =
    itk::Rigid3DPerspectiveTransform<double>::New();
  std::cout << "------------Rigid3DPerspectiveTransform" << Rigid3DPerspectiveTransformObj;

  const itk::ScaleTransform<float, 3>::Pointer ScaleTransformObj = itk::ScaleTransform<float, 3>::New();
  std::cout << "------------ScaleTransform" << ScaleTransformObj;

  const itk::SegmentationBorder::Pointer SegmentationBorderObj = itk::SegmentationBorder::New();
  std::cout << "------------SegmentationBorder" << SegmentationBorderObj;

  const itk::SegmentationRegion::Pointer SegmentationRegionObj = itk::SegmentationRegion::New();
  std::cout << "------------SegmentationRegion" << SegmentationRegionObj;

  const itk::Similarity2DTransform<double>::Pointer Similarity2DTransformObj =
    itk::Similarity2DTransform<double>::New();
  std::cout << "------------Similarity2DTransform" << Similarity2DTransformObj;

  const itk::SinImageAdaptor<InputType, OutputType>::Pointer SinImageAdaptorObj =
    itk::SinImageAdaptor<InputType, OutputType>::New();
  std::cout << "------------SinImageAdaptor" << SinImageAdaptorObj;

  const itk::SphereSpatialFunction<2, PointType>::Pointer SphereSpatialFunctionObj =
    itk::SphereSpatialFunction<2, PointType>::New();
  std::cout << "------------SphereSpatialFunction" << SphereSpatialFunctionObj;

  const itk::SqrtImageAdaptor<InputType, OutputType>::Pointer SqrtImageAdaptorObj =
    itk::SqrtImageAdaptor<InputType, OutputType>::New();
  std::cout << "------------SqrtImageAdaptor" << SqrtImageAdaptorObj;

  const itk::SymmetricEllipsoidInteriorExteriorSpatialFunction<>::Pointer
    SymmetricEllipsoidInteriorExteriorSpatialFunctionObj =
      itk::SymmetricEllipsoidInteriorExteriorSpatialFunction<>::New();
  std::cout << "------------SymmetricEllipsoidInteriorExteriorSpatialFunction"
            << SymmetricEllipsoidInteriorExteriorSpatialFunctionObj;

  const itk::TanImageAdaptor<InputType, OutputType>::Pointer TanImageAdaptorObj =
    itk::TanImageAdaptor<InputType, OutputType>::New();
  std::cout << "------------TanImageAdaptor" << TanImageAdaptorObj;

  const itk::TextOutput::Pointer TextOutputObj = itk::TextOutput::New();
  std::cout << "------------TextOutput" << TextOutputObj;

  const itk::ThinPlateR2LogRSplineKernelTransform<double, 3>::Pointer ThinPlateR2LogRSplineKernelTransformObj =
    itk::ThinPlateR2LogRSplineKernelTransform<double, 3>::New();
  std::cout << "------------ThinPlateR2LogRSplineKernelTransform" << ThinPlateR2LogRSplineKernelTransformObj;

  const itk::ThinPlateSplineKernelTransform<double, 3>::Pointer ThinPlateSplineKernelTransformObj =
    itk::ThinPlateSplineKernelTransform<double, 3>::New();
  std::cout << "------------ThinPlateSplineKernelTransform" << ThinPlateSplineKernelTransformObj;

  const itk::TorusInteriorExteriorSpatialFunction<>::Pointer TorusInteriorExteriorSpatialFunctionObj =
    itk::TorusInteriorExteriorSpatialFunction<>::New();
  std::cout << "------------TorusInteriorExteriorSpatialFunction" << TorusInteriorExteriorSpatialFunctionObj;

  const itk::TranslationTransform<double, 3>::Pointer TranslationTransformObj =
    itk::TranslationTransform<double, 3>::New();
  std::cout << "------------TranslationTransform" << TranslationTransformObj;

  const itk::ValarrayImageContainer<unsigned long, PointType>::Pointer ValarrayImageContainerObj =
    itk::ValarrayImageContainer<unsigned long, PointType>::New();
  std::cout << "------------ValarrayImageContainer" << ValarrayImageContainerObj;

  const itk::VarianceImageFunction<InputType, float>::Pointer VarianceImageFunctionObj =
    itk::VarianceImageFunction<InputType, float>::New();
  std::cout << "------------VarianceImageFunction" << VarianceImageFunctionObj;

  const itk::VectorContainer<PointType>::Pointer VectorContainerObj = itk::VectorContainer<PointType>::New();
  std::cout << "------------VectorContainer" << VectorContainerObj;

  const itk::VectorImage<float, 3>::Pointer VectorImageObj = itk::VectorImage<float, 3>::New();
  std::cout << "------------VectorImage" << VectorImageObj;

  const itk::VectorLinearInterpolateImageFunction<VectorImageType, float>::Pointer
    VectorLinearInterpolateImageFunctionObj = itk::VectorLinearInterpolateImageFunction<VectorImageType, float>::New();
  std::cout << "------------VectorLinearInterpolateImageFunction" << VectorLinearInterpolateImageFunctionObj;

  const itk::VectorToRGBImageAdaptor<VectorImageType>::Pointer VectorToRGBImageAdaptorObj =
    itk::VectorToRGBImageAdaptor<VectorImageType>::New();
  std::cout << "------------VectorToRGBImageAdaptor" << VectorToRGBImageAdaptorObj;

  const itk::Version::Pointer VersionObj = itk::Version::New();
  std::cout << "------------Version" << VersionObj;

  const itk::ScaleSkewVersor3DTransform<double>::Pointer ScaleSkewVersor3DTransformObj =
    itk::ScaleSkewVersor3DTransform<double>::New();
  std::cout << "------------ScaleSkewVersor3DTransform" << ScaleSkewVersor3DTransformObj;

  const itk::VersorRigid3DTransform<double>::Pointer VersorRigid3DTransformObj =
    itk::VersorRigid3DTransform<double>::New();
  std::cout << "------------VersorRigid3DTransform" << VersorRigid3DTransformObj;

  const itk::VersorTransform<double>::Pointer VersorTransformObj = itk::VersorTransform<double>::New();
  std::cout << "------------VersorTransform" << VersorTransformObj;

  const itk::VolumeSplineKernelTransform<double, 3>::Pointer VolumeSplineKernelTransformObj =
    itk::VolumeSplineKernelTransform<double, 3>::New();
  std::cout << "------------VolumeSplineKernelTransform" << VolumeSplineKernelTransformObj;

  const itk::XMLFileOutputWindow::Pointer XMLFileOutputWindowObj = itk::XMLFileOutputWindow::New();
  std::cout << "------------XMLFileOutputWindow" << XMLFileOutputWindowObj;


  return EXIT_SUCCESS;
}
