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

#include "itkMesh.h"
#include "itkEllipseSpatialObject.h"

#include "itkGeodesicActiveContourLevelSetImageFilter.h"
#include "itkGradientVectorFlowImageFilter.h"
#include "itkHistogramMatchingImageFilter.h"
#include "itkImageClassifierBase.h"
#include "itkImageGaussianModelEstimator.h"
#include "itkImageKmeansModelEstimator.h"
#include "itkImageRegistrationMethod.h"
#include "itkImageToSpatialObjectRegistrationMethod.h"
#include "itkKLMRegionGrowImageFilter.h"
#include "itkLaplacianSegmentationLevelSetImageFilter.h"
#include "itkLevelSetVelocityNeighborhoodExtractor.h"

#include "itkDistanceToCentroidMembershipFunction.h"
#include "itkMahalanobisDistanceMembershipFunction.h"
#include "itkGroupSpatialObject.h"
#include "itkMeanReciprocalSquareDifferencePointSetToImageMetric.h"

int
main(int, char *[])
{
  using InputType = itk::Image<float, 2>;
  using OutputType = itk::Image<float, 2>;
  using UShortImageType = itk::Image<unsigned short, 2>;
  using VectorType = itk::Vector<float, 2>;
  using VectorImageType = itk::Image<VectorType, 2>;
  using PointSetType = itk::PointSet<float, 2>;

  // Used for GradientVectorFlowImageFilter
  using GradientType = itk::CovariantVector<double, 2>;
  using GradientImageType = itk::Image<GradientType, 2>;

  // Used for ImageKMeansModelEstimator
  using KMeansMemFuncType = itk::Statistics::DistanceToCentroidMembershipFunction<VectorType>;

  // Used for ImageGaussianModelEstimator
  using GaussianMemFuncType = itk::Statistics::MahalanobisDistanceMembershipFunction<VectorType>;

  // Used for ImageToSpatialObjectRegistrationMethod
  using GroupType = itk::GroupSpatialObject<2>;

  itk::GeodesicActiveContourLevelSetFunction<InputType>::Pointer GeodesicActiveContourLevelSetFunctionObj =
    itk::GeodesicActiveContourLevelSetFunction<InputType>::New();
  std::cout << "-------------GeodesicActiveContourLevelSetFunction " << GeodesicActiveContourLevelSetFunctionObj;

  itk::GeodesicActiveContourLevelSetImageFilter<InputType, OutputType>::Pointer
    GeodesicActiveContourLevelSetImageFilterObj =
      itk::GeodesicActiveContourLevelSetImageFilter<InputType, OutputType>::New();
  std::cout << "-------------GeodesicActiveContourLevelSetImageFilter " << GeodesicActiveContourLevelSetImageFilterObj;

  itk::GradientVectorFlowImageFilter<GradientImageType, GradientImageType>::Pointer GradientVectorFlowImageFilterObj =
    itk::GradientVectorFlowImageFilter<GradientImageType, GradientImageType>::New();
  std::cout << "-------------GradientVectorFlowImageFilter " << GradientVectorFlowImageFilterObj;

  itk::HistogramMatchingImageFilter<InputType, OutputType>::Pointer HistogramMatchingImageFilterObj =
    itk::HistogramMatchingImageFilter<InputType, OutputType>::New();
  std::cout << "-------------HistogramMatchingImageFilter " << HistogramMatchingImageFilterObj;

  itk::ImageClassifierBase<VectorImageType, OutputType>::Pointer ImageClassifierBaseObj =
    itk::ImageClassifierBase<VectorImageType, OutputType>::New();
  std::cout << "-------------ImageClassifierBase " << ImageClassifierBaseObj;

  itk::ImageGaussianModelEstimator<VectorImageType, GaussianMemFuncType, UShortImageType>::Pointer
    ImageGaussianModelEstimatorObj =
      itk::ImageGaussianModelEstimator<VectorImageType, GaussianMemFuncType, UShortImageType>::New();
  std::cout << "-------------ImageGaussianModelEstimator " << ImageGaussianModelEstimatorObj;

  itk::ImageKmeansModelEstimator<VectorImageType, KMeansMemFuncType>::Pointer ImageKmeansModelEstimatorObj =
    itk::ImageKmeansModelEstimator<VectorImageType, KMeansMemFuncType>::New();
  std::cout << "-------------ImageKmeansModelEstimator " << ImageKmeansModelEstimatorObj;

  itk::ImageRegistrationMethod<InputType, InputType>::Pointer ImageRegistrationMethodObj =
    itk::ImageRegistrationMethod<InputType, InputType>::New();
  std::cout << "-------------ImageRegistrationMethod " << ImageRegistrationMethodObj;

  itk::ImageToSpatialObjectRegistrationMethod<InputType, GroupType>::Pointer ImageToSpatialObjectRegistrationMethodObj =
    itk::ImageToSpatialObjectRegistrationMethod<InputType, GroupType>::New();
  std::cout << "-------------ImageToSpatialObjectRegistrationMethod " << ImageToSpatialObjectRegistrationMethodObj;

  itk::KLMRegionGrowImageFilter<VectorImageType, VectorImageType>::Pointer KLMRegionGrowImageFilterObj =
    itk::KLMRegionGrowImageFilter<VectorImageType, VectorImageType>::New();
  std::cout << "-------------KLMRegionGrowImageFilter " << KLMRegionGrowImageFilterObj;

  itk::LaplacianSegmentationLevelSetFunction<InputType, InputType>::Pointer LaplacianSegmentationLevelSetFunctionObj =
    itk::LaplacianSegmentationLevelSetFunction<InputType, InputType>::New();
  std::cout << "-------------LaplacianSegmentationLevelSetFunction " << LaplacianSegmentationLevelSetFunctionObj;

  itk::LaplacianSegmentationLevelSetImageFilter<InputType, InputType, float>::Pointer
    LaplacianSegmentationLevelSetImageFilterObj =
      itk::LaplacianSegmentationLevelSetImageFilter<InputType, InputType, float>::New();
  std::cout << "-------------LaplacianSegmentationLevelSetImageFilter " << LaplacianSegmentationLevelSetImageFilterObj;

  itk::LevelSetNeighborhoodExtractor<InputType>::Pointer LevelSetNeighborhoodExtractorObj =
    itk::LevelSetNeighborhoodExtractor<InputType>::New();
  std::cout << "-------------LevelSetNeighborhoodExtractor " << LevelSetNeighborhoodExtractorObj;

  itk::LevelSetVelocityNeighborhoodExtractor<InputType, double>::Pointer LevelSetVelocityNeighborhoodExtractorObj =
    itk::LevelSetVelocityNeighborhoodExtractor<InputType, double>::New();

  std::cout << "-------------LevelSetVelocityNeighborhoodExtractor " << LevelSetVelocityNeighborhoodExtractorObj;

  itk::MeanReciprocalSquareDifferencePointSetToImageMetric<PointSetType, InputType>::Pointer
    MeanReciprocalSquareDifferencePointSetToImageMetricObj =
      itk::MeanReciprocalSquareDifferencePointSetToImageMetric<PointSetType, InputType>::New();
  std::cout << "-------------MeanReciprocalSquareDifferencePointSetToImageMetric "
            << MeanReciprocalSquareDifferencePointSetToImageMetricObj;

  return EXIT_SUCCESS;
}
