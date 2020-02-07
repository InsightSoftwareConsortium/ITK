/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include "itkPointSet.h"
#include "itkBSplineScatteredDataPointSetToImageFilter.h"
#include "itkBSplineTransform.h"
#include "itkVectorIndexSelectionCastImageFilter.h"
#include "itkVectorLinearInterpolateImageFunction.h"
#include "itkTestingMacros.h"


/**
 * In this test, we approximate a 2-D scalar field.
 * The scattered data is derived from a segmented
 * image.  We write the output to an image for
 * comparison.
 */
int
itkBSplineScatteredDataPointSetToImageFilterTest4(int, char *[])
{
  constexpr unsigned int ParametricDimension = 3;
  constexpr unsigned int DataDimension = 3;
  constexpr unsigned int SplineOrder = 3;

  using RealType = float;
  using VectorType = itk::Vector<RealType, DataDimension>;
  using VectorImageType = itk::Image<VectorType, ParametricDimension>;
  using PointSetType = itk::PointSet<VectorImageType::PixelType, ParametricDimension>;
  using PointType = PointSetType::PointType;

  // Instantiate the B-spline filter

  using FilterType = itk::BSplineScatteredDataPointSetToImageFilter<PointSetType, VectorImageType>;

  VectorImageType::SizeType size;
  size.Fill(100);
  VectorImageType::PointType origin;
  origin.Fill(0);
  VectorImageType::SpacingType spacing;
  spacing.Fill(1);
  VectorImageType::DirectionType direction;
  direction.SetIdentity();

  // Instantiate example corresponding points with relative weighting

  PointSetType::Pointer pointSet = PointSetType::New();
  pointSet->Initialize();

  using WeightsContainerType = FilterType::WeightsContainerType;
  WeightsContainerType::Pointer weights = WeightsContainerType::New();
  weights->Initialize();

  // Create first landmark pair and weights
  PointType landmarkInFirstImage1;
  PointType landmarkInSecondImage1;

  landmarkInFirstImage1[0] = 4.0;
  landmarkInFirstImage1[1] = 4.0;
  landmarkInFirstImage1[2] = 4.0;

  landmarkInSecondImage1[0] = 5.0;
  landmarkInSecondImage1[1] = 5.0;
  landmarkInSecondImage1[2] = 5.0;

  RealType weight1 = 1.0;
  weights->InsertElement(0, weight1);

  VectorType vector1;
  for (unsigned int d = 0; d < DataDimension; d++)
  {
    vector1[d] = landmarkInSecondImage1[d] - landmarkInFirstImage1[d];
  }
  pointSet->SetPoint(0, landmarkInFirstImage1);
  pointSet->SetPointData(0, vector1);

  // Create second landmark pair
  PointType landmarkInFirstImage2;
  PointType landmarkInSecondImage2;

  landmarkInFirstImage2[0] = 25.0;
  landmarkInFirstImage2[1] = 25.0;
  landmarkInFirstImage2[2] = 25.0;

  landmarkInSecondImage2[0] = 30.0;
  landmarkInSecondImage2[1] = 35.0;
  landmarkInSecondImage2[2] = 45.0;

  RealType weight2 = 3.0;
  weights->InsertElement(1, weight2);

  VectorType vector2;
  for (unsigned int d = 0; d < DataDimension; d++)
  {
    vector2[d] = landmarkInSecondImage2[d] - landmarkInFirstImage2[d];
  }
  pointSet->SetPoint(1, landmarkInFirstImage2);
  pointSet->SetPointData(1, vector2);

  // Create third landmark pair
  PointType landmarkInFirstImage3;
  PointType landmarkInSecondImage3;

  landmarkInFirstImage3[0] = 25.0;
  landmarkInFirstImage3[1] = 25.0;
  landmarkInFirstImage3[2] = 25.0;

  landmarkInSecondImage3[0] = 30.0;
  landmarkInSecondImage3[1] = 35.0;
  landmarkInSecondImage3[2] = 45.0;

  RealType weight3 = 0.5;
  weights->InsertElement(2, weight3);

  VectorType vector3;
  for (unsigned int d = 0; d < DataDimension; d++)
  {
    vector3[d] = landmarkInSecondImage3[d] - landmarkInFirstImage3[d];
  }
  pointSet->SetPoint(2, landmarkInFirstImage3);
  pointSet->SetPointData(2, vector3);

  // Now fit the displacement

  FilterType::Pointer filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, BSplineScatteredDataPointSetToImageFilter, PointSetToImageFilter);

  // Define the parametric domain.
  filter->SetOrigin(origin);
  filter->SetSpacing(spacing);
  filter->SetSize(size);
  filter->SetDirection(direction);
  filter->SetInput(pointSet);
  filter->SetPointWeights(weights);

  // we normally don't need the sampled b-spline object
  // so we should turn this off but since we want to
  // compare, we need to have it on.
  // we only need the control point lattice to
  // set as input in the bspline transform.
  filter->SetGenerateOutputImage(true);

  // Define the rest of the B-spline parameters.
  // Note that some are transferred to the B-spline
  // transform.  Specifically, this includes the final
  // number of control points and the spline order.
  filter->SetSplineOrder(SplineOrder);
  FilterType::ArrayType ncps;
  ncps.Fill(4);
  filter->SetNumberOfControlPoints(ncps);
  filter->SetNumberOfLevels(3);
  FilterType::ArrayType close;
  close.Fill(0);
  filter->SetCloseDimension(close);


  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());


  // Instantiate the BSpline transform

  using TransformType = itk::BSplineTransform<float, DataDimension, SplineOrder>;
  TransformType::Pointer transform = TransformType::New();

  using CoefficientImageType = TransformType::ImageType;

  TransformType::CoefficientImageArray coefficientImages;
  for (unsigned int j = 0; j < DataDimension; j++)
  {
    using SelectorType = itk::VectorIndexSelectionCastImageFilter<VectorImageType, CoefficientImageType>;
    SelectorType::Pointer selector = SelectorType::New();
    selector->SetInput(filter->GetPhiLattice());
    selector->SetIndex(j);

    coefficientImages[j] = selector->GetOutput();
    coefficientImages[j]->Update();
    coefficientImages[j]->DisconnectPipeline();
  }

  transform->SetCoefficientImages(coefficientImages);

  using InputPointType = TransformType::InputPointType;
  using OutputPointType = TransformType::OutputPointType;

  InputPointType inputPoint;
  inputPoint.Fill(50.0);

  OutputPointType outputPoint = transform->TransformPoint(inputPoint);

  // Now instantiate an interpolator to get an approximation of what
  // the transform should produce

  using InterpolatorType = itk::VectorLinearInterpolateImageFunction<VectorImageType>;
  InterpolatorType::Pointer interpolator = InterpolatorType::New();
  interpolator->SetInputImage(filter->GetOutput());

  VectorImageType::PointType testPoint;
  testPoint.CastFrom(inputPoint);

  VectorType vector = interpolator->Evaluate(testPoint);
  RealType   testDistance = vector.GetNorm();
  RealType   approximateDistance = inputPoint.EuclideanDistanceTo(outputPoint);

  VectorImageType::PointType approximateOutputPoint;
  for (unsigned int d = 0; d < DataDimension; d++)
  {
    approximateOutputPoint[d] = testPoint[d] + vector[d];
  }

  std::cout << "Input point " << inputPoint << std::endl;
  std::cout << "True output point " << outputPoint << std::endl;
  std::cout << "Approximate output point " << approximateOutputPoint << std::endl;

  if (itk::Math::abs(testDistance - approximateDistance) > 0.0001)
  {
    std::cerr << "Didn't return correct distance." << std::endl;

    std::cerr << "true distance: " << testDistance << std::endl;
    std::cerr << "Approximate distance: " << approximateDistance << std::endl;

    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
