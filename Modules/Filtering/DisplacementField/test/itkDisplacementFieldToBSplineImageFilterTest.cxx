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

#include "itkDisplacementFieldToBSplineImageFilter.h"
#include "itkTestingMacros.h"

int
itkDisplacementFieldToBSplineImageFilterTest(int, char *[])
{
  constexpr unsigned int ImageDimension = 2;

  using VectorType = itk::Vector<float, ImageDimension>;
  using DisplacementFieldType = itk::Image<VectorType, ImageDimension>;
  using PointSetType = itk::PointSet<VectorType, ImageDimension>;

  // Create a displacement field
  DisplacementFieldType::PointType     origin;
  DisplacementFieldType::SpacingType   spacing;
  DisplacementFieldType::SizeType      size;
  DisplacementFieldType::DirectionType direction;

  direction.SetIdentity();
  origin.Fill(0.0);
  spacing.Fill(0.5);
  size.Fill(100);

  auto ones = itk::MakeFilled<VectorType>(1);

  auto field = DisplacementFieldType::New();
  field->SetOrigin(origin);
  field->SetSpacing(spacing);
  field->SetRegions(size);
  field->SetDirection(direction);
  field->Allocate();
  field->FillBuffer(ones);

  using BSplineFilterType = itk::DisplacementFieldToBSplineImageFilter<DisplacementFieldType, PointSetType>;
  using RealImageType = BSplineFilterType::RealImageType;

  auto confidenceImage = RealImageType::New();
  confidenceImage->CopyInformation(field);
  confidenceImage->SetRegions(size);
  confidenceImage->Allocate();
  confidenceImage->FillBuffer(1.0);

  auto pointSet = PointSetType::New();
  pointSet->Initialize();

  auto ones_points = itk::MakeFilled<VectorType>(1.0);

  // Assign some random points within the b-spline domain
  PointSetType::PointType point1;
  point1[0] = 23.75;
  point1[1] = 5.125;
  pointSet->SetPoint(0, point1);
  pointSet->SetPointData(0, ones_points);

  PointSetType::PointType point2;
  point2[0] = 1.75;
  point2[1] = 45.125;
  pointSet->SetPoint(1, point2);
  pointSet->SetPointData(1, ones_points);

  PointSetType::PointType point3;
  point3[0] = 45.75;
  point3[1] = 2.125;
  pointSet->SetPoint(2, point3);
  pointSet->SetPointData(2, ones_points);

  BSplineFilterType::ArrayType numberOfControlPoints;
  numberOfControlPoints.Fill(4);

  auto bspliner = BSplineFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(bspliner, DisplacementFieldToBSplineImageFilter, ImageToImageFilter);


  bspliner->SetDisplacementField(field);
  ITK_TEST_SET_GET_VALUE(field, bspliner->GetDisplacementField());

  bspliner->SetConfidenceImage(confidenceImage);
  ITK_TEST_SET_GET_VALUE(confidenceImage, bspliner->GetConfidenceImage());

  bspliner->SetPointSet(pointSet);
  ITK_TEST_SET_GET_VALUE(pointSet, bspliner->GetPointSet());

  ITK_TEST_SET_GET_BOOLEAN(bspliner, UseInputFieldToDefineTheBSplineDomain, true);

  bspliner->SetNumberOfControlPoints(numberOfControlPoints);
  ITK_TEST_SET_GET_VALUE(numberOfControlPoints, bspliner->GetNumberOfControlPoints());

  unsigned int splineOrder = 3;
  bspliner->SetSplineOrder(splineOrder);
  ITK_TEST_SET_GET_VALUE(splineOrder, bspliner->GetSplineOrder());

  typename BSplineFilterType::ArrayType::ValueType numberOfFittingLevelsVal = 8;
  typename BSplineFilterType::ArrayType            numberOfFittingLevels;
  numberOfFittingLevels.Fill(numberOfFittingLevelsVal);
  bspliner->SetNumberOfFittingLevels(numberOfFittingLevelsVal);
  ITK_TEST_SET_GET_VALUE(numberOfFittingLevels, bspliner->GetNumberOfFittingLevels());

  bspliner->SetNumberOfFittingLevels(numberOfFittingLevels);
  ITK_TEST_SET_GET_VALUE(numberOfFittingLevels, bspliner->GetNumberOfFittingLevels());

  ITK_TEST_SET_GET_BOOLEAN(bspliner, EnforceStationaryBoundary, false);

  ITK_TEST_SET_GET_BOOLEAN(bspliner, EstimateInverse, false);

  typename BSplineFilterType::OriginType::ValueType bSplineDomainOriginVal = 0.0;
  typename BSplineFilterType::OriginType            bSplineDomainOrigin;
  bSplineDomainOrigin.Fill(bSplineDomainOriginVal);
  ITK_TEST_EXPECT_EQUAL(bSplineDomainOrigin, bspliner->GetBSplineDomainOrigin());

  typename BSplineFilterType::SpacingType::ValueType bSplineDomainSpacingVal = 1.0;
  typename BSplineFilterType::SpacingType            bSplineDomainSpacing;
  bSplineDomainSpacing.Fill(bSplineDomainSpacingVal);
  ITK_TEST_EXPECT_EQUAL(bSplineDomainSpacing, bspliner->GetBSplineDomainSpacing());

  typename BSplineFilterType::SizeType::value_type bSplineDomainSizeVal = 0;
  typename BSplineFilterType::SizeType             bSplineDomainSize;
  bSplineDomainSize.Fill(bSplineDomainSizeVal);
  ITK_TEST_EXPECT_EQUAL(bSplineDomainSize, bspliner->GetBSplineDomainSize());

  typename BSplineFilterType::DirectionType bSplineDomainDirection;
  bSplineDomainDirection.SetIdentity();
  ITK_TEST_EXPECT_EQUAL(bSplineDomainDirection, bspliner->GetBSplineDomainDirection());

  try
  {
    bspliner->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Exception thrown " << std::endl;
    std::cerr << excp << std::endl;
  }

  bSplineDomainOrigin = field->GetOrigin();
  ITK_TEST_EXPECT_EQUAL(bSplineDomainOrigin, bspliner->GetBSplineDomainOrigin());

  bSplineDomainSpacing = field->GetSpacing();
  ITK_TEST_EXPECT_EQUAL(bSplineDomainSpacing, bspliner->GetBSplineDomainSpacing());

  bSplineDomainSize = field->GetRequestedRegion().GetSize();
  ITK_TEST_EXPECT_EQUAL(bSplineDomainSize, bspliner->GetBSplineDomainSize());

  bSplineDomainDirection = field->GetDirection();
  ITK_TEST_EXPECT_EQUAL(bSplineDomainDirection, bspliner->GetBSplineDomainDirection());

  DisplacementFieldType::IndexType index;
  index[0] = 50;
  index[1] = 50;

  VectorType v = bspliner->GetOutput()->GetPixel(index);

  if (itk::Math::abs(v.GetNorm() - 1.414214) >= 0.01)
  {
    std::cerr << "Failed to find the correct forward displacement vector." << std::endl;
    return EXIT_FAILURE;
  }

  numberOfFittingLevelsVal = 5;
  bspliner->SetNumberOfFittingLevels(numberOfFittingLevelsVal);
  bspliner->EstimateInverseOn();

  bspliner->Update();

  v = bspliner->GetOutput()->GetPixel(index);

  if (itk::Math::abs(v.GetNorm() - 1.414214) >= 0.01)
  {
    std::cerr << "Failed to find the correct inverse displacement vector." << std::endl;
    return EXIT_FAILURE;
  }

  /** do a second run using only the point set. */

  auto bspliner2 = BSplineFilterType::New();
  bspliner2->SetPointSet(pointSet);

  bspliner2->UseInputFieldToDefineTheBSplineDomainOff();

  bspliner2->SetBSplineDomainFromImage(field);

  ITK_TEST_EXPECT_EQUAL(bSplineDomainOrigin, bspliner2->GetBSplineDomainOrigin());
  ITK_TEST_EXPECT_EQUAL(bSplineDomainSpacing, bspliner2->GetBSplineDomainSpacing());
  ITK_TEST_EXPECT_EQUAL(bSplineDomainSize, bspliner2->GetBSplineDomainSize());
  ITK_TEST_EXPECT_EQUAL(bSplineDomainDirection, bspliner2->GetBSplineDomainDirection());

  bspliner2->SetNumberOfControlPoints(numberOfControlPoints);
  bspliner2->SetSplineOrder(splineOrder);

  numberOfFittingLevelsVal = 8;
  bspliner2->SetNumberOfFittingLevels(numberOfFittingLevelsVal);

  bspliner2->EnforceStationaryBoundaryOff();

  bspliner2->EstimateInverseOff();

  try
  {
    bspliner2->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Exception thrown " << std::endl;
    std::cerr << excp << std::endl;
  }

  return EXIT_SUCCESS;
}
