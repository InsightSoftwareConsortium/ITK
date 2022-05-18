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

#include "itkTranslationTransform.h"

#include "itkANTSNeighborhoodCorrelationImageToImageMetricv4.h"
#include "itkTestingMacros.h"
#include "itkMath.h"

/**
 * Test program for ANTSNeighborhoodCorrelationImageToImageMetricv4,
 * using a synthetic image and initial displacement.
 *
 */

template <typename ImagePointerType, typename DerivativeType>
void
ANTSNeighborhoodCorrelationImageToImageMetricv4Test_PrintDerivativeAsVectorImage(ImagePointerType   image,
                                                                                 DerivativeType &   derivative,
                                                                                 itk::SizeValueType vecdim)
{

  using ImageType = typename ImagePointerType::ObjectType;
  typename ImageType::RegionType imageRegion = image->GetBufferedRegion();

  // only display the first slice
  itk::SizeValueType dim0 = imageRegion.GetSize()[0];
  itk::SizeValueType dim1 = imageRegion.GetSize()[1];

  using IteratorType = itk::ImageRegionConstIterator<ImageType>;
  IteratorType it(image, imageRegion);
  it.GoToBegin();
  itk::SizeValueType cnt = 0;
  for (itk::SizeValueType ycnt = 0; ycnt < dim1; ++ycnt)
  {
    for (itk::SizeValueType xcnt = 0; xcnt < dim0; ++xcnt)
    {
      std::cout << '[';
      for (itk::SizeValueType d = 0; d < vecdim - 1; ++d)
      {
        std::cout << derivative[cnt * vecdim + d] << ",";
      }
      std::cout << derivative[cnt * vecdim + vecdim - 1] << ']' << "\t";
      ++it;
      ++cnt;
    }
    std::cout << std::endl;
  }
}


template <typename ImageType>
void
ANTSNeighborhoodCorrelationImageToImageMetricv4Test_PrintImage(ImageType * imageP)
{

  using ImageConstPointerType = typename ImageType::ConstPointer;
  ImageConstPointerType image = imageP;

  typename ImageType::RegionType imageRegion = image->GetBufferedRegion();

  // only display the first slice
  itk::SizeValueType dim0 = imageRegion.GetSize()[0];
  itk::SizeValueType dim1 = imageRegion.GetSize()[1];

  using IteratorType = itk::ImageRegionConstIterator<ImageType>;
  IteratorType it(image, imageRegion);
  it.GoToBegin();

  for (itk::SizeValueType ycnt = 0; ycnt < dim1; ++ycnt)
  {
    for (itk::SizeValueType xcnt = 0; xcnt < dim0; ++xcnt)
    {
      std::cout << it.Get() << "\t";
      ++it;
    }
    std::cout << std::endl;
  }
}

template <typename ImagePointerType>
void
ANTSNeighborhoodCorrelationImageToImageMetricv4Test_PrintImage(const ImagePointerType & image)
{

  using ImageType = typename ImagePointerType::ObjectType;
  typename ImageType::RegionType imageRegion = image->GetBufferedRegion();

  // only display the first slice
  itk::SizeValueType dim0 = imageRegion.GetSize()[0];
  itk::SizeValueType dim1 = imageRegion.GetSize()[1];

  using IteratorType = itk::ImageRegionConstIterator<ImageType>;
  IteratorType it(image, imageRegion);
  it.GoToBegin();

  for (itk::SizeValueType ycnt = 0; ycnt < dim1; ++ycnt)
  {
    for (itk::SizeValueType xcnt = 0; xcnt < dim0; ++xcnt)
    {
      std::cout << it.Get() << "\t";
      ++it;
    }
    std::cout << std::endl;
  }
}

int
itkANTSNeighborhoodCorrelationImageToImageMetricv4Test(int, char ** const)
{
  constexpr itk::SizeValueType ImageDimension = 2;

  using ImageType = itk::Image<double, ImageDimension>;
  using VectorType = itk::Vector<double, ImageDimension>;

  using IdentityTransformType = itk::IdentityTransform<double, ImageDimension>;
  using CompositeTransformType = itk::CompositeTransform<double, ImageDimension>;
  using TranslationTransformType = itk::TranslationTransform<double, ImageDimension>;
  using DisplacementTransformType = itk::DisplacementFieldTransform<double, ImageDimension>;
  using FieldType = DisplacementTransformType::DisplacementFieldType;

  auto transformFId = IdentityTransformType::New();

  auto transformMId = IdentityTransformType::New();
  if (transformMId.IsNull())
  {
    std::cerr << "transformMId == nullptr" << std::endl;
    return EXIT_FAILURE;
  }
  auto transformMdisplacement = DisplacementTransformType::New();
  auto transformMtranslation = TranslationTransformType::New();
  auto transformMtranslation2 = TranslationTransformType::New();
  auto transformMComp = CompositeTransformType::New();
  auto transformFComp = CompositeTransformType::New();


  constexpr itk::SizeValueType imageSize = 6;

  ImageType::SizeType size;
  size.Fill(imageSize);
  ImageType::IndexType index;
  index.Fill(0);
  ImageType::RegionType region;
  region.SetSize(size);
  region.SetIndex(index);
  ImageType::SpacingType spacing;
  spacing.Fill(1.0);
  ImageType::PointType origin;
  origin.Fill(0);
  ImageType::DirectionType direction;
  direction.SetIdentity();

  /* Create simple test images. */
  auto fixedImage = ImageType::New();
  fixedImage->SetRegions(region);
  fixedImage->SetSpacing(spacing);
  fixedImage->SetOrigin(origin);
  fixedImage->SetDirection(direction);
  fixedImage->Allocate();

  auto movingImage = ImageType::New();
  movingImage->SetRegions(region);
  movingImage->SetSpacing(spacing);
  movingImage->SetOrigin(origin);
  movingImage->SetDirection(direction);
  movingImage->Allocate();

  /* Fill images */
  itk::ImageRegionIterator<ImageType> itFixed(fixedImage, region);
  itFixed.GoToBegin();
  itk::SizeValueType count = 1;
  while (!itFixed.IsAtEnd())
  {
    itFixed.Set(count * count);
    count++;
    ++itFixed;
  }
  itk::ImageRegionIteratorWithIndex<ImageType> itMoving(movingImage, region);
  itMoving.GoToBegin();
  count = 1;
  while (!itMoving.IsAtEnd())
  {
    itMoving.Set(count * count);
    count++;
    ++itMoving;
  }

  VectorType zero;
  float      def_value = -0.5;

  zero.Fill(def_value);
  auto field = FieldType::New();
  field->SetRegions(fixedImage->GetLargestPossibleRegion());
  field->SetSpacing(fixedImage->GetSpacing());
  field->SetOrigin(fixedImage->GetOrigin());
  field->SetDirection(fixedImage->GetDirection());
  field->Allocate();
  field->FillBuffer(zero);

  auto fieldInv = FieldType::New();

  zero.Fill(def_value * (-1.0));
  fieldInv->SetRegions(fixedImage->GetLargestPossibleRegion());
  fieldInv->SetSpacing(fixedImage->GetSpacing());
  fieldInv->SetOrigin(fixedImage->GetOrigin());
  fieldInv->SetDirection(fixedImage->GetDirection());
  fieldInv->Allocate();
  fieldInv->FillBuffer(zero);

  zero.Fill(def_value * (1.0));
  transformMtranslation->Translate(zero);
  zero.Fill(def_value * (1.0));
  transformMtranslation2->Translate(zero);

  transformMdisplacement->SetDisplacementField(field);
  transformMdisplacement->SetInverseDisplacementField(fieldInv);

  transformMComp->AddTransform(transformMtranslation);
  transformFComp->AddTransform(transformFId);

  using MetricType = itk::ANTSNeighborhoodCorrelationImageToImageMetricv4<ImageType, ImageType>;

  using MetricTypePointer = MetricType::Pointer;
  MetricTypePointer metric = MetricType::New();

  itk::Size<ImageDimension> neighborhood_radius;
  neighborhood_radius.Fill(1);

  metric->SetRadius(neighborhood_radius);

  metric->SetFixedImage(fixedImage);
  metric->SetMovingImage(movingImage);

  metric->SetFixedTransform(transformFId);
  metric->SetMovingTransform(transformMdisplacement);

  std::cout << "fixedImage:" << std::endl;
  ANTSNeighborhoodCorrelationImageToImageMetricv4Test_PrintImage(fixedImage);

  std::cout << "movingImage:" << std::endl;
  ANTSNeighborhoodCorrelationImageToImageMetricv4Test_PrintImage(movingImage);

  /* Initialize. */
  try
  {
    std::cout << "Calling Initialize..." << std::endl;
    metric->Initialize();
  }
  catch (const itk::ExceptionObject & exc)
  {
    std::cerr << "Caught unexpected exception during Initialize: " << exc;
    std::cerr << "Test FAILED." << std::endl;
    return EXIT_FAILURE;
  }

  // Evaluate
  MetricType::MeasureType    valueReturn1;
  MetricType::DerivativeType derivativeReturn;
  try
  {
    std::cout << "Calling GetValueAndDerivative..." << std::endl;
    metric->GetValueAndDerivative(valueReturn1, derivativeReturn);
  }
  catch (const itk::ExceptionObject & exc)
  {
    std::cerr << "Caught unexpected exception during GetValueAndDerivative: " << exc;
    std::cerr << "Test FAILED." << std::endl;
    return EXIT_FAILURE;
  }

  /* Re-initialize. */
  try
  {
    std::cout << "Calling Initialize..." << std::endl;
    metric->Initialize();
  }
  catch (const itk::ExceptionObject & exc)
  {
    std::cerr << "Caught unexpected exception during re-initialize: " << exc;
    std::cerr << "Test FAILED." << std::endl;
    return EXIT_FAILURE;
  }

  // Evaluate with GetValue
  MetricType::MeasureType valueReturn2;
  try
  {
    std::cout << "Calling GetValue..." << std::endl;
    valueReturn2 = metric->GetValue();
  }
  catch (const itk::ExceptionObject & exc)
  {
    std::cerr << "Caught unexpected exception during GetValue: " << exc;
    std::cerr << "Test FAILED." << std::endl;
    return EXIT_FAILURE;
  }

  // Test same value returned by different methods
  std::cout << "Check Value return values..." << std::endl;
  if (itk::Math::NotExactlyEquals(valueReturn1, valueReturn2))
  {
    std::cerr << "Results for Value don't match: " << valueReturn1 << ", " << valueReturn2 << std::endl;
  }

  std::cout << "Test passed." << std::endl;
  std::cout << "transformMdisplacement parameters" << std::endl;
  std::cout << transformMdisplacement->GetParameters() << std::endl;
  ANTSNeighborhoodCorrelationImageToImageMetricv4Test_PrintImage(transformMdisplacement->GetDisplacementField());

  std::cout << "derivative of moving transform:" << std::endl;
  std::cout << derivativeReturn << std::endl;
  std::cout << std::endl << "derivative of moving transform as a field:" << std::endl;
  ANTSNeighborhoodCorrelationImageToImageMetricv4Test_PrintDerivativeAsVectorImage(
    fixedImage, derivativeReturn, ImageDimension);

  /* Compare the derivative computed from sparse threader to the dense threader */
  /* Create a sample point set by sampling all points */
  using PointSetType = MetricType::FixedSampledPointSetType;
  using PointType = PointSetType::PointType;

  std::cout << "Creating point set..." << std::endl;

  PointSetType::Pointer pset(PointSetType::New());

  unsigned int                                 ind = 0, ct = 0;
  itk::ImageRegionIteratorWithIndex<ImageType> It(fixedImage, fixedImage->GetLargestPossibleRegion());
  for (It.GoToBegin(); !It.IsAtEnd(); ++It)
  {
    // take every point
    PointType pt;
    fixedImage->TransformIndexToPhysicalPoint(It.GetIndex(), pt);
    pset->SetPoint(ind, pt);
    ind++;
    ct++;
  }
  std::cout << "Setting point set with " << ind << " points of "
            << fixedImage->GetLargestPossibleRegion().GetNumberOfPixels() << " total " << std::endl;
  std::cout << "Testing metric with point set..." << std::endl;

  /* run the metric with the sparse threader */
  MetricTypePointer metricSparse = MetricType::New();
  metricSparse->SetRadius(neighborhood_radius);
  metricSparse->SetFixedImage(fixedImage);
  metricSparse->SetMovingImage(movingImage);
  metricSparse->SetFixedTransform(transformFId);
  metricSparse->SetMovingTransform(transformMdisplacement);
  metricSparse->SetFixedSampledPointSet(pset);
  metricSparse->SetUseSampledPointSet(true);

  try
  {
    metricSparse->Initialize();
  }
  catch (const itk::ExceptionObject & exc)
  {
    std::cerr << "Caught unexpected exception during Initialize() for sparse threader: " << exc;
    std::cerr << "Test FAILED." << std::endl;
    return EXIT_FAILURE;
  }

  MetricType::MeasureType    valueReturnSparse;
  MetricType::DerivativeType derivativeReturnSparse;

  try
  {
    std::cout << "Calling GetValueAndDerivative..." << std::endl;
    metricSparse->GetValueAndDerivative(valueReturnSparse, derivativeReturnSparse);
  }
  catch (const itk::ExceptionObject & exc)
  {
    std::cerr << "Caught unexpected exception during GetValueAndDrivative() for sparse threader: " << exc;
    std::cerr << "Test FAILED." << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Check Value return values between dense and sparse threader..." << std::endl;
  std::cout << "dense: " << valueReturn1 << ", sparse: " << valueReturnSparse << std::endl;
  if (itk::Math::NotExactlyEquals(valueReturn1, valueReturnSparse))
  {
    std::cerr << "Results for Value don't match using dense and sparse threaders: " << valueReturn1 << ", (sparse) "
              << valueReturnSparse << std::endl;
  }
  std::cout << "Test passed." << std::endl;

  std::cout << "Check Derivative return values between dense and sparse threader..." << std::endl;
  std::cout << "derivative of moving transform (sparse threader):" << std::endl;
  std::cout << derivativeReturnSparse << std::endl;
  std::cout << std::endl << "derivative of moving transform as a field  (sparse threader):" << std::endl;
  ANTSNeighborhoodCorrelationImageToImageMetricv4Test_PrintDerivativeAsVectorImage(
    fixedImage, derivativeReturnSparse, ImageDimension);
  double tolerance = 1e-7;
  if (!derivativeReturn.is_equal(derivativeReturnSparse, tolerance))
  {
    std::cerr << "Results for derivative don't match using dense and sparse threaders: "
              << "dense threader: " << std::endl;
    ANTSNeighborhoodCorrelationImageToImageMetricv4Test_PrintDerivativeAsVectorImage(
      fixedImage, derivativeReturn, ImageDimension);
  }

  // Test that non-overlapping images will generate a warning
  // and return max value for metric value.
  DisplacementTransformType::ParametersType parameters(transformMdisplacement->GetNumberOfParameters());
  parameters.Fill(static_cast<DisplacementTransformType::ParametersValueType>(1000.0));
  transformMdisplacement->SetParameters(parameters);
  MetricType::MeasureType expectedMetricMax, valueReturn;
  expectedMetricMax = itk::NumericTraits<MetricType::MeasureType>::max();
  std::cout << "Testing non-overlapping images. Expect a warning:" << std::endl;
  metric->GetValueAndDerivative(valueReturn, derivativeReturn);
  if (metric->GetNumberOfValidPoints() != 0 || itk::Math::NotExactlyEquals(valueReturn, expectedMetricMax))
  {
    std::cerr << "Failed testing for non-overlapping images. " << std::endl
              << "  Number of valid points: " << metric->GetNumberOfValidPoints() << std::endl
              << "  Metric value: " << valueReturn << std::endl
              << "  Expected metric max value: " << expectedMetricMax << std::endl;
  }

  std::cout << "Test PASSED." << std::endl;
  return EXIT_SUCCESS;
}
