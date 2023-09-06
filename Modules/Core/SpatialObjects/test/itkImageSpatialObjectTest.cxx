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
// Disable warning for long symbol names in this file only

/*
 * This is a test file for the itkImageSpatialObject class.
 * The supported pixel types does not include itkRGBPixel, itkRGBAPixel, etc...
 * So far it only allows to manage images of simple types like unsigned short,
 * unsigned int, or itk::Vector<...>.
 */


#include "itkImageRegionIterator.h"

#include "itkImageSpatialObject.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkTestingMacros.h"


int
itkImageSpatialObjectTest(int, char *[])
{
#define VDimension 3

  using ScalarType = double;
  using Pixel = unsigned short;
  using ImageType = itk::Image<Pixel, VDimension>;
  using ImageSpatialObject = itk::ImageSpatialObject<VDimension, Pixel>;
  using Iterator = itk::ImageRegionIterator<ImageType>;
  using PointType = itk::Point<ScalarType, VDimension>;

  auto                  image = ImageType::New();
  ImageType::SizeType   size = { { 10, 10, 10 } };
  ImageType::IndexType  index = { { 0, 0, 0 } };
  ImageType::RegionType region;
  ImageType::PointType  origin;
  origin.Fill(5);

  region.SetSize(size);
  region.SetIndex(index);
  image->SetOrigin(origin);
  image->SetRegions(region);
  image->Allocate();

  Iterator it(image, region);
  Pixel    p = 0;

  for (; !it.IsAtEnd(); ++it, ++p)
  {
    it.Set(p);
  }
  it.GoToBegin();

  auto imageSO = ImageSpatialObject::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(imageSO, ImageSpatialObject, SpatialObject);


  typename ImageSpatialObject::IndexType sliceNumber;
  sliceNumber.Fill(0);
  imageSO->SetSliceNumber(sliceNumber);
  ITK_TEST_SET_GET_VALUE(sliceNumber, imageSO->GetSliceNumber());

  imageSO->SetImage(image);
  imageSO->Update();

  ImageSpatialObject::TransformType::OffsetType offset;
  offset.Fill(5);

  imageSO->GetModifiableObjectToParentTransform()->SetOffset(offset);
  imageSO->Update();

  PointType q;
  PointType r;
  double    returnedValue;
  double    expectedValue;

  r.Fill(9);
  q.Fill(15);

  std::cout << "Bounding Box = " << imageSO->GetMyBoundingBoxInWorldSpace()->GetBounds() << std::endl;

  ITK_TEST_EXPECT_TRUE(!imageSO->IsInsideInWorldSpace(r));
  ITK_TEST_EXPECT_TRUE(imageSO->IsInsideInWorldSpace(q));

  q.Fill(15.1);
  expectedValue = 555;

  ITK_TRY_EXPECT_NO_EXCEPTION(imageSO->ValueAtInWorldSpace(q, returnedValue));


  std::cout << "ValueAt()...";
  if (itk::Math::NotAlmostEquals(returnedValue, expectedValue))
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in ValueAt at point " << q << std::endl;
    std::cerr << "Expected value " << expectedValue << std::endl;
    std::cerr << " differs from " << returnedValue << std::endl;
    return EXIT_FAILURE;
  }
  else
  {
    std::cout << "[PASSED]" << std::endl;
  }

  ImageSpatialObject::DerivativeVectorType derivative;
  ImageSpatialObject::DerivativeVectorType expectedDerivative;
  Pixel                                    expectedPixel;

  imageSO->DerivativeAtInWorldSpace(q, 1, derivative);
  expectedPixel = 1;
  expectedDerivative[0] = expectedPixel;
  expectedPixel = 10;
  expectedDerivative[1] = expectedPixel;
  expectedPixel = 100;
  expectedDerivative[2] = expectedPixel;

  ITK_TEST_EXPECT_EQUAL(derivative, expectedDerivative);


  // Now testing the ValueAt() with an interpolator
  using InterpolatorType = itk::LinearInterpolateImageFunction<ImageType>;
  auto interpolator = InterpolatorType::New();
  imageSO->SetInterpolator(interpolator);
  ITK_TEST_SET_GET_VALUE(interpolator, imageSO->GetInterpolator());

  expectedValue = 566.1;

  ITK_TRY_EXPECT_NO_EXCEPTION(imageSO->ValueAtInWorldSpace(q, returnedValue));


  std::cout << "ValueAt() with interpolator...";
  double epsilon = 0.001;
  if (itk::Math::abs(returnedValue - expectedValue) > epsilon)
  {
    std::cerr.precision(static_cast<int>(itk::Math::abs(std::log10(epsilon))));
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in ValueAt at point " << q << std::endl;
    std::cerr << "Expected value " << expectedValue << std::endl;
    std::cerr << " differs from " << returnedValue;
    std::cerr << " by more than " << epsilon << std::endl;
    return EXIT_FAILURE;
  }
  else
  {
    std::cout << "[PASSED]" << std::endl;
  }


  imageSO->DerivativeAtInWorldSpace(q, 1, derivative);
  expectedDerivative[0] = 1;
  expectedDerivative[1] = 10;
  expectedDerivative[2] = 100;
  std::cout << "DerivativeAt() with interpolator ...";
  epsilon = 0.00001;
  if (itk::Math::abs(derivative[0] - expectedDerivative[0]) > epsilon ||
      itk::Math::abs(derivative[1] - expectedDerivative[1]) > epsilon ||
      itk::Math::abs(derivative[2] - expectedDerivative[2]) > epsilon)
  {
    std::cerr.precision(static_cast<int>(itk::Math::abs(std::log10(epsilon))));
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in ValueAt at point " << q << std::endl;
    std::cerr << "Expected value " << expectedDerivative << std::endl;
    std::cerr << " differs from " << derivative;
    std::cerr << " by more than " << epsilon << std::endl;
    return EXIT_FAILURE;
  }
  else
  {
    std::cout << "[PASSED]" << std::endl;
  }


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
