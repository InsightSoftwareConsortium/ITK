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

#include <iostream>

#include "itkPeriodicBoundaryCondition.h"
#include "itkVectorImage.h"

using ImageType = itk::Image<int, 2>;
using RegionType = ImageType::RegionType;
using IndexType = ImageType::IndexType;
using SizeType = ImageType::SizeType;
using IteratorType = itk::ConstNeighborhoodIterator<ImageType>;
using RadiusType = IteratorType::RadiusType;

using VectorImageType = itk::VectorImage<int, 2>;
using VectorIteratorType = itk::ConstNeighborhoodIterator<VectorImageType>;

static bool
TestPrintNeighborhood(IteratorType & p, VectorIteratorType & v)
{
  bool success = true;

  std::cout << "Output from operator()(const OffsetType &, const OffsetType &, const NeighborhoodType *) const"
            << std::endl;
  unsigned x, y, i = 0;
  for (y = 0; y < p.GetSize()[1]; ++y)
  {
    for (x = 0; x < p.GetSize()[0]; ++x, ++i)
    {
      std::cout << p.GetPixel(i) << " ";
    }
    std::cout << std::endl;
  }

  std::cout << "Output from operator()(const OffsetType &, const OffsetType &, const NeighborhoodType *, "
            << "const NeighborhoodAccessorFunctorType &) const" << std::endl;

  i = 0;
  for (y = 0; y < v.GetSize()[1]; ++y)
  {
    for (x = 0; x < v.GetSize()[0]; ++x, ++i)
    {
      std::cout << v.GetPixel(i)[0] << " ";
    }
    std::cout << std::endl;
  }

  std::cout << "Ouptut from GetPixel( const IndexType & index, const TImage * image ) const" << std::endl;

  i = 0;
  for (y = 0; y < p.GetSize()[1]; ++y)
  {
    itk::Index<2> index;
    index[1] = p.GetIndex()[1] - p.GetRadius()[1] + y;
    for (x = 0; x < p.GetSize()[0]; ++x, ++i)
    {
      index[0] = p.GetIndex()[0] - p.GetRadius()[0] + x;

      // Access the pixel value through two different methods in the
      // boundary condition.
      int pixel1 = p.GetBoundaryCondition()->GetPixel(index, p.GetImagePointer());
      int pixel2 = p.GetPixel(i);

      std::cout << pixel1 << " ";

      // Check agreement of output from three three methods of accessing pixel values.
      if (pixel1 != pixel2)
      {
        success = false;
      }
    }
    std::cout << std::endl;
  }

  std::cout << "----" << std::endl;
  if (!success)
  {
    std::cerr << "Unexpected neighborhood value encountered in neighborhoods printed above." << std::endl;
  }

  return success;
}

static bool
CheckInputRequestedRegion(const RegionType & imageRegion,
                          const RegionType & requestedRegion,
                          const RegionType & expectedRegion)
{
  if (requestedRegion != expectedRegion)
  {
    std::cerr << "Unexpected input region for request region: " << std::endl;
    std::cerr << imageRegion << std::endl;
    std::cerr << "Got:" << std::endl;
    std::cerr << requestedRegion << std::endl;
    std::cerr << "Expected: " << std::endl;
    std::cerr << expectedRegion << std::endl;

    return false;
  }

  return true;
}

int
itkPeriodicBoundaryConditionTest(int, char *[])
{
  // Test an image to cover one operator() method.
  ImageType::Pointer image = ImageType::New();
  RegionType         imageRegion;
  SizeType           imageSize = { { 5, 5 } };
  IndexType          imageIndex = { { 0, 0 } };
  imageRegion.SetSize(imageSize);
  imageRegion.SetIndex(imageIndex);
  image->SetRegions(imageRegion);
  image->Allocate();

  // Test a vector image to cover the other operator() method.
  VectorImageType::Pointer vectorImage = VectorImageType::New();
  vectorImage->SetRegions(imageRegion);
  vectorImage->SetNumberOfComponentsPerPixel(1);
  vectorImage->Allocate();

  IndexType pos;
  for (pos[1] = 0; pos[1] < 5; ++pos[1])
  {
    for (pos[0] = 0; pos[0] < 5; ++pos[0])
    {
      image->SetPixel(pos, pos[0] * 10 + pos[1]);
      VectorImageType::PixelType vectorPixel(1);
      vectorPixel[0] = image->GetPixel(pos);
      vectorImage->SetPixel(pos, vectorPixel);
      std::cout << image->GetPixel(pos) << " ";
    }
    std::cout << std::endl;
  }

  RadiusType radius;
  RadiusType radiusTwo;
  radius[0] = radius[1] = 1;
  IteratorType       it(radius, image, image->GetRequestedRegion());
  VectorIteratorType vit(radius, vectorImage, vectorImage->GetRequestedRegion());

  itk::PeriodicBoundaryCondition<ImageType>       bc;
  itk::PeriodicBoundaryCondition<VectorImageType> vbc;

  it.OverrideBoundaryCondition(&bc);
  vit.OverrideBoundaryCondition(&vbc);

  pos[0] = pos[1] = 0;
  it.SetLocation(pos);

  for (it.GoToBegin(); !it.IsAtEnd(); ++it)
  {
    std::cout << "Index: " << it.GetIndex() << std::endl;
    bool success = TestPrintNeighborhood(it, vit);
    if (!success)
    {
      return EXIT_FAILURE;
    }
  }

  radiusTwo[0] = radiusTwo[1] = 2;
  IteratorType       it2(radiusTwo, image, image->GetRequestedRegion());
  VectorIteratorType vit2(radiusTwo, vectorImage, vectorImage->GetRequestedRegion());

  it2.OverrideBoundaryCondition(&bc);
  vit2.OverrideBoundaryCondition(&vbc);

  pos[0] = pos[1] = 0;
  it2.SetLocation(pos);
  vit2.SetLocation(pos);

  for (it2.GoToBegin(); !it2.IsAtEnd(); ++it2)
  {
    std::cout << "Index: " << it2.GetIndex() << std::endl;
    bool success = TestPrintNeighborhood(it2, vit2);
    if (!success)
    {
      return EXIT_FAILURE;
    }
  }

  // Now test the input region calculation
  IndexType  requestIndex;
  SizeType   requestSize;
  RegionType requestRegion;

  IndexType  expectedIndex;
  SizeType   expectedSize;
  RegionType expectedRegion;

  RegionType inputRegion;

  // Test 1
  std::cout << "GetInputRequestedRegion() Test 1" << std::endl;
  requestIndex.Fill(0);
  requestSize.Fill(2);
  requestRegion.SetIndex(requestIndex);
  requestRegion.SetSize(requestSize);

  expectedRegion = requestRegion;

  inputRegion = bc.GetInputRequestedRegion(imageRegion, requestRegion);
  if (!CheckInputRequestedRegion(imageRegion, inputRegion, expectedRegion))
  {
    std::cerr << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED]" << std::endl;

  // Test 2
  std::cout << "GetInputRequestedRegion() Test 2" << std::endl;
  requestIndex[0] = -2;
  requestIndex[1] = 0;
  requestSize[0] = 3;
  requestSize[1] = 2;
  requestRegion.SetIndex(requestIndex);
  requestRegion.SetSize(requestSize);

  expectedIndex[0] = 0;
  expectedIndex[1] = 0;
  expectedSize[0] = 5;
  expectedSize[1] = 2;
  expectedRegion.SetIndex(expectedIndex);
  expectedRegion.SetSize(expectedSize);

  inputRegion = bc.GetInputRequestedRegion(imageRegion, requestRegion);
  if (!CheckInputRequestedRegion(imageRegion, inputRegion, expectedRegion))
  {
    std::cerr << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED]" << std::endl;

  // Test 3
  std::cout << "GetInputRequestedRegion() Test 3" << std::endl;
  requestIndex[0] = -2;
  requestIndex[1] = 8;
  requestSize[0] = 3;
  requestSize[1] = 3;
  requestRegion.SetIndex(requestIndex);
  requestRegion.SetSize(requestSize);

  expectedIndex[0] = 0;
  expectedIndex[1] = 0;
  expectedSize[0] = 5;
  expectedSize[1] = 5;
  expectedRegion.SetIndex(expectedIndex);
  expectedRegion.SetSize(expectedSize);

  inputRegion = bc.GetInputRequestedRegion(imageRegion, requestRegion);
  if (!CheckInputRequestedRegion(imageRegion, inputRegion, expectedRegion))
  {
    std::cerr << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "[PASSED]" << std::endl;

  // Other boundary condition tests
  if (bc.RequiresCompleteNeighborhood() != true)
  {
    std::cerr << "RequiresCompleteNeighborhood() expected to return true, got false instead." << std::endl;
    return EXIT_FAILURE;
  }

  // Print boundary condition
  bc.Print(std::cout);
  vbc.Print(std::cout);

  // Check when ConstNeighborhoodIterator is templated over the
  // PeriodicBoundaryCondition
  auto                  testImage = ImageType::New();
  ImageType::SizeType   testSize = { { 64, 64 } };
  ImageType::RegionType testRegion(testSize);
  testImage->SetRegions(testRegion);
  testImage->Allocate();

  using BoundaryConditionType = itk::PeriodicBoundaryCondition<ImageType, ImageType>;
  using IterType = itk::ConstNeighborhoodIterator<ImageType, BoundaryConditionType>;
  IterType              testIter(radius, testImage, testRegion);
  BoundaryConditionType boundaryCondition;
  testIter.OverrideBoundaryCondition(&boundaryCondition);
  std::vector<IterType::OffsetType> back;
  back.push_back({ { -1, 0 } });
  back.push_back({ { 0, -1 } });
  back.push_back({ { 0, 0 } });
  testIter.SetNeedToUseBoundaryCondition(true);
  testIter.GoToBegin();
  while (!testIter.IsAtEnd())
  {
    float sum = 0;
    for (auto & j : back)
    {
      const float d = testIter.GetPixel(j);
      sum += d * d;
    }
    ++testIter;
  }

  return EXIT_SUCCESS;
}
