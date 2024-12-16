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

#include <iostream>
#include "itkImage.h"
#include "itkFixedArray.h"
#include "itkImageAlgorithm.h"
#include "itkTransform.h"

template <unsigned int InputDimension>
class TestTransform
{
public:
  using InputPointType = itk::Point<double, InputDimension>;
  using OutputPointType = itk::Point<double, 2>;
  OutputPointType
  TransformPoint(const InputPointType & inputPoint) const
  {
    OutputPointType outputPoint{};
    // if InputPoint Dimension < 2 then embed point in 2D space
    // else project the point to 2D space.
    for (unsigned int d = 0; d < std::min(inputPoint.GetPointDimension(), outputPoint.GetPointDimension()); ++d)
    {
      outputPoint[d] = inputPoint[d];
    }
    return outputPoint;
  }
};


int
itkImageTest(int, char *[])
{
  using Image = itk::Image<float, 2>;
  auto                      image = Image::New();
  const Image::ConstPointer myconstptr = image;
  image->DebugOn();
  const char * const knownStringName = "My First Image For Testing.";
  image->SetObjectName(knownStringName);
  if (std::string(knownStringName) != image->GetObjectName())
  {
    std::cerr << "ERROR:  Object name not set and recovered correctly.\n"
              << std::string(knownStringName) << " != " << image->GetObjectName() << std::endl;
    return EXIT_FAILURE;
  }
  image->GetSource();
  image->DisconnectPipeline();


  Image::DirectionType direction;
  direction[0][0] = .5;
  direction[0][1] = .7;
  direction[1][0] = .7;
  direction[1][1] = .5;
  image->SetDirection(direction);

  double dspacing[Image::ImageDimension] = { 2.0, 2.0 };
  double dorigin[Image::ImageDimension] = { 2.0, 2.0 };
  image->SetSpacing(dspacing);
  image->SetOrigin(dorigin);

  float fspacing[Image::ImageDimension] = { 3.0, 3.0 };
  float forigin[Image::ImageDimension] = { 3.0, 3.0 };
  image->SetSpacing(fspacing);
  image->SetOrigin(forigin);

  // test inverse direction
  std::cout << "Test inverse direction." << std::endl;
  Image::DirectionType product = direction * image->GetInverseDirection();
  constexpr double     eps = 1e-06;
  if (itk::Math::abs(product[0][0] - 1.0) > eps || itk::Math::abs(product[1][1] - 1.0) > eps ||
      itk::Math::abs(product[0][1]) > eps || itk::Math::abs(product[1][0]) > eps)
  {
    std::cerr << "Inverse direction test failed: "
              << "direction * inverse: " << product << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Test transform to/from physical vector." << std::endl;
  using GradientType = itk::FixedArray<float, 2>;
  GradientType truthGradient;
  truthGradient[0] = 1.0;
  truthGradient[1] = 1.0;
  GradientType outputGradient;
  image->TransformLocalVectorToPhysicalVector(truthGradient, outputGradient);
  GradientType testGradient;
  image->TransformPhysicalVectorToLocalVector(outputGradient, testGradient);
  if (itk::Math::abs(truthGradient[0] - testGradient[0]) > eps ||
      itk::Math::abs(truthGradient[1] - testGradient[1]) > eps)
  {
    std::cerr << "Transform to/from PhysicalVector test failed: "
              << "truthGradient: " << truthGradient << std::endl
              << "testGradient:  " << testGradient << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Test GetSmallestRegionContainingRegion." << std::endl;
  // reset image spacing default values
  auto spacing = itk::MakeFilled<Image::SpacingType>(1.0);
  image->SetSpacing(spacing);
  auto origin = itk::MakeFilled<Image::PointType>(1.2);
  image->SetOrigin(origin);
  direction.SetIdentity();
  image->SetDirection(direction);

  constexpr Image::IndexType index{};
  constexpr auto             size = Image::SizeType::Filled(4);
  const Image::RegionType    region{ index, size };
  image->SetRegions(region);

  auto                       imageRef = Image::New();
  constexpr auto             spacingRef = itk::MakeFilled<Image::SpacingType>(2);
  constexpr Image::PointType originRef{};
  Image::DirectionType       directionRef;
  directionRef.SetIdentity();
  imageRef->SetSpacing(spacingRef);
  imageRef->SetOrigin(originRef);
  imageRef->SetDirection(directionRef);
  constexpr Image::IndexType indexRef{};
  constexpr auto             sizeRef = itk::MakeFilled<Image::SizeType>(5);
  const Image::RegionType    regionRef{ indexRef, sizeRef };
  imageRef->SetRegions(regionRef);

  using TransformType = itk::Transform<double, Image::ImageDimension, Image::ImageDimension>;

  const Image::RegionType    boxRegion = itk::ImageAlgorithm::EnlargeRegionOverBox(image->GetLargestPossibleRegion(),
                                                                                image.GetPointer(),
                                                                                imageRef.GetPointer(),
                                                                                static_cast<TransformType *>(nullptr));
  constexpr Image::IndexType correctIndex{};
  constexpr auto             correctSize = Image::SizeType::Filled(3);
  if (!(boxRegion.GetIndex() == correctIndex) || !(boxRegion.GetSize() == correctSize))
  {
    std::cerr << "EnlargeRegionOverBox test failed: "
              << "boxRegion: " << boxRegion << std::endl;
    return EXIT_FAILURE;
  }

  using Image3D = itk::Image<float, 3>;
  auto volume = Image3D::New();

  Image3D::DirectionType directionVol;
  directionVol.SetIdentity();
  volume->SetDirection(directionVol);


  constexpr Image3D::IndexType indexCuboid{};
  Image3D::SizeType            sizeCuboid;
  sizeCuboid[0] = 1;
  sizeCuboid[1] = 2;
  sizeCuboid[2] = 3;
  const Image3D::RegionType cuboid{ indexCuboid, sizeCuboid };
  volume->SetRegions(cuboid);

  using ProjectionTransformType = TestTransform<Image3D::ImageDimension>;
  auto * projectionTransform = new ProjectionTransformType;

  const Image::RegionType rectangleRegion = itk::ImageAlgorithm::EnlargeRegionOverBox(
    volume->GetLargestPossibleRegion(), volume.GetPointer(), imageRef.GetPointer(), projectionTransform);

  delete projectionTransform;
  constexpr Image::IndexType correctRectangleIndex{};
  Image::SizeType            correctRectangleSize;
  correctRectangleSize[0] = 1;
  correctRectangleSize[1] = 2;
  if (!(rectangleRegion.GetIndex() == correctRectangleIndex) || !(rectangleRegion.GetSize() == correctRectangleSize))
  {
    std::cerr << "EnlargeRegionOverBox test for projecting transform failed: "
              << "rectangle Region: " << rectangleRegion << std::endl;
    return EXIT_FAILURE;
  }

  using TestIdentityTransformType = TestTransform<Image::ImageDimension>;
  auto * testIdentityTransform = new TestIdentityTransformType;

  const Image::RegionType tesBoxRegion = itk::ImageAlgorithm::EnlargeRegionOverBox(
    image->GetLargestPossibleRegion(), image.GetPointer(), imageRef.GetPointer(), testIdentityTransform);

  delete testIdentityTransform;

  if (!(tesBoxRegion.GetIndex() == correctIndex) || !(tesBoxRegion.GetSize() == correctSize))
  {
    std::cerr << "EnlargeRegionOverBox test for test Identity failed: "
              << "rectangle Region: " << tesBoxRegion << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Print: " << std::endl;
  image->Print(std::cout);

  std::cout << "Test Graft." << std::endl;
  image->Graft(imageRef);
  if (image->GetPixelContainer() != imageRef->GetPixelContainer())
  {
    std::cerr << "Graft test failed." << std::endl;
    return EXIT_FAILURE;
  }

  return (EXIT_SUCCESS);
}
