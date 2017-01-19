/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
#include "itkImage.h"
#include "itkFixedArray.h"
#include "itkImageAlgorithm.h"

int itkImageTest(int, char* [] )
{

  typedef itk::Image<float,2> Image;
  Image::Pointer image = Image::New();
  image->DebugOn();
  const char * const knownStringName = "My First Image For Testing.";
  image->SetObjectName( knownStringName );
  if( std::string(knownStringName) != image->GetObjectName() )
    {
    std::cerr << "ERROR:  Object name not set and recovered correctly.\n"
      << std::string(knownStringName) << " != " << image->GetObjectName() << std::endl;
    return EXIT_FAILURE;
    }
  image->GetSource();
  image->DisconnectPipeline();

  Image::SpacingType spacing; spacing.Fill(1.0);
  Image::PointType origin; origin.Fill(1.0);
  Image::DirectionType direction;
  direction[0][0] = .5;
  direction[0][1] = .7;
  direction[1][0] = .7;
  direction[1][1] = .5;
  image->SetSpacing (spacing);
  image->SetOrigin (origin);
  image->SetDirection (direction);

  double dspacing[Image::ImageDimension] = {2.0, 2.0};
  double dorigin[Image::ImageDimension] = {2.0, 2.0};
  image->SetSpacing (dspacing);
  image->SetOrigin (dorigin);

  float fspacing[Image::ImageDimension] = {3.0, 3.0};
  float forigin[Image::ImageDimension] = {3.0, 3.0};
  image->SetSpacing (fspacing);
  image->SetOrigin (forigin);

  // test inverse direction
  std::cout << "Test inverse direction." << std::endl;
  Image::DirectionType product;
  product = direction * image->GetInverseDirection();
  double eps = 1e-06;
  if( std::fabs( product[0][0] - 1.0 ) > eps ||
      std::fabs( product[1][1] - 1.0 ) > eps ||
      std::fabs( product[0][1] ) > eps ||
      std::fabs( product[1][0] ) > eps )
    {
    std::cerr << "Inverse direction test failed: "
              << "direction * inverse: " << product << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test transform to/from physical vector." << std::endl;
  typedef itk::FixedArray<float, 2> GradientType;
  GradientType truthGradient, outputGradient, testGradient;
  truthGradient[0] = 1.0;
  truthGradient[1] = 1.0;
  image->TransformLocalVectorToPhysicalVector( truthGradient, outputGradient );
  image->TransformPhysicalVectorToLocalVector( outputGradient, testGradient );
  if( std::fabs( truthGradient[0] - testGradient[0] ) > eps ||
      std::fabs( truthGradient[1] - testGradient[1] ) > eps )
    {
    std::cerr << "Transform to/from PhysicalVector test failed: "
              << "truthGradient: " << truthGradient << std::endl
              << "testGradient:  " << testGradient << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test GetSmallestRegionContainingRegion." << std::endl;
  image->SetSpacing(spacing);
  origin.Fill(1.2);
  image->SetOrigin(origin);
  direction.SetIdentity();
  image->SetDirection(direction);
  Image::RegionType region;
  Image::IndexType index;
  index.Fill(0);
  Image::SizeType size;
  size.Fill(4);
  region.SetIndex(index);
  region.SetSize(size);
  image->SetRegions(region);

  Image::Pointer imageRef = Image::New();
  Image::SpacingType spacingRef; spacingRef.Fill(2);
  Image::PointType originRef; originRef.Fill(0);
  Image::DirectionType directionRef; directionRef.SetIdentity();
  imageRef->SetSpacing(spacingRef);
  imageRef->SetOrigin(originRef);
  imageRef->SetDirection(directionRef);
  Image::RegionType regionRef;
  Image::IndexType indexRef;
  Image::SizeType sizeRef;
  indexRef.Fill(0);
  sizeRef.Fill(5);
  regionRef.SetIndex(indexRef);
  regionRef.SetSize(sizeRef);
  imageRef->SetRegions(regionRef);

  Image::RegionType boxRegion = itk::ImageAlgorithm::EnlargeRegionOverBox(image->GetLargestPossibleRegion(),
                                                                          image.GetPointer(),
                                                                          imageRef.GetPointer());
  Image::IndexType correctIndex; correctIndex.Fill(0);
  Image::SizeType correctSize; correctSize.Fill(3);
  if( !(boxRegion.GetIndex() == correctIndex) ||
      !(boxRegion.GetSize() == correctSize))
    {
    std::cerr << "EnlargeRegionOverBox test failed: "
              << "boxRegion: " << boxRegion << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Print: " << std::endl;
  image->Print(std::cout);

  std::cout << "Test Graft." << std::endl;
  image->Graft(imageRef);
  if(image->GetPixelContainer() != imageRef->GetPixelContainer() )
  {
    std::cerr << "Graft test failed." << std::endl;
    return EXIT_FAILURE;
  }

  return (EXIT_SUCCESS);
}
