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

#include "itkCenteredVersorTransformInitializer.h"

#include "itkImageRegionIterator.h"


/**
 *  This program tests the use of the CenteredVersorTransformInitializer class
 *
 *
 */

int itkCenteredVersorTransformInitializerTest(int , char* [] )
{

  bool pass = true;

  const unsigned int Dimension = 3;

  // Fixed Image Type
  typedef itk::Image<unsigned char, Dimension>      FixedImageType;

  // Moving Image Type
  typedef itk::Image<unsigned char, Dimension>       MovingImageType;

  // Size Type
  typedef FixedImageType::SizeType                 SizeType;
  typedef FixedImageType::SpacingType              SpacingType;
  typedef FixedImageType::PointType                PointType;
  typedef FixedImageType::IndexType                IndexType;
  typedef FixedImageType::RegionType               RegionType;


  // Transform Type
  typedef itk::VersorRigid3DTransform< double >     TransformType;

  SizeType size;
  size[0] = 100;
  size[1] = 100;
  size[2] = 150;

  PointType fixedOrigin;
  fixedOrigin[0] = 0.0;
  fixedOrigin[1] = 0.0;
  fixedOrigin[2] = 0.0;

  PointType movingOrigin;
  movingOrigin[0] = 29.0;
  movingOrigin[1] = 17.0;
  movingOrigin[2] = 13.0;

  SpacingType spacing;
  spacing[0] = 1.5;
  spacing[1] = 1.5;
  spacing[2] = 1.0;

  IndexType index;
  index[0] = 0;
  index[1] = 0;
  index[2] = 0;

  RegionType region;
  region.SetSize( size );
  region.SetIndex( index );


  FixedImageType::Pointer     fixedImage    = FixedImageType::New();
  MovingImageType::Pointer    movingImage   = MovingImageType::New();

  fixedImage->SetRegions( region );
  fixedImage->SetSpacing( spacing );
  fixedImage->SetOrigin(  fixedOrigin );
  fixedImage->Allocate();
  fixedImage->FillBuffer( 0 );

  movingImage->SetRegions( region );
  movingImage->SetSpacing( spacing );
  movingImage->SetOrigin(  movingOrigin );
  movingImage->Allocate();
  movingImage->FillBuffer( 0 );

  RegionType internalRegion;
  SizeType  internalSize;
  IndexType internalIndex;

  internalIndex[0] = index[0] + 20;
  internalIndex[1] = index[1] + 30;
  internalIndex[2] = index[2] + 10;

  internalSize[0]  = size[0] - 2 * 20;
  internalSize[1]  = size[1] - 2 * 30;
  internalSize[2]  = size[2] - 2 * 10;


  internalRegion.SetSize(  internalSize  );
  internalRegion.SetIndex( internalIndex );

  typedef itk::ImageRegionIterator< FixedImageType > FixedIterator;
  FixedIterator fi( fixedImage, internalRegion );

  fi.GoToBegin();
  while( !fi.IsAtEnd() )
    {
    fi.Set( 200 );
    ++fi;
    }


  internalIndex[0] = index[0] + 10;
  internalIndex[1] = index[1] + 20;
  internalIndex[2] = index[2] + 30;

  internalSize[0]  = size[0] - 2 * 10;
  internalSize[1]  = size[1] - 2 * 20;
  internalSize[2]  = size[2] - 2 * 30;


  internalRegion.SetSize(  internalSize  );
  internalRegion.SetIndex( internalIndex );


  typedef itk::ImageRegionIterator< MovingImageType > MovingIterator;
  MovingIterator mi( movingImage, internalRegion );

  mi.GoToBegin();
  while( !mi.IsAtEnd() )
    {
    mi.Set( 200 );
    ++mi;
    }

  TransformType::Pointer transform = TransformType::New();
  transform->SetIdentity();


  typedef itk::CenteredVersorTransformInitializer<
                                  FixedImageType,
                                  MovingImageType >
                                            InitializerType;

  InitializerType::Pointer initializer = InitializerType::New();

  initializer->SetFixedImage( fixedImage );
  initializer->SetMovingImage( movingImage );
  initializer->SetTransform( transform );

  initializer->InitializeTransform();

  TransformType::OutputVectorType translation2 = transform->GetTranslation();
  TransformType::OffsetType       offset2      = transform->GetOffset();

  { // Verfications
  TransformType::InputPointType   fixedCenter;
  TransformType::InputPointType   movingCenter;

  for(unsigned int j=0; j < Dimension; j++ )
    {
    fixedCenter[j]  = fixedOrigin[j]  + size[j] * spacing[j] / 2.0;
    movingCenter[j] = movingOrigin[j] + size[j] * spacing[j] / 2.0;
    }

  TransformType::InputVectorType relativeCenter = movingCenter - fixedCenter;


  const double tolerance = 1e-3;

  for(unsigned int k=0; k < Dimension; k++ )
    {
    if( std::fabs( translation2[k] - relativeCenter[k] ) > tolerance )
      {
      std::cerr << "Translation differs from expected value" << std::endl;
      std::cerr << "It should be " << relativeCenter << std::endl;
      std::cerr << "but it is    " << translation2 << std::endl;
      pass = false;
      break;
      }
    if( std::fabs( offset2[k] - relativeCenter[k] ) > tolerance )
      {
      std::cerr << "Offset differs from expected value" << std::endl;
      std::cerr << "It should be " << relativeCenter << std::endl;
      std::cerr << "but it is    " << offset2 << std::endl;
      pass = false;
      break;
      }
    }

  initializer->ComputeRotationOn();
  initializer->InitializeTransform();

  std::cout << "Initialized Transform is" << std::endl;

  transform->Print( std::cout );

  TransformType::InputPointType mappedOrigin = transform->TransformPoint( fixedOrigin );
  TransformType::InputPointType expectedPoint;
  expectedPoint[0] = 29.0;
  expectedPoint[1] = 165.75;
  expectedPoint[2] = 13.25;

  for(unsigned int j=0; j < Dimension; j++ )
    {
    if( std::fabs( expectedPoint[j] - mappedOrigin[j] ) > tolerance )
      {
      std::cerr << "Mapped point differs from expected point" << std::endl;
      std::cerr << "It should be " << expectedPoint << std::endl;
      std::cerr << "but it is    " << mappedOrigin << std::endl;
      pass = false;
      break;
      }
    }
  }

  if( !pass )
    {
    std::cout << "Test FAILED." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test PASSED." << std::endl;
  return EXIT_SUCCESS;


}
