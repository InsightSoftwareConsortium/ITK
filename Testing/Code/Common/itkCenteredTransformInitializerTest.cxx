/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCenteredTransformInitializerTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkVersorRigid3DTransform.h"
#include "itkCenteredTransformInitializer.h"
#include "itkImage.h"

#include "itkImageRegionIterator.h"


/** 
 *  This program tests the use of the CenteredTransformInitializer class
 * 
 *  
 */ 

int itkCenteredTransformInitializerTest(int , char* [] )
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
  typedef TransformType::ParametersType             ParametersType;

  SizeType size;
  size[0] = 100;
  size[1] = 100;
  size[2] =  60;
  
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
  spacing[2] = 2.5;

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

  const unsigned int border = 20;

  internalIndex[0] = index[0] + border;
  internalIndex[1] = index[1] + border;
  internalIndex[2] = index[2] + border;
  
  internalSize[0]  = size[0] - 2 * border;
  internalSize[1]  = size[1] - 2 * border;
  internalSize[2]  = size[2] - 2 * border;


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
   

  typedef itk::ImageRegionIterator< MovingImageType > MovingIterator;
  MovingIterator mi( movingImage, internalRegion );

  mi.GoToBegin();
  while( !mi.IsAtEnd() )
    {
    mi.Set( 200 );
    ++mi;
    }
   


  
  TransformType::Pointer transform = TransformType::New();



  typedef itk::CenteredTransformInitializer< 
                                  TransformType, 
                                  FixedImageType, 
                                  MovingImageType >
                                            InitializerType;

  InitializerType::Pointer initializer = InitializerType::New();

  initializer->SetFixedImage( fixedImage );
  initializer->SetMovingImage( movingImage );
  initializer->SetTransform( transform );
                                    
  transform->SetIdentity();
  initializer->GeometryOn();
  initializer->InitializeTransform();

  std::cout << std::endl << std::endl;
  std::cout << "Testing Geometric Mode " << std::endl;
  transform->Print( std::cout );

  TransformType::InputPointType   center1      = transform->GetCenter();
  TransformType::OutputVectorType translation1 = transform->GetTranslation();
  TransformType::OffsetType       offset1      = transform->GetOffset();

  const double tolerance = 1e-3;
  
  { // Verfications for the Geometry Mode
  TransformType::InputPointType   fixedCenter;
  TransformType::InputPointType   movingCenter;

  for(unsigned int j=0; j < Dimension; j++ )
    {
    fixedCenter[j]  = fixedOrigin[j]  + size[j] * spacing[j] / 2.0 ;
    movingCenter[j] = movingOrigin[j] + size[j] * spacing[j] / 2.0 ;
    }
  
  TransformType::InputVectorType relativeCenter = movingCenter - fixedCenter;

  for(unsigned int k=0; k < Dimension; k++ )
    {
    if( fabs( translation1[k] - relativeCenter[k] ) > tolerance )
      {
      std::cerr << "Translation differs from expected value" << std::endl;
      std::cerr << "It should be " << relativeCenter << std::endl;
      std::cerr << "but it is    " << translation1 << std::endl;
      pass = false;
      break;
      }
    if( fabs( offset1[k] - relativeCenter[k] ) > tolerance )
      {
      std::cerr << "Offset differs from expected value" << std::endl;
      std::cerr << "It should be " << relativeCenter << std::endl;
      std::cerr << "but it is    " << offset1 << std::endl;
      pass = false;
      break;
      }
    }
  }

  transform->SetIdentity();
  initializer->MomentsOn();
  initializer->InitializeTransform();

  std::cout << std::endl << std::endl;
  std::cout << "Testing Moments Mode " << std::endl;
  transform->Print( std::cout );

  TransformType::InputPointType   center2      = transform->GetCenter();
  TransformType::OutputVectorType translation2 = transform->GetTranslation();
  TransformType::OffsetType       offset2      = transform->GetOffset();

  { // Verfications for the Moments Mode
  TransformType::InputPointType   fixedCenter;
  TransformType::InputPointType   movingCenter;

  for(unsigned int j=0; j < Dimension; j++ )
    {
    fixedCenter[j]  = fixedOrigin[j]  + size[j] * spacing[j] / 2.0 ;
    movingCenter[j] = movingOrigin[j] + size[j] * spacing[j] / 2.0 ;
    }
  
  TransformType::InputVectorType relativeCenter = movingCenter - fixedCenter;



  for(unsigned int k=0; k < Dimension; k++ )
    {
    if( fabs( translation2[k] - relativeCenter[k] ) > tolerance )
      {
      std::cerr << "Translation differs from expected value" << std::endl;
      std::cerr << "It should be " << relativeCenter << std::endl;
      std::cerr << "but it is    " << translation2 << std::endl;
      pass = false;
      break;
      }
    if( fabs( offset2[k] - relativeCenter[k] ) > tolerance )
      {
      std::cerr << "Offset differs from expected value" << std::endl;
      std::cerr << "It should be " << relativeCenter << std::endl;
      std::cerr << "but it is    " << offset2 << std::endl;
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

