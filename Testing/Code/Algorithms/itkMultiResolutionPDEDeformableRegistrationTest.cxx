/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMultiResolutionPDEDeformableRegistrationTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkMultiResolutionPDEDeformableRegistration.h"
#include "itkIndex.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkWarpImageFilter.h"
#include "itkNearestNeighborInterpolateImageFunction.h"
#include "itkCommand.h"
#include "vnl/vnl_math.h"

// uncomment to output images
//#include "itkRawImageWriter.h"

#include <iostream>
#include <string>

namespace
{
  
// The following class is used to support callbacks
// on the filter in the pipeline that follows later
class ShowProgressPDEObject
{
public:
  ShowProgressPDEObject(itk::ProcessObject* o)
    {m_Process = o; m_Prefix="";}
  void ShowProgress()
    {
    std::cout <<  m_Prefix ;
    std::cout << "Progress " << m_Process->GetProgress() << std::endl;
    }
  void ShowIteration()
    {
    std::cout << "New Level" << std::endl;
    }
  itk::ProcessObject::Pointer m_Process;
  std::string m_Prefix;
};
}


// Template function to fill in an image with a value
template <class TImage>
void
FillImage(
TImage * image,
typename TImage::PixelType value )
{

 typedef itk::ImageRegionIteratorWithIndex<TImage> Iterator;
 Iterator it( image, image->GetBufferedRegion() );
 it.Begin();
    
 for( ; !it.IsAtEnd(); ++it )
  {
   it.Set( value );
  }

}


// Template function to fill in an image with a circle.
template <class TImage>
void
FillWithCircle(
TImage * image,
double * center,
double radius,
typename TImage::PixelType foregnd,
typename TImage::PixelType backgnd )
{

 typedef itk::ImageRegionIteratorWithIndex<TImage> Iterator;
 Iterator it( image, image->GetBufferedRegion() );
 it.Begin();
    
 typename TImage::IndexType index;
 double r2 = vnl_math_sqr( radius );

 for( ; !it.IsAtEnd(); ++it )
  {
    index = it.GetIndex();
    double distance = 0;
    for( unsigned int j = 0; j < TImage::ImageDimension; j++ )
      {
      distance += vnl_math_sqr((double) index[j] - center[j]);
      }
    if( distance <= r2 ) it.Set( foregnd );
    else it.Set( backgnd ); 
  }

}


// Template function to copy image regions
template <class TImage>
void
CopyImageBuffer(
TImage *input,
TImage *output )
{
  typedef itk::ImageRegionIteratorWithIndex<TImage> Iterator;
  Iterator inIt( input, output->GetBufferedRegion() );
  Iterator outIt( output, output->GetBufferedRegion() );
  for( ; !inIt.IsAtEnd(); ++inIt, ++outIt )
    {
    outIt.Set( inIt.Get() );
    }

}

int itkMultiResolutionPDEDeformableRegistrationTest(int, char* [] )
{

  typedef unsigned char PixelType;
  enum {ImageDimension = 2};
  typedef itk::Image<PixelType,ImageDimension> ImageType;  
  typedef itk::Vector<float,ImageDimension> VectorType;
  typedef itk::Image<VectorType,ImageDimension> FieldType;
  typedef itk::Image<VectorType::ValueType,ImageDimension> FloatImageType;
  typedef ImageType::IndexType  IndexType;
  typedef ImageType::SizeType   SizeType;
  typedef ImageType::RegionType RegionType;

  //--------------------------------------------------------
  std::cout << "Generate input images and initial field";
  std::cout << std::endl;

  unsigned long sizeArray[ImageDimension] = { 256,256 };
  SizeType size;
  size.SetSize( sizeArray );

  IndexType index;
  index.Fill( 0 );
 
  RegionType region;
  region.SetSize( size );
  region.SetIndex( index );
  
  ImageType::Pointer moving = ImageType::New();
  ImageType::Pointer fixed = ImageType::New();
  FieldType::Pointer initField = FieldType::New();

  moving->SetLargestPossibleRegion( region );
  moving->SetBufferedRegion( region );
  moving->Allocate();

  fixed->SetLargestPossibleRegion( region );
  fixed->SetBufferedRegion( region );
  fixed->Allocate();
  
  initField->SetLargestPossibleRegion( region );
  initField->SetBufferedRegion( region );
  initField->Allocate();

  double center[ImageDimension];
  double radius;
  PixelType fgnd = 250;
  PixelType bgnd = 15;

  // fill moving with circle 
  center[0] = 128; center[1] = 128; radius = 60;
  FillWithCircle<ImageType>( moving, center, radius, fgnd, bgnd );

  // fill fixed with circle
  center[0] = 115; center[1] = 120; radius = 65;
  FillWithCircle<ImageType>( fixed, center, radius, fgnd, bgnd );

  // fill initial deformation with zero vectors
  VectorType zeroVec;
  zeroVec.Fill( 0.0 );
  FillImage<FieldType>( initField, zeroVec );

  //----------------------------------------------------------------
  std::cout << "Run registration." << std::endl;

  typedef itk::MultiResolutionPDEDeformableRegistration<ImageType,
    ImageType, FieldType> RegistrationType;

  RegistrationType::Pointer registrator = RegistrationType::New();

  registrator->SetMovingImage( moving );
  registrator->SetFixedImage( fixed );
 
  unsigned int numLevel = 5;
  unsigned int numIterations[10];
  numIterations[0] = 128;

  unsigned int ilevel;
  for( ilevel = 1; ilevel < numLevel; ilevel++ )
    {
    numIterations[ilevel] = numIterations[ilevel-1]/2;
    }
  
  registrator->SetNumberOfLevels( numLevel );
  registrator->SetNumberOfIterations( numIterations );

  registrator->Print(std::cout);

  typedef itk::SimpleMemberCommand<ShowProgressPDEObject> CommandType;

  ShowProgressPDEObject progressWatch(registrator);
  CommandType::Pointer command = CommandType::New();
  command->SetCallbackFunction(&progressWatch,
                               &ShowProgressPDEObject::ShowIteration);
  registrator->AddObserver(itk::IterationEvent(), command);

  ShowProgressPDEObject innerWatch(registrator->GetRegistrationFilter() );
  innerWatch.m_Prefix = "    ";
  CommandType::Pointer innerCommand = CommandType::New();
  innerCommand->SetCallbackFunction(&innerWatch,
                               &ShowProgressPDEObject::ShowProgress);
  registrator->GetRegistrationFilter()->
    AddObserver(itk::ProgressEvent(), innerCommand);

  registrator->Update();

 
  // -------------------------------------------------------
  std::cout << "Warp moving image" << std::endl;

  typedef itk::WarpImageFilter<ImageType,ImageType,FieldType> WarperType;
  WarperType::Pointer warper = WarperType::New();

  typedef WarperType::CoordRepType CoordRepType;
  typedef itk::NearestNeighborInterpolateImageFunction<ImageType,CoordRepType>
    InterpolatorType;
  InterpolatorType::Pointer interpolator = InterpolatorType::New();
  

  warper->SetInput( moving );
  warper->SetDeformationField( registrator->GetOutput() );
  warper->SetInterpolator( interpolator );
  warper->SetOutputSpacing( fixed->GetSpacing() );
  warper->SetOutputOrigin( fixed->GetOrigin() );

  warper->Update();

 //-------------------------------------------------------

// uncomment to write out images
/*
  std::cout << "Write images to file " << std::endl;
  typedef itk::RawImageWriter<ImageType> WriterType;
  WriterType::Pointer writer = WriterType::New();

  writer->SetFileTypeToBinary();
  
  writer->SetInput( moving );
  writer->SetFileName( "moving.raw" );
  writer->Write();

  writer->SetInput( fixed );
  writer->SetFileName( "fixed.raw" );
  writer->Write();

  writer->SetInput( warper->GetOutput() );
  writer->SetFileName( "warp.raw" );
  writer->Write();
*/
 
  // ---------------------------------------------------------
  std::cout << "Compare warped moving and fixed." << std::endl;

  // compare the warp and fixed images
  itk::ImageRegionIterator<ImageType> fixedIter( fixed,
      fixed->GetBufferedRegion() );
  itk::ImageRegionIterator<ImageType> warpedIter( warper->GetOutput(),
      warper->GetOutput()->GetBufferedRegion() );

  unsigned int numPixelsDifferent = 0;
  while( !fixedIter.IsAtEnd() )
    {
    if( vnl_math_abs( fixedIter.Get() - warpedIter.Get() ) > 
        0.1 * vnl_math_abs( fgnd - bgnd ) )
      {
      numPixelsDifferent++;
      }
    ++fixedIter;
    ++warpedIter;
    }

  std::cout << "Number of pixels different: " << numPixelsDifferent; 
  std::cout << std::endl;

  if( numPixelsDifferent > 20 )
    {
    std::cout << "Test failed - too many pixels different." << std::endl;
    return EXIT_FAILURE;
    }

  //-------------------------------------------------------------
  std::cout << "Test when last shrink factors are not ones." << std::endl;

  registrator->SetNumberOfLevels( 1 );
  registrator->GetFixedImagePyramid()->SetStartingShrinkFactors( 2 );

  unsigned int n = 5;
  registrator->SetNumberOfIterations( &n );

  registrator->Update();

  if( registrator->GetOutput()->GetBufferedRegion() != 
   fixed->GetBufferedRegion() )
    {
    std::cout << "Deformation field should be the same size as fixed";
    std::cout << std::endl;
    return EXIT_FAILURE; 
    }

  // Exercise Get Methods
  std::cout << "RegistrationFilter: " << registrator->GetRegistrationFilter() << std::endl;
  std::cout << "FixedImage: " << registrator->GetFixedImage() << std::endl;
  std::cout << "MovingImage: " << registrator->GetMovingImage() << std::endl;
  std::cout << "FixedImagePyramid: " << registrator->GetFixedImagePyramid() << std::endl;
  std::cout << "MovingImagePyramid: " << registrator->GetMovingImagePyramid() << std::endl;
  std::cout << "NumberOfLevels: " << registrator->GetNumberOfLevels() << std::endl;
  std::cout << "CurrentLevel: " << registrator->GetCurrentLevel() << std::endl;
  std::cout << "NumberOfIterations[0]: " << registrator->GetNumberOfIterations()[0] << std::endl;

  // Exercise error handling
  bool passed;

  typedef RegistrationType::RegistrationType InternalRegistrationType;
  InternalRegistrationType::Pointer demons = registrator->GetRegistrationFilter();

  try
    {
    passed = false;
    std::cout << "Set RegistrationFilter to NULL" << std::endl;
    registrator->SetRegistrationFilter( NULL );
    registrator->Update();
    }
  catch( itk::ExceptionObject& err )
    {
    std::cout << err << std::endl;
    passed = true;
    registrator->ResetPipeline();
    registrator->SetRegistrationFilter( demons );
    }
  
  if ( !passed )
    {
    std::cout << "Test failed" << std::endl;
    }

  typedef RegistrationType::FixedImagePyramidType FixedImagePyramidType;
  FixedImagePyramidType::Pointer fixedPyramid = registrator->GetFixedImagePyramid();

  try
    {
    passed = false;
    std::cout << "Set FixedImagePyramid to NULL" << std::endl;
    registrator->SetFixedImagePyramid( NULL );
    registrator->Update();
    }
  catch( itk::ExceptionObject& err )
    {
    std::cout << err << std::endl;
    passed = true;
    registrator->ResetPipeline();
    registrator->SetFixedImagePyramid( fixedPyramid );
    }
  
  if ( !passed )
    {
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
   }

  typedef RegistrationType::MovingImagePyramidType MovingImagePyramidType;
  MovingImagePyramidType::Pointer movingPyramid = registrator->GetMovingImagePyramid();

  try
    {
    passed = false;
    std::cout << "Set MovingImagePyramid to NULL" << std::endl;
    registrator->SetMovingImagePyramid( NULL );
    registrator->Update();
    }
  catch( itk::ExceptionObject& err )
    {
    std::cout << err << std::endl;
    passed = true;
    registrator->ResetPipeline();
    registrator->SetMovingImagePyramid( movingPyramid );
    }
  
  if ( !passed )
    {
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
    }


  try
    {
    passed = false;
    std::cout << "Set FixedImage to NULL" << std::endl;
    registrator->SetFixedImage( NULL );
    registrator->Update();
    }
  catch( itk::ExceptionObject& err )
    {
    std::cout << err << std::endl;
    passed = true;
    registrator->ResetPipeline();
    registrator->SetFixedImage( fixed );
    }
  
  if ( !passed )
    {
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed" << std::endl;
  return EXIT_SUCCESS;

}
