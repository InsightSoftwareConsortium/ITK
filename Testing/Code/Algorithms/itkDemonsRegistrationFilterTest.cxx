/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDemonsRegistrationFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkDemonsRegistrationFilter.h"

#include "itkIndex.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkWarpImageFilter.h"
#include "itkNearestNeighborInterpolateImageFunction.h"
#include "itkCommand.h"
#include "vnl/vnl_math.h"


// The following class is used to support callbacks
// on the filter in the pipeline that follows later
class ShowProgressObject
{
public:
  ShowProgressObject(itk::ProcessObject* o)
    {m_Process = o;}
  void ShowProgress()
    {std::cout << "Progress " << m_Process->GetProgress() << std::endl;}
  itk::ProcessObject::Pointer m_Process;
};

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
 double rad2 = vnl_math_sqr( radius );

 for( ; !it.IsAtEnd(); ++it )
  {
    index = it.GetIndex();
    double distance = 0;
    for( int j = 0; j < TImage::ImageDimension; j++ )
      {
      distance += vnl_math_sqr((double) index[j] - center[j]);
      }
    if( distance <= rad2 ) it.Set( foregnd );
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

int main()
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
  std::cout << "Generate input images and initial deformation field";
  std::cout << std::endl;

  unsigned long sizeArray[ImageDimension] = { 128, 128 };
  SizeType size;
  size.SetSize( sizeArray );

  IndexType index;
  index.Fill( 0 );
 
  RegionType region;
  region.SetSize( size );
  region.SetIndex( index );
  
  ImageType::Pointer reference = ImageType::New();
  ImageType::Pointer target = ImageType::New();
  FieldType::Pointer initField = FieldType::New();

  reference->SetLargestPossibleRegion( region );
  reference->SetBufferedRegion( region );
  reference->Allocate();

  target->SetLargestPossibleRegion( region );
  target->SetBufferedRegion( region );
  target->Allocate();
  
  initField->SetLargestPossibleRegion( region );
  initField->SetBufferedRegion( region );
  initField->Allocate();

  double center[ImageDimension];
  double radius;
  PixelType fgnd = 250;
  PixelType bgnd = 15;

  // fill reference with circle 
  center[0] = 64; center[1] = 64; radius = 30;
  FillWithCircle<ImageType>( reference, center, radius, fgnd, bgnd );

  // fill target with circle
  center[0] = 62; center[1] = 64; radius = 32;
  FillWithCircle<ImageType>( target, center, radius, fgnd, bgnd );

  // fill initial deformation with zero vectors
  VectorType zeroVec;
  zeroVec.Fill( 0.0 );
  FillImage<FieldType>( initField, zeroVec );

  //-------------------------------------------------------------
  std::cout << "Run registration and warp reference" << std::endl;

  typedef itk::DemonsRegistrationFilter<ImageType,ImageType,FieldType> 
    RegistrationType;
  RegistrationType::Pointer registrator = RegistrationType::New();

  registrator->SetInitialDeformationField( initField );
  registrator->SetReference( reference );
  registrator->SetTarget( target );
  registrator->SetNumberOfIterations( 150 );
  registrator->SetStandardDeviations( 1.0 );
  registrator->Print( std::cout );

  typedef RegistrationType::DemonsRegistrationFunctionType FunctionType;
  FunctionType * fptr;
  fptr = dynamic_cast<FunctionType *>(
    registrator->GetDifferenceFunction().GetPointer() );
  fptr->Print( std::cout );

  ShowProgressObject progressWatch(registrator);
  itk::SimpleMemberCommand<ShowProgressObject>::Pointer command;
  command = itk::SimpleMemberCommand<ShowProgressObject>::New();
  command->SetCallbackFunction(&progressWatch,
                               &ShowProgressObject::ShowProgress);
  registrator->AddObserver( itk::ProgressEvent(), command);
 
  registrator->Update();

  // warp reference image
  typedef itk::WarpImageFilter<ImageType,ImageType,FieldType> WarperType;
  WarperType::Pointer warper = WarperType::New();

  typedef WarperType::CoordRepType CoordRepType;
  typedef itk::NearestNeighborInterpolateImageFunction<ImageType,CoordRepType>
    InterpolatorType;
  InterpolatorType::Pointer interpolator = InterpolatorType::New();
  

  warper->SetInput( reference );
  warper->SetDeformationField( registrator->GetOutput() );
  warper->SetInterpolator( interpolator );
  warper->SetOutputSpacing( target->GetSpacing() );
  warper->SetOutputOrigin( target->GetOrigin() );

  warper->Print( std::cout );

  warper->Update();
 
  // ---------------------------------------------------------
  std::cout << "Compare warped reference and target." << std::endl;

  // compare the warp and target images
  itk::ImageRegionIterator<ImageType> targetIter( target,
      target->GetBufferedRegion() );
  itk::ImageRegionIterator<ImageType> warpedIter( warper->GetOutput(),
      target->GetBufferedRegion() );

  unsigned int numPixelsDifferent = 0;
  while( !targetIter.IsAtEnd() )
    {
    if( targetIter.Get() != warpedIter.Get() )
      {
      numPixelsDifferent++;
      }
    ++targetIter;
    ++warpedIter;
    }

  std::cout << "Number of pixels different: " << numPixelsDifferent; 
  std::cout << std::endl;

  if( numPixelsDifferent > 10 )
    {
    std::cout << "Test failed - too many pixels different." << std::endl;
    return EXIT_FAILURE;
    }


  // -----------------------------------------------------------
  std::cout << "Test running registrator with initial deformation field.";
  std::cout << std::endl;

  registrator->SetInput( NULL );
  registrator->SetNumberOfIterations( 2 );
  registrator->Update();
  
  std::cout << "Test passed" << std::endl;
  return EXIT_SUCCESS;
  

}

