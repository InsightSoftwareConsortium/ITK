/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkMultiResolutionPDEDeformableRegistrationTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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


// The following class is used to support callbacks
// on the filter in the pipeline that follows later
class ShowProgressObject
{
public:
  ShowProgressObject(itk::ProcessObject* o)
    {m_Process = o; m_Prefix="";}
  void ShowProgress()
    {
    std::cout <<  m_Prefix ;
    std::cout << "Progress " << m_Process->GetProgress() << std::endl;
    }
  itk::ProcessObject::Pointer m_Process;
  std::string m_Prefix;
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
  center[0] = 128; center[1] = 128; radius = 60;
  FillWithCircle<ImageType>( reference, center, radius, fgnd, bgnd );

  // fill target with circle
  center[0] = 115; center[1] = 120; radius = 65;
  FillWithCircle<ImageType>( target, center, radius, fgnd, bgnd );

  // fill initial deformation with zero vectors
  VectorType zeroVec;
  zeroVec.Fill( 0.0 );
  FillImage<FieldType>( initField, zeroVec );

  //----------------------------------------------------------------
  std::cout << "Run registration." << std::endl;

  typedef itk::MultiResolutionPDEDeformableRegistration<ImageType,
    ImageType, FieldType> RegistrationType;

  RegistrationType::Pointer registrator = RegistrationType::New();

  registrator->SetReference( reference );
  registrator->SetTarget( target );
 
  unsigned int numLevel = 5;
  std::vector<unsigned int> numIterations(numLevel);
  numIterations[0] = 512;

  unsigned int ilevel;
  for( ilevel = 1; ilevel < numLevel; ilevel++ )
    {
    numIterations[ilevel] = numIterations[ilevel-1]/2;
    }
  
  registrator->SetNumberOfLevels( numLevel );
  registrator->SetNumberOfIterations( 
   static_cast<unsigned int*>(numIterations.begin()) );

  registrator->Print(std::cout);

  typedef itk::SimpleMemberCommand<ShowProgressObject> CommandType;

  ShowProgressObject progressWatch(registrator);
  CommandType::Pointer command = CommandType::New();
  command->SetCallbackFunction(&progressWatch,
                               &ShowProgressObject::ShowProgress);
  registrator->AddObserver(itk::Command::ProgressEvent, command);

  ShowProgressObject innerWatch(registrator->GetRegistrationFilter() );
  innerWatch.m_Prefix = "    ";
  CommandType::Pointer innerCommand = CommandType::New();
  innerCommand->SetCallbackFunction(&innerWatch,
                               &ShowProgressObject::ShowProgress);
  registrator->GetRegistrationFilter()->
    AddObserver(itk::Command::ProgressEvent, innerCommand);

  registrator->Update();

 
  // -------------------------------------------------------
  std::cout << "Warp reference image" << std::endl;

  typedef itk::NearestNeighborInterpolateImageFunction<ImageType>
    InterpolatorType;
  InterpolatorType::Pointer interpolator = InterpolatorType::New();
  
  typedef itk::WarpImageFilter<ImageType,ImageType,FieldType> WarperType;
  WarperType::Pointer warper = WarperType::New();

  warper->SetInput( reference );
  warper->SetDeformationField( registrator->GetOutput() );
  warper->SetInterpolator( interpolator );
  warper->SetOutputSpacing( target->GetSpacing() );
  warper->SetOutputOrigin( target->GetOrigin() );

  warper->Update();

 //-------------------------------------------------------

// uncomment to write out images
/*
  std::cout << "Write images to file " << std::endl;
  typedef itk::RawImageWriter<ImageType> WriterType;
  WriterType::Pointer writer = WriterType::New();

  writer->SetFileTypeToBinary();
  
  writer->SetInput( reference );
  writer->SetFileName( "reference.raw" );
  writer->Write();

  writer->SetInput( target );
  writer->SetFileName( "target.raw" );
  writer->Write();

  writer->SetInput( warper->GetOutput() );
  writer->SetFileName( "warp.raw" );
  writer->Write();
*/
 
  // ---------------------------------------------------------
  std::cout << "Compare warped reference and target." << std::endl;

  // compare the warp and target images
  itk::ImageRegionIterator<ImageType> targetIter( target,
      target->GetBufferedRegion() );
  itk::ImageRegionIterator<ImageType> warpedIter( warper->GetOutput(),
      warper->GetOutput()->GetBufferedRegion() );

  unsigned int numPixelsDifferent = 0;
  while( !targetIter.IsAtEnd() )
    {
    if( vnl_math_abs( targetIter.Get() - warpedIter.Get() ) > 
        0.1 * vnl_math_abs( fgnd - bgnd ) )
      {
      numPixelsDifferent++;
      }
    ++targetIter;
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
  registrator->GetTargetPyramid()->SetStartingShrinkFactors( 2 );

  unsigned int n = 5;
  registrator->SetNumberOfIterations( &n );

  registrator->Update();

  if( registrator->GetOutput()->GetBufferedRegion() != 
   target->GetBufferedRegion() )
    {
    std::cout << "Deformation field should be the same size as target";
    std::cout << std::endl;
    return EXIT_FAILURE; 
    }
  
  return EXIT_SUCCESS;

}
