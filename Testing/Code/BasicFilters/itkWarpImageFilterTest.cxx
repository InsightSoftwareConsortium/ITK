/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWarpImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <iostream>

#include "itkVector.h"
#include "itkIndex.h"
#include "itkImage.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkWarpImageFilter.h"
#include "itkVectorCastImageFilter.h"
#include "itkStreamingImageFilter.h"
#include "vnl/vnl_math.h"

// class to produce a linear image pattern
template <int VDimension>
class ImagePattern
{
public:
  typedef itk::Index<VDimension> IndexType;

  ImagePattern() 
    {
    offset = 0.0;
    for( int j = 0; j < VDimension; j++ )
      {
      coeff[j] = 0.0;
      }
    }

  double Evaluate( const IndexType& index )
    {
    double accum = offset;
    for( int j = 0; j < VDimension; j++ )
      {
      accum += coeff[j] * (double) index[j];
      }
    return accum;
    }

  double coeff[VDimension];
  double offset;

};

// The following three classes are used to support callbacks
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



int itkWarpImageFilterTest(int, char* [] )
{
  typedef float PixelType;
  enum { ImageDimension = 2 };
  typedef itk::Image<PixelType,ImageDimension> ImageType;

  typedef itk::Vector<float,ImageDimension> VectorType;
  typedef itk::Image<VectorType,ImageDimension> FieldType;

  bool testPassed = true;


  //=============================================================

  std::cout << "Create the input image pattern." << std::endl;
  ImageType::RegionType region;
  ImageType::SizeType size = {{64, 64}};
  region.SetSize( size );
  
  ImageType::Pointer input = ImageType::New();
  input->SetLargestPossibleRegion( region );
  input->SetBufferedRegion( region );
  input->Allocate();


  int j;
  ImagePattern<ImageDimension> pattern;
  pattern.offset = 64;
  for( j = 0; j < ImageDimension; j++ )
    {
    pattern.coeff[j] = 1.0;
    }

  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;
  Iterator inIter( input, region );

  for( ; !inIter.IsAtEnd(); ++inIter )
    {
    inIter.Set( pattern.Evaluate( inIter.GetIndex() ) );
    }

  //=============================================================

  std::cout << "Create the input deformation field." << std::endl;

  unsigned int factors[ImageDimension] = { 2, 3 };

  ImageType::RegionType fieldRegion;
  ImageType::SizeType fieldSize;
  for( j = 0; j < ImageDimension; j++ )
    {
    fieldSize[j] = size[j] * factors[j] + 5;
    }
  fieldRegion.SetSize( fieldSize );

  FieldType::Pointer field = FieldType::New();
  field->SetLargestPossibleRegion( fieldRegion );
  field->SetBufferedRegion( fieldRegion );
  field->Allocate(); 

  typedef itk::ImageRegionIteratorWithIndex<FieldType> FieldIterator;
  FieldIterator fieldIter( field, fieldRegion );

  for( ; !fieldIter.IsAtEnd(); ++fieldIter )
    {
    ImageType::IndexType index = fieldIter.GetIndex();
    VectorType displacement;
    for( j = 0; j < ImageDimension; j++ )
      {
      displacement[j] = (float) index[j] * ( (1.0 / factors[j]) - 1.0 );
      }
    fieldIter.Set( displacement );
    }

  //=============================================================

  std::cout << "Run WarpImageFilter in standalone mode with progress.";
  std::cout << std::endl;
  typedef itk::WarpImageFilter<ImageType,ImageType,FieldType> WarperType;
  WarperType::Pointer warper = WarperType::New();

  ImageType::PixelType padValue = 4.0;

  warper->SetInput( input );
  warper->SetDeformationField( field );
  warper->SetEdgePaddingValue( padValue );

  ShowProgressObject progressWatch(warper);
  itk::SimpleMemberCommand<ShowProgressObject>::Pointer command;
  command = itk::SimpleMemberCommand<ShowProgressObject>::New();
  command->SetCallbackFunction(&progressWatch,
                               &ShowProgressObject::ShowProgress);
  warper->AddObserver(itk::ProgressEvent(), command);

  warper->Print( std::cout );
  warper->Update();

  //=============================================================

  std::cout << "Checking the output against expected." << std::endl;
  Iterator outIter( warper->GetOutput(),
    warper->GetOutput()->GetBufferedRegion() );

  // compute non-padded output region
  ImageType::RegionType validRegion;
  ImageType::SizeType validSize = validRegion.GetSize();
  for( j = 0; j < ImageDimension; j++ )
    {
    validSize[j] = size[j] * factors[j] - (factors[j] - 1);
    }
  validRegion.SetSize( validSize );

  // adjust the pattern coefficients to match
  for( j = 0; j < ImageDimension; j++ )
    {
    pattern.coeff[j] /= (double) factors[j];
    }

  for( ; !outIter.IsAtEnd(); ++outIter )
    {
    ImageType::IndexType index = outIter.GetIndex();
    double value = outIter.Get();

    if( validRegion.IsInside( index ) )
      {

      double trueValue = pattern.Evaluate( outIter.GetIndex() );

      if( vnl_math_abs( trueValue - value ) > 1e-4 )
        {
        testPassed = false;
        std::cout << "Error at Index: " << index << " ";
        std::cout << "Expected: " << trueValue << " ";
        std::cout << "Actual: " << value << std::endl;
        }
      }
    else
      {
      
      if( value != padValue )
        {
        testPassed = false;
        std::cout << "Error at Index: " << index << " ";
        std::cout << "Expected: " << padValue << " ";
        std::cout << "Actual: " << value << std::endl;
        }
      }

    }

  //=============================================================

  std::cout << "Run ExpandImageFilter with streamer";
  std::cout << std::endl;

  typedef itk::VectorCastImageFilter<FieldType,FieldType> VectorCasterType;
  VectorCasterType::Pointer vcaster = VectorCasterType::New();

  vcaster->SetInput( warper->GetDeformationField() );

  WarperType::Pointer warper2 = WarperType::New();

  warper2->SetInput( warper->GetInput() );
  warper2->SetDeformationField( vcaster->GetOutput() );
  warper2->SetEdgePaddingValue( warper->GetEdgePaddingValue() );

  typedef itk::StreamingImageFilter<ImageType,ImageType> StreamerType;
  StreamerType::Pointer streamer = StreamerType::New();
  streamer->SetInput( warper2->GetOutput() );
  streamer->SetNumberOfStreamDivisions( 3 );
  streamer->Update();

  //=============================================================
  std::cout << "Compare standalone and streamed outputs" << std::endl;

  Iterator streamIter( streamer->GetOutput(),
    streamer->GetOutput()->GetBufferedRegion() );

  outIter.GoToBegin();
  streamIter.GoToBegin();

  while( !outIter.IsAtEnd() )
    {
    if( outIter.Get() != streamIter.Get() )
      {
      testPassed = false;
      }
    ++outIter;
    ++streamIter;
    }
  

  if ( testPassed )
    {
    std::cout << "Test passed." << std::endl;
    return EXIT_SUCCESS;
    }
  else 
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

}
