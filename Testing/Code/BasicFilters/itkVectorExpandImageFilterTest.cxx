/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorExpandImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include <iostream>

#include "itkVector.h"
#include "itkIndex.h"
#include "itkImage.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkVectorExpandImageFilter.h"
//#include "itkVectorCastImageFilter.h"
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


int itkVectorExpandImageFilterTest(int, char* [] )
{
  typedef float ValueType;
  enum { VectorDimension = 3 };
  typedef itk::Vector<ValueType,VectorDimension> PixelType;
  enum { ImageDimension = 2 };
  typedef itk::Image<PixelType,ImageDimension> ImageType;

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

  int j, k;
  ImagePattern<ImageDimension> pattern;
  pattern.offset = 64;
  for( j = 0; j < ImageDimension; j++ )
    {
    pattern.coeff[j] = 1.0;
    }

  double vectorCoeff[VectorDimension] = { 1.0, 4.0, 6.0 };

  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;
  Iterator inIter( input, region );

  for( ; !inIter.IsAtEnd(); ++inIter )
    {

    double value = pattern.Evaluate( inIter.GetIndex() );
    PixelType pixel;
    for( k = 0; k < VectorDimension; k++ )
      {
      pixel[k] = vectorCoeff[k] * value;
      }

    inIter.Set( pixel );
    }

  //=============================================================

  std::cout << "Run ExpandImageFilter in standalone mode with progress.";
  std::cout << std::endl;
  typedef itk::VectorExpandImageFilter<ImageType,ImageType> ExpanderType;
  ExpanderType::Pointer expander = ExpanderType::New();

  expander->SetInput( input );

  unsigned int factors[ImageDimension] = {2,3};
  expander->SetExpandFactors( factors );

  typedef ImageType::PixelType PixelType;
  typedef PixelType::ValueType ValueType;
  ValueType padValueArray[VectorDimension] = {2.0, 7.0, 9.0};
  ImageType::PixelType padValue( padValueArray );
  expander->SetEdgePaddingValue( padValue );

  ShowProgressObject progressWatch(expander);
  itk::SimpleMemberCommand<ShowProgressObject>::Pointer command;
  command = itk::SimpleMemberCommand<ShowProgressObject>::New();
  command->SetCallbackFunction(&progressWatch,
                               &ShowProgressObject::ShowProgress);
  expander->AddObserver(itk::ProgressEvent(), command);

  expander->Print( std::cout );
  expander->Update();

  //=============================================================

  std::cout << "Checking the output against expected." << std::endl;
  Iterator outIter( expander->GetOutput(),
    expander->GetOutput()->GetBufferedRegion() );

  // compute non-padded output region
  ImageType::RegionType validRegion = 
    expander->GetOutput()->GetLargestPossibleRegion();
  ImageType::SizeType validSize = validRegion.GetSize();
  for( j = 0; j < ImageDimension; j++ )
    {
    validSize[j] -= (factors[j] - 1);
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
    ImageType::PixelType value = outIter.Get();

    if( validRegion.IsInside( index ) )
      {

      double baseValue = pattern.Evaluate( outIter.GetIndex() );

      for( k = 0; k < VectorDimension; k++ )
        {
        if( vnl_math_abs( baseValue * vectorCoeff[k] - value[k] ) > 1e-4 )
          {
          break;
          }
        }
      if( k < VectorDimension )
        {
        testPassed = false;
        std::cout << "Error at Index: " << index << std::endl;
        }
      }
    else
      {
      
      for( k = 0; k < VectorDimension; k++ )
        {
        if( value[k] != padValue[k] ){break;}
        }
      if( k < VectorDimension )
        {
        testPassed = false;
        std::cout << "Error at Index: " << index << std::endl;
        }
      }

    }


  //=============================================================

  std::cout << "Run ExpandImageFilter with streamer";
  std::cout << std::endl;

/*
  typedef itk::VectorCastImageFilter<ImageType,ImageType> CasterType;
  CasterType::Pointer caster = CasterType::New();

  caster->SetInput( expander->GetInput() );
*/

  ExpanderType::Pointer expander2 = ExpanderType::New();

//  expander2->SetInput( caster->GetOutput() );
  expander2->SetInput( expander->GetInput() );
  expander2->SetExpandFactors( expander->GetExpandFactors() );
  expander2->SetEdgePaddingValue( expander->GetEdgePaddingValue() );

  typedef itk::StreamingImageFilter<ImageType,ImageType> StreamerType;
  StreamerType::Pointer streamer = StreamerType::New();
  streamer->SetInput( expander2->GetOutput() );
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

    for( k = 0; k < VectorDimension; k++ )
      {
      if( outIter.Get()[k] != streamIter.Get()[k] )
        {
        testPassed = false;
        }
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
