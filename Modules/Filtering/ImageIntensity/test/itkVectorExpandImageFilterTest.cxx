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

#include "itkVectorExpandImageFilter.h"
#include "itkVectorNearestNeighborInterpolateImageFunction.h"
#include "itkVectorCastImageFilter.h"
#include "itkStreamingImageFilter.h"
#include "itkMath.h"

// class to produce a linear image pattern
template <int VDimension>
class ImagePattern
{
public:
  typedef itk::Index<VDimension> IndexType;

  ImagePattern()
    {
    m_Offset = 0.0;
    for( int j = 0; j < VDimension; j++ )
      {
      m_Coeff[j] = 0.0;
      }
    }

  double Evaluate( const IndexType& index )
    {
    double accum = m_Offset;
    for( int j = 0; j < VDimension; j++ )
      {
      accum += m_Coeff[j] * (double) index[j];
      }
    return accum;
    }

  double m_Coeff[VDimension];
  double m_Offset;

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
  pattern.m_Offset = 64;
  for( j = 0; j < ImageDimension; j++ )
    {
    pattern.m_Coeff[j] = 1.0;
    }

  double vectorCoeff[VectorDimension] = { 1.0, 4.0, 6.0 };

  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;
  Iterator inIter( input, region );

  for(; !inIter.IsAtEnd(); ++inIter )
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

  typedef itk::VectorNearestNeighborInterpolateImageFunction<ImageType,double> InterpolatorType;
  InterpolatorType::Pointer interpolator = InterpolatorType::New();

  expander->SetInterpolator( interpolator );
  std::cout << "Interpolator: " << expander->GetInterpolator() << std::endl;

  expander->SetExpandFactors( 5 );

  unsigned int factors[ImageDimension] = {2,3};
  expander->SetExpandFactors( factors );

  typedef ImageType::PixelType PixelType;
  typedef PixelType::ValueType ValueType;
  ValueType padValueArray[VectorDimension] = {2.0, 7.0, 9.0};
  ImageType::PixelType padValue( padValueArray );
//TEST_RMV20100728   expander->SetEdgePaddingValue( padValue );

  ShowProgressObject progressWatch(expander);
  itk::SimpleMemberCommand<ShowProgressObject>::Pointer command;
  command = itk::SimpleMemberCommand<ShowProgressObject>::New();
  command->SetCallbackFunction(&progressWatch,
                               &ShowProgressObject::ShowProgress);
  expander->AddObserver(itk::ProgressEvent(), command);

  expander->Print( std::cout );
  expander->Update();

  ImageType *expanderOutput = expander->GetOutput();

  //=============================================================

  std::cout << "Checking the output against expected." << std::endl;
  Iterator outIter( expanderOutput,
    expanderOutput->GetBufferedRegion() );

  // compute non-padded output region
  ImageType::RegionType validRegion =
    expanderOutput->GetLargestPossibleRegion();
  ImageType::SizeType validSize = validRegion.GetSize();

  validRegion.SetSize( validSize );

  for(; !outIter.IsAtEnd(); ++outIter )
    {
    ImageType::IndexType index = outIter.GetIndex();
    ImageType::PixelType value = outIter.Get();

    if( validRegion.IsInside( index ) )
      {

      ImageType::PointType point;
      ImageType::IndexType inputIndex;
      expanderOutput->TransformIndexToPhysicalPoint( outIter.GetIndex(), point );
      input->TransformPhysicalPointToIndex(point, inputIndex );
      double baseValue = pattern.Evaluate( inputIndex );

      for( k = 0; k < VectorDimension; k++ )
        {
        if( itk::Math::abs( baseValue * vectorCoeff[k] - value[k] ) > 1e-4 )
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
        if( itk::Math::NotExactlyEquals(value[k], padValue[k]) ){break;}
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

  typedef itk::VectorCastImageFilter<ImageType,ImageType> CasterType;
  CasterType::Pointer caster = CasterType::New();

  caster->SetInput( expander->GetInput() );

  ExpanderType::Pointer expander2 = ExpanderType::New();

  expander2->SetInput( caster->GetOutput() );
  expander2->SetExpandFactors( expander->GetExpandFactors() );
//TEST_RMV20100728   expander2->SetEdgePaddingValue( expander->GetEdgePaddingValue() );
  expander2->SetInterpolator( expander->GetModifiableInterpolator() );

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
      if( itk::Math::NotExactlyEquals(outIter.Get()[k], streamIter.Get()[k]) )
        {
        testPassed = false;
        }
      }

    ++outIter;
    ++streamIter;
    }

  if ( !testPassed )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  // Test error handling

  try
    {
    testPassed = false;
    std::cout << "Setting Input to ITK_NULLPTR" << std::endl;
    expander->SetInput( ITK_NULLPTR );
    expander->Update();
    }
  catch( itk::ExceptionObject& err )
    {
    std::cout << err << std::endl;
    expander->ResetPipeline();
    expander->SetInput( input );
    testPassed = true;
    }

  if ( !testPassed )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }


  try
    {
    testPassed = false;
    std::cout << "Setting Interpolator to ITK_NULLPTR" << std::endl;
    expander->SetInterpolator( ITK_NULLPTR );
    expander->Update();
    }
  catch( itk::ExceptionObject& err )
    {
    std::cout << err << std::endl;
    expander->ResetPipeline();
    expander->SetInterpolator( interpolator );
    testPassed = true;
    }

  if ( !testPassed )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;

}
