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

#include "itkWarpVectorImageFilter.h"
#include "itkVectorCastImageFilter.h"
#include "itkStreamingImageFilter.h"

// class to produce a linear image pattern
template <int VDimension>
class ImagePattern
{
public:
  typedef itk::Index<VDimension> IndexType;
  typedef itk::Size<VDimension>  SizeType;

  ImagePattern()
  {
    m_Offset = 0.0;
    for( int j = 0; j < VDimension; j++ )
      {
      m_Coeff[j] = 0.0;
      }
  }

  double Evaluate( const IndexType& index , const SizeType& size,
                   const SizeType& clampSize, const float& padValue)
  {
    double accum = m_Offset;
    for( int j = 0; j < VDimension; j++ )
      {
         if ( static_cast< unsigned int >( index[j] ) < size[j] )
           {
           if ( static_cast< unsigned int >( index[j] ) >= clampSize[j] )
             {
             //Interpolators behave this way in half-pixel band at image perimeter
             accum += m_Coeff[j] * (double) (clampSize[j]-1);
             }
           else
             {
             accum += m_Coeff[j] * (double) index[j];
             }
           }
         else
           {
           accum = padValue;
           break;
           }
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
  {
    m_Process = o;
  }
  void ShowProgress()
  {
    std::cout << "Progress " << m_Process->GetProgress() << std::endl;
  }
  itk::ProcessObject::Pointer m_Process;
};

int itkWarpVectorImageFilterTest(int, char* [] )
{
  const unsigned int ImageDimension = 2;

  typedef itk::Vector<float,ImageDimension>     VectorType;
  typedef itk::Image<VectorType,ImageDimension> FieldType;

  // In this case, the image to be warped is also a vector field.
  typedef FieldType             ImageType;
  typedef ImageType::PixelType  PixelType;
  typedef ImageType::IndexType  IndexType;

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


  unsigned int j;
  ImagePattern<ImageDimension> pattern;
  pattern.m_Offset = 64;
  for( j = 0; j < ImageDimension; j++ )
    {
    pattern.m_Coeff[j] = 1.0;
    }

  typedef itk::ImageRegionIteratorWithIndex<ImageType> Iterator;

  float padValue = 4.0;

  for( Iterator inIter( input, region ); !inIter.IsAtEnd(); ++inIter )
    {
    inIter.Set( PixelType(pattern.Evaluate( inIter.GetIndex(), size, size, padValue )) );
    }

  //=============================================================

  std::cout << "Create the input displacement field." << std::endl;

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

  for( FieldIterator fieldIter( field, fieldRegion ); !fieldIter.IsAtEnd(); ++fieldIter )
    {
    IndexType index = fieldIter.GetIndex();
    VectorType displacement;
    for( j = 0; j < ImageDimension; j++ )
      {
      displacement[j] = (float) index[j] * ( (1.0 / factors[j]) - 1.0 );
      }
    fieldIter.Set( displacement );
    }

  //=============================================================

  std::cout << "Run WarpVectorImageFilter in standalone mode with progress.";
  std::cout << std::endl;
  typedef itk::WarpVectorImageFilter<ImageType,ImageType,FieldType> WarperType;
  WarperType::Pointer warper = WarperType::New();

  warper->SetInput( input );
  warper->SetDisplacementField( field );
  warper->SetEdgePaddingValue( PixelType(padValue) );

  ShowProgressObject progressWatch(warper);
  itk::SimpleMemberCommand<ShowProgressObject>::Pointer command;
  command = itk::SimpleMemberCommand<ShowProgressObject>::New();
  command->SetCallbackFunction(&progressWatch,
                               &ShowProgressObject::ShowProgress);
  warper->AddObserver(itk::ProgressEvent(), command);

  warper->Print( std::cout );

  // exercise Get methods
  std::cout << "Interpolator: " << warper->GetInterpolator() << std::endl;
  std::cout << "DisplacementField: " << warper->GetDisplacementField() << std::endl;
  std::cout << "EdgePaddingValue: " << warper->GetEdgePaddingValue() << std::endl;

  // exercise Set methods
  itk::FixedArray<double,ImageDimension> array;
  array.Fill( 2.0 );
  warper->SetOutputSpacing( array.GetDataPointer() );
  array.Fill( 1.0 );
  warper->SetOutputSpacing( array.GetDataPointer() );

  array.Fill( -10.0 );
  warper->SetOutputOrigin( array.GetDataPointer() );
  array.Fill( 0.0 );
  warper->SetOutputOrigin( array.GetDataPointer() );

  // Update the filter
  warper->Update();

  //=============================================================

  std::cout << "Checking the output against expected." << std::endl;

  // compute non-padded output region
  ImageType::RegionType validRegion;
  ImageType::SizeType validSize = validRegion.GetSize();
  //Needed to deal with incompatibility of various IsInside()s &
  //nearest-neighbour type interpolation on half-band at perimeter of
  //image. Evaluate() now has logic for this outer half-band.
  ImageType::SizeType decrementForScaling;
  ImageType::SizeType clampSizeDecrement;
  ImageType::SizeType clampSize;
  for( j = 0; j < ImageDimension; j++ )
    {
    validSize[j] = size[j] * factors[j];

    //Consider as inside anything < 1/2 pixel of (size[j]-1)*factors[j]
    //(0-63) map to (0,126), with 127 exactly at 1/2 pixel, therefore
    //edged out; or to (0,190), with 190 just beyond 189 by 1/3 pixel;
    //or to (0,253), with 254 exactly at 1/2 pixel, therefore out
    //also; or (0, 317), with 317 at 2/5 pixel beyond 315. And so on.

    decrementForScaling[j] =   factors[j] / 2;

    validSize[j] -= decrementForScaling[j];

    //This part of logic determines what is inside, but in outer
    //1/2 pixel band, which has to be clamped to that nearest outer
    //pixel scaled by factor: (0,63) maps to (0,190) as inside, but
    //pixel 190 is outside of (0,189), and must be clamped to it.
    //If factor is 2 or less, this decrement has no effect.

    if( factors[j] < 1+decrementForScaling[j])
      {
      clampSizeDecrement[j] = 0;
      }
    else
      {
      clampSizeDecrement[j]  =  (factors[j] - 1 - decrementForScaling[j]);
      }
    clampSize[j]= validSize[j] - clampSizeDecrement[j];
    }
  validRegion.SetSize( validSize );

  // adjust the pattern coefficients to match
  for( j = 0; j < ImageDimension; j++ )
    {
    pattern.m_Coeff[j] /= (double) factors[j];
    }

  Iterator outIter( warper->GetOutput(), warper->GetOutput()->GetBufferedRegion() );
  while(  !outIter.IsAtEnd() )
    {
    IndexType index = outIter.GetIndex();
    PixelType value = outIter.Get();

    if( validRegion.IsInside( index ) )
      {

      PixelType trueValue(pattern.Evaluate( outIter.GetIndex(), validSize, clampSize, padValue));
      for( unsigned int k=0; k<ImageDimension; k++ )
        {
        if( itk::Math::abs( trueValue[k] - value[k] ) > 1e-4 )
          {
          testPassed = false;
          std::cout << "Error at Index: " << index << " ";
          std::cout << "Expected: " << trueValue << " ";
          std::cout << "Actual: " << value << std::endl;
          break;
          }
        }
      }
    else
      {

      if( value != PixelType(padValue) )
        {
        testPassed = false;
        std::cout << "Error at Index: " << index << " ";
        std::cout << "Expected: " << padValue << " ";
        std::cout << "Actual: " << value << std::endl;
        }
      }
    ++outIter;
    }

  //=============================================================

  std::cout << "Run ExpandImageFilter with streamer";
  std::cout << std::endl;

  typedef itk::VectorCastImageFilter<FieldType,FieldType> VectorCasterType;
  VectorCasterType::Pointer vcaster = VectorCasterType::New();

  vcaster->SetInput( warper->GetDisplacementField() );

  WarperType::Pointer warper2 = WarperType::New();

  warper2->SetInput( warper->GetInput() );
  warper2->SetDisplacementField( vcaster->GetOutput() );
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


  if ( !testPassed )
    {
    std::cout << "Test failed." << std::endl;
    return EXIT_FAILURE;
    }

  // Exercise error handling

  typedef WarperType::InterpolatorType InterpolatorType;
  InterpolatorType::Pointer interp = warper->GetModifiableInterpolator();

  try
    {
    std::cout << "Setting interpolator to ITK_NULLPTR" << std::endl;
    testPassed = false;
    warper->SetInterpolator( ITK_NULLPTR );
    warper->Update();
    }
  catch( itk::ExceptionObject& err )
    {
    std::cout << err << std::endl;
    testPassed = true;
    warper->ResetPipeline();
    warper->SetInterpolator( interp );
    }

  if (!testPassed) {
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
  }

 std::cout << "Test passed." << std::endl;
 return EXIT_SUCCESS;

}
