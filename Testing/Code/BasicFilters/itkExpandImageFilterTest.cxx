/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkExpandImageFilterTest.cxx
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
#include <iostream>

#include "itkIndex.h"
#include "itkImage.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkExpandImageFilter.h"
#include "itkCastImageFilter.h"
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



int main()
{
  typedef float PixelType;
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

  std::cout << "Run ExpandImageFilter in standalone mode with progress.";
  std::cout << std::endl;
  typedef itk::ExpandImageFilter<ImageType,ImageType> ExpanderType;
  ExpanderType::Pointer expander = ExpanderType::New();

  unsigned int factors[ImageDimension] = {2,3};
  ImageType::PixelType padValue = 4.0;
  expander->SetInput( input );
  expander->SetExpandFactors( factors );
  expander->SetEdgePaddingValue( padValue );


  ShowProgressObject progressWatch(expander);
  itk::SimpleMemberCommand<ShowProgressObject>::Pointer command;
  command = itk::SimpleMemberCommand<ShowProgressObject>::New();
  command->SetCallbackFunction(&progressWatch,
                               &ShowProgressObject::ShowProgress);
  expander->AddObserver(itk::Command::ProgressEvent, command);

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

  typedef itk::CastImageFilter<ImageType,ImageType> CasterType;
  CasterType::Pointer caster = CasterType::New();

  caster->SetInput( expander->GetInput() );


  ExpanderType::Pointer expander2 = ExpanderType::New();

  expander2->SetInput( caster->GetOutput() );
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
