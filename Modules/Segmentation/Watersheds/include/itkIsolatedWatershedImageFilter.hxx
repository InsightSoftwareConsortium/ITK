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
#ifndef itkIsolatedWatershedImageFilter_hxx
#define itkIsolatedWatershedImageFilter_hxx

#include "itkIsolatedWatershedImageFilter.h"
#include "itkProgressReporter.h"
#include "itkIterationReporter.h"

namespace itk
{

template< typename TInputImage, typename TOutputImage >
IsolatedWatershedImageFilter< TInputImage, TOutputImage >
::IsolatedWatershedImageFilter()
{
  m_Threshold = NumericTraits< InputImagePixelType >::ZeroValue();
  m_Seed1.Fill(0);
  m_Seed2.Fill(0);
  m_ReplaceValue1 = NumericTraits< OutputImagePixelType >::OneValue();
  m_ReplaceValue2 = NumericTraits< OutputImagePixelType >::ZeroValue();
  m_IsolatedValue = 0.0;
  m_IsolatedValueTolerance = 0.001;
  m_UpperValueLimit = 1.0;
  m_GradientMagnitude = GradientMagnitudeType::New();
  m_Watershed = WatershedType::New();
}

template< typename TInputImage, typename TOutputImage >
void
IsolatedWatershedImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();
  if ( this->GetInput() )
    {
    InputImagePointer image =
      const_cast< TInputImage * >( this->GetInput() );
    image->SetRequestedRegionToLargestPossibleRegion();
    }
}

template< typename TInputImage, typename TOutputImage >
void
IsolatedWatershedImageFilter< TInputImage, TOutputImage >
::EnlargeOutputRequestedRegion(DataObject *output)
{
  Superclass::EnlargeOutputRequestedRegion(output);
  output->SetRequestedRegionToLargestPossibleRegion();
}

template< typename TInputImage, typename TOutputImage >
void
IsolatedWatershedImageFilter< TInputImage, TOutputImage >
::VerifyInputInformation()
{
  Superclass::VerifyInputInformation();

  const InputImageType *inputImage = this->GetInput();

  const InputImageRegionType region = inputImage->GetRequestedRegion();

  // Check that the seeds are valid after the input has had its output
  // information updated
  if ( !region.IsInside(m_Seed1) )
    {
    itkExceptionMacro("Seed1 is not within the input image!");
    }
  if ( !region.IsInside(m_Seed2) )
    {
    itkExceptionMacro("Seed2 is not within the input image!");
    }

}

template< typename TInputImage, typename TOutputImage >
void
IsolatedWatershedImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  const InputImageType *inputImage = this->GetInput();
  OutputImageType      *outputImage = this->GetOutput();
  OutputImageRegionType region = outputImage->GetRequestedRegion();

  // Set up the pipeline
  m_GradientMagnitude->SetInput (inputImage);

  // Set up the Watershed
  m_Watershed->SetInput( m_GradientMagnitude->GetOutput() );
  m_Watershed->SetThreshold( m_Threshold );
  m_Watershed->SetLevel( m_UpperValueLimit );

  // Allocate the output
  this->AllocateOutputs();

  double lower = m_Threshold;
  double upper = m_UpperValueLimit;
  double guess = upper;

  const unsigned int maximumIterationsInBinarySearch =
    static_cast< unsigned int >(
      std::log( ( static_cast< float >( upper ) - static_cast< float >( lower ) )
               / static_cast< float >( m_IsolatedValueTolerance ) )  / std::log(2.0) );

  const float progressWeight = 1.0f / static_cast< float >( maximumIterationsInBinarySearch + 2 );
  float       cumulatedProgress = 0.0f;

  IterationReporter iterate(this, 0, 1);

  // Do a binary search to find an upper waterlevel that separates the
  // two seeds.
  while ( lower + m_IsolatedValueTolerance < guess )
    {
    ProgressReporter progress(this, 0, region.GetNumberOfPixels(), 100, cumulatedProgress, progressWeight);
    cumulatedProgress += progressWeight;
    m_Watershed->SetLevel (guess);
    m_Watershed->Update ();
    if ( m_Watershed->GetOutput()->GetPixel(m_Seed1) == m_Watershed->GetOutput()->GetPixel(m_Seed2) )
      {
      upper = guess;
      }
    else
      {
      lower = guess;
      }
    guess = ( upper + lower ) / 2;
    iterate.CompletedStep();
    }

  // If the watershed basins are not separated or if the upper/lower
  // threshold were not valid, then use lower.
  if (  m_Watershed->GetOutput()->GetBufferedRegion() != region
        || m_Watershed->GetOutput()->GetPixel(m_Seed1) ==
        m_Watershed->GetOutput()->GetPixel(m_Seed2) )
    {
    m_Watershed->SetLevel (lower);
    m_Watershed->Update ();
    }

  // Now produce an output image with the two seeded basins labeled

  ProgressReporter progress(this, 0, region.GetNumberOfPixels(), 100, cumulatedProgress, progressWeight);

  ImageRegionIterator< OutputImageType > ot =
    ImageRegionIterator< OutputImageType >(outputImage, region);
  ImageRegionIterator< typename WatershedType::OutputImageType > it =
    ImageRegionIterator< typename WatershedType::OutputImageType >(m_Watershed->GetOutput(), region);

  IdentifierType seed1Label = m_Watershed->GetOutput()->GetPixel(m_Seed1);
  IdentifierType seed2Label = m_Watershed->GetOutput()->GetPixel(m_Seed2);
  IdentifierType value;

  it.GoToBegin();
  ot.GoToBegin();
  while ( !it.IsAtEnd() )
    {
    value = it.Get();
    if ( value == seed1Label )
      {
      ot.Set(m_ReplaceValue1);
      }
    else if ( value == seed2Label )
      {
      ot.Set(m_ReplaceValue2);
      }
    else
      {
      ot.Set(NumericTraits< OutputImagePixelType >::ZeroValue());
      }
    ++it;
    ++ot;
    progress.CompletedPixel(); // potential exception thrown here
    }
  m_IsolatedValue = lower;
  iterate.CompletedStep();
}

template< typename TInputImage, typename TOutputImage >
void
IsolatedWatershedImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);
  os << indent << "Threshold: "
     << m_Threshold
     << std::endl;
  os << indent << "UpperValueLimit: "
     << m_UpperValueLimit
     << std::endl;
  os << indent << "ReplaceValue1: "
     << static_cast< typename NumericTraits< OutputImagePixelType >::PrintType >( m_ReplaceValue1 )
     << std::endl;
  os << indent << "ReplaceValue2: "
     << static_cast< typename NumericTraits< OutputImagePixelType >::PrintType >( m_ReplaceValue2 )
     << std::endl;
  os << indent << "Seed1: " << m_Seed1 << std::endl;
  os << indent << "Seed2: " << m_Seed2 << std::endl;
  os << indent << "IsolatedValue: "
     << m_IsolatedValue
     << std::endl;
  os << indent << "IsolatedValueTolerance: "
     << m_IsolatedValueTolerance
     << std::endl;
}

} // end namespace itk

#endif
