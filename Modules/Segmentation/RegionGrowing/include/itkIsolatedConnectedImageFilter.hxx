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
#ifndef itkIsolatedConnectedImageFilter_hxx
#define itkIsolatedConnectedImageFilter_hxx

#include "itkIsolatedConnectedImageFilter.h"
#include "itkBinaryThresholdImageFunction.h"
#include "itkFloodFilledImageFunctionConditionalIterator.h"
#include "itkProgressReporter.h"
#include "itkIterationReporter.h"
#include "itkMath.h"
#include "itkNumericTraits.h"
#include "itkMath.h"

namespace itk
{
/**
 * Constructor
 */
template< typename TInputImage, typename TOutputImage >
IsolatedConnectedImageFilter< TInputImage, TOutputImage >
::IsolatedConnectedImageFilter()
{
  m_Lower = NumericTraits< InputImagePixelType >::NonpositiveMin();
  m_Upper = NumericTraits< InputImagePixelType >::max();
  m_Seeds1.clear();
  m_Seeds2.clear();
  m_ReplaceValue = NumericTraits< OutputImagePixelType >::OneValue();
  m_IsolatedValue = NumericTraits< InputImagePixelType >::ZeroValue();
  m_IsolatedValueTolerance = NumericTraits< InputImagePixelType >::OneValue();
  m_FindUpperThreshold = true;
  m_ThresholdingFailed = false;
}

/**
 * Standard PrintSelf method.
 */
template< typename TInputImage, typename TOutputImage >
void
IsolatedConnectedImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);
  os << indent << "Lower: "
     << static_cast< typename NumericTraits< InputImagePixelType >::PrintType >( m_Lower )
     << std::endl;
  os << indent << "Upper: "
     << static_cast< typename NumericTraits< InputImagePixelType >::PrintType >( m_Upper )
     << std::endl;
  os << indent << "ReplaceValue: "
     << static_cast< typename NumericTraits< OutputImagePixelType >::PrintType >( m_ReplaceValue )
     << std::endl;
  os << indent << "IsolatedValue: "
     << static_cast< typename NumericTraits< InputImagePixelType >::PrintType >( m_IsolatedValue )
     << std::endl;
  os << indent << "IsolatedValueTolerance: "
     << static_cast< typename NumericTraits< InputImagePixelType >::PrintType >( m_IsolatedValueTolerance )
     << std::endl;
  os << indent << "FindUpperThreshold: "
     << static_cast< typename NumericTraits< bool >::PrintType >( m_FindUpperThreshold )
     << std::endl;
  os << indent << "Thresholding Failed: "
     << static_cast< typename NumericTraits< bool >::PrintType >( m_ThresholdingFailed )
     << std::endl;
}

template< typename TInputImage, typename TOutputImage >
void
IsolatedConnectedImageFilter< TInputImage, TOutputImage >
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
IsolatedConnectedImageFilter< TInputImage, TOutputImage >
::EnlargeOutputRequestedRegion(DataObject *output)
{
  Superclass::EnlargeOutputRequestedRegion(output);
  output->SetRequestedRegionToLargestPossibleRegion();
}

/** Add seed point 1. This seed will be isolated from Seed2 (if possible). */
template< typename TInputImage, typename TOutputImage >
void
IsolatedConnectedImageFilter< TInputImage, TOutputImage >
::AddSeed1(const IndexType & seed)
{
  this->m_Seeds1.push_back(seed);
  this->Modified();
}

#if ! defined ( ITK_FUTURE_LEGACY_REMOVE )
/** \deprecated
 * Set seed point 1. This seed will be isolated from Seed2 (if possible).
 *  This method is deprecated, please use AddSeed1() */
template< typename TInputImage, typename TOutputImage >
void
IsolatedConnectedImageFilter< TInputImage, TOutputImage >
::SetSeed1(const IndexType & seed)
{
  this->ClearSeeds1();
  this->AddSeed1(seed);
}
#endif

/** Clear all the seeds1. */
template< typename TInputImage, typename TOutputImage >
void
IsolatedConnectedImageFilter< TInputImage, TOutputImage >
::ClearSeeds1()
{
  if ( this->m_Seeds1.size() > 0 )
    {
    this->m_Seeds1.clear();
    this->Modified();
    }
}

/** Add seed point 2. This seed will be isolated from Seed1 (if possible). */
template< typename TInputImage, typename TOutputImage >
void
IsolatedConnectedImageFilter< TInputImage, TOutputImage >
::AddSeed2(const IndexType & seed)
{
  this->m_Seeds2.push_back(seed);
  this->Modified();
}

#if ! defined ( ITK_FUTURE_LEGACY_REMOVE )
/** \deprecated
 * Set seed point 2. This seed will be isolated from Seed1 (if possible).
 *  This method is deprecated, please use AddSeed2() */
template< typename TInputImage, typename TOutputImage >
void
IsolatedConnectedImageFilter< TInputImage, TOutputImage >
::SetSeed2(const IndexType & seed)
{
  this->ClearSeeds2();
  this->AddSeed2(seed);
}
#endif

/** Clear all the seeds2. */
template< typename TInputImage, typename TOutputImage >
void
IsolatedConnectedImageFilter< TInputImage, TOutputImage >
::ClearSeeds2()
{
  if ( this->m_Seeds2.size() > 0 )
    {
    this->m_Seeds2.clear();
    this->Modified();
    }
}

template< typename TInputImage, typename TOutputImage >
const typename IsolatedConnectedImageFilter< TInputImage, TOutputImage >::SeedsContainerType &
IsolatedConnectedImageFilter< TInputImage, TOutputImage >
::GetSeeds1() const
{
  itkDebugMacro("returning Seeds1");
  return this->m_Seeds1;
}

template< typename TInputImage, typename TOutputImage >
const typename IsolatedConnectedImageFilter< TInputImage, TOutputImage >::SeedsContainerType &
IsolatedConnectedImageFilter< TInputImage, TOutputImage >
::GetSeeds2() const
{
  itkDebugMacro("returning Seeds2");
  return this->m_Seeds2;
}

template< typename TInputImage, typename TOutputImage >
void
IsolatedConnectedImageFilter< TInputImage, TOutputImage >
::GenerateData()
{
  InputImageConstPointer inputImage = this->GetInput();
  OutputImagePointer     outputImage = this->GetOutput();

  typedef typename NumericTraits< InputImagePixelType >::AccumulateType AccumulateType;

  if( m_Seeds1.empty() )
    {
    itkExceptionMacro("Seeds1 container is empty");
    }

  if( m_Seeds2.empty() )
    {
    itkExceptionMacro("Seeds2 container is empty");
    }

  // Zero the output
  OutputImageRegionType region = outputImage->GetRequestedRegion();
  outputImage->SetBufferedRegion(region);
  outputImage->Allocate();
  outputImage->FillBuffer (NumericTraits< OutputImagePixelType >::ZeroValue());

  typedef BinaryThresholdImageFunction< InputImageType >                               FunctionType;
  typedef FloodFilledImageFunctionConditionalIterator< OutputImageType, FunctionType > IteratorType;

  typename FunctionType::Pointer function = FunctionType::New();
  function->SetInputImage (inputImage);

  float             progressWeight = 0.0f;
  float             cumulatedProgress = 0.0f;
  IteratorType      it = IteratorType (outputImage, function, m_Seeds1);
  IterationReporter iterate(this, 0, 1);

  // If the upper threshold has not been set, find it.
  if ( m_FindUpperThreshold )
    {
    AccumulateType lower = static_cast< AccumulateType >( m_Lower );
    AccumulateType upper = static_cast< AccumulateType >( m_Upper );
    AccumulateType guess = upper;

    // do a binary search to find an upper threshold that separates the
    // two sets of seeds.
    const unsigned int maximumIterationsInBinarySearch =
      static_cast< unsigned int >(
        std::log( ( static_cast< float >( upper ) - static_cast< float >( lower ) )
                 / static_cast< float >( m_IsolatedValueTolerance ) )  / std::log(2.0) );

    progressWeight = 1.0f / static_cast< float >( maximumIterationsInBinarySearch + 2 );
    cumulatedProgress = 0.0f;

    while ( lower + m_IsolatedValueTolerance < guess )
      {
      ProgressReporter progress(this, 0, region.GetNumberOfPixels(), 100, cumulatedProgress, progressWeight);
      cumulatedProgress += progressWeight;
      outputImage->FillBuffer (NumericTraits< OutputImagePixelType >::ZeroValue());
      function->ThresholdBetween ( m_Lower, static_cast< InputImagePixelType >( guess ) );
      it.GoToBegin();
      while ( !it.IsAtEnd() )
        {
        it.Set(m_ReplaceValue);
        if ( it.GetIndex() == *m_Seeds2.begin() )
          {
          break;
          }
        ++it;
        progress.CompletedPixel(); // potential exception thrown here
        }
      // If any of second seeds are included, decrease the upper bound.
      // Find the sum of the intensities in m_Seeds2.  If the second
      // seeds are not included, the sum should be zero.  Otherwise,
      // it will be other than zero.
      InputRealType seedIntensitySum = NumericTraits< InputRealType >::ZeroValue();
      typename SeedsContainerType::const_iterator si = m_Seeds2.begin();
      typename SeedsContainerType::const_iterator li = m_Seeds2.end();
      while ( si != li )
        {
        const InputRealType value =
          static_cast< InputRealType >( outputImage->GetPixel(*si) );
        seedIntensitySum += value;
        si++;
        }

      if ( Math::NotExactlyEquals(seedIntensitySum, NumericTraits< InputRealType >::ZeroValue()) )
        {
        upper = guess;
        }
      // Otherwise, increase the lower bound.
      else
        {
        lower = guess;
        }
      guess = ( upper + lower ) / 2;
      iterate.CompletedStep();
      }

    m_IsolatedValue = static_cast< InputImagePixelType >( lower ); //the lower
                                                                   // bound on
                                                                   // the upper
                                                                   // threshold
                                                                   // guess
    }
  else
    { // If the lower threshold has not been set, find it.
    AccumulateType lower = static_cast< AccumulateType >( m_Lower );
    AccumulateType upper = static_cast< AccumulateType >( m_Upper );
    AccumulateType guess = lower;

    // do a binary search to find a lower threshold that separates the
    // two sets of seeds.
    const unsigned int maximumIterationsInBinarySearch =
      static_cast< unsigned int >(
        std::log( ( static_cast< float >( upper ) - static_cast< float >( lower ) )
                 / static_cast< float >( m_IsolatedValueTolerance ) )  / std::log(2.0) );

    progressWeight = 1.0f / static_cast< float >( maximumIterationsInBinarySearch + 2 );
    cumulatedProgress = 0.0f;

    while ( guess < upper - m_IsolatedValueTolerance )
      {
      ProgressReporter progress(this, 0, region.GetNumberOfPixels(), 100, cumulatedProgress, progressWeight);
      cumulatedProgress += progressWeight;
      outputImage->FillBuffer (NumericTraits< OutputImagePixelType >::ZeroValue());
      function->ThresholdBetween (static_cast< InputImagePixelType >( guess ), m_Upper);
      it.GoToBegin();
      while ( !it.IsAtEnd() )
        {
        it.Set(m_ReplaceValue);
        if ( it.GetIndex() == *m_Seeds2.begin() )
          {
          break;
          }
        ++it;
        progress.CompletedPixel(); // potential exception thrown here
        }
      // If any of second seeds are included, increase the lower bound.
      // Find the sum of the intensities in m_Seeds2.  If the second
      // seeds are not included, the sum should be zero.  Otherwise,
      // it will be other than zero.
      InputRealType seedIntensitySum = NumericTraits< InputRealType >::ZeroValue();
      typename SeedsContainerType::const_iterator si = m_Seeds2.begin();
      typename SeedsContainerType::const_iterator li = m_Seeds2.end();
      while ( si != li )
        {
        const InputRealType value =
          static_cast< InputRealType >( outputImage->GetPixel(*si) );
        seedIntensitySum += value;
        si++;
        }

      if ( Math::NotExactlyEquals(seedIntensitySum, NumericTraits< InputRealType >::ZeroValue()) )
        {
        lower = guess;
        }
      // Otherwise, decrease the upper bound.
      else
        {
        upper = guess;
        }
      guess = ( upper + lower ) / 2;
      iterate.CompletedStep();
      }

    m_IsolatedValue = static_cast< InputImagePixelType >( upper ); //the upper
                                                                   // bound on
                                                                   // the lower
                                                                   // threshold
                                                                   // guess
    }

  // now rerun the algorithm with the thresholds that separate the seeds.
  ProgressReporter progress(this, 0, region.GetNumberOfPixels(), 100, cumulatedProgress, progressWeight);

  outputImage->FillBuffer (NumericTraits< OutputImagePixelType >::ZeroValue());
  if ( m_FindUpperThreshold )
    {
    function->ThresholdBetween (m_Lower, m_IsolatedValue);
    }
  else if ( !m_FindUpperThreshold )
    {
    function->ThresholdBetween (m_IsolatedValue, m_Upper);
    }
  it.GoToBegin();
  while ( !it.IsAtEnd() )
    {
    it.Set(m_ReplaceValue);
    ++it;
    progress.CompletedPixel(); // potential exception thrown here
    }

  // If any of the second seeds are included or some of the first
  // seeds are not included, the algorithm could not find any threshold
  // that would separate the two sets of seeds.  Set an error flag in
  // this case.

  // Find the sum of the intensities in m_Seeds2.  If the second
  // seeds are not included, the sum should be zero.  Otherwise,
  // it will be other than zero.
  InputRealType seed1IntensitySum = NumericTraits< InputRealType >::ZeroValue();
  InputRealType seed2IntensitySum = NumericTraits< InputRealType >::ZeroValue();
  typename SeedsContainerType::const_iterator si1 = m_Seeds1.begin();
  typename SeedsContainerType::const_iterator li1 = m_Seeds1.end();
  while ( si1 != li1 )
    {
    const InputRealType value =
      static_cast< InputRealType >( outputImage->GetPixel(*si1) );
    seed1IntensitySum += value;
    si1++;
    }
  typename SeedsContainerType::const_iterator si2 = m_Seeds2.begin();
  typename SeedsContainerType::const_iterator li2 = m_Seeds2.end();
  while ( si2 != li2 )
    {
    const InputRealType value =
      static_cast< InputRealType >( outputImage->GetPixel(*si2) );
    seed2IntensitySum += value;
    si2++;
    }
  if ( Math::NotAlmostEquals( seed1IntensitySum, m_ReplaceValue * m_Seeds1.size() ) ||
       Math::NotExactlyEquals(seed2IntensitySum, NumericTraits< InputRealType >::ZeroValue()) )
    {
    m_ThresholdingFailed = true;
    }
  iterate.CompletedStep();
}
} // end namespace itk

#endif
