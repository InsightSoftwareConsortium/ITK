/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkIsolatedConnectedImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkIsolatedConnectedImageFilter_txx_
#define __itkIsolatedConnectedImageFilter_txx_

#include "itkIsolatedConnectedImageFilter.h"
#include "itkBinaryThresholdImageFunction.h"
#include "itkFloodFilledImageFunctionConditionalIterator.h"
#include "itkProgressReporter.h"
#include "itkIterationReporter.h"

namespace itk
{

/**
 * Constructor
 */
template <class TInputImage, class TOutputImage>
IsolatedConnectedImageFilter<TInputImage, TOutputImage>
::IsolatedConnectedImageFilter()
{
  m_Lower = NumericTraits<InputImagePixelType>::NonpositiveMin();
  m_Upper = NumericTraits<InputImagePixelType>::max();
  m_Seeds1.clear();
  m_Seeds2.clear();
  m_ReplaceValue = NumericTraits<OutputImagePixelType>::One;
  m_IsolatedValue = NumericTraits<InputImagePixelType >::Zero;
  m_IsolatedValueTolerance = NumericTraits<InputImagePixelType >::One;
  m_FindUpperThreshold = true;
  m_ThresholdingFailed = false;
}

/**
 * Standard PrintSelf method.
 */
template <class TInputImage, class TOutputImage>
void
IsolatedConnectedImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);
  os << indent << "Lower: "
     << static_cast<typename NumericTraits<InputImagePixelType>::PrintType>(m_Lower)
     << std::endl;
  os << indent << "Upper: "
     << static_cast<typename NumericTraits<InputImagePixelType>::PrintType>(m_Upper)
     << std::endl;
  os << indent << "ReplaceValue: "
     << static_cast<typename NumericTraits<OutputImagePixelType>::PrintType>(m_ReplaceValue)
     << std::endl;
  os << indent << "IsolatedValue: "
     << static_cast<typename NumericTraits<InputImagePixelType>::PrintType>(m_IsolatedValue)
     << std::endl;
  os << indent << "IsolatedValueTolerance: "
     << static_cast<typename NumericTraits<InputImagePixelType>::PrintType>(m_IsolatedValueTolerance)
     << std::endl;
  os << indent << "FindUpperThreshold: "
     << static_cast<typename NumericTraits<bool>::PrintType>(m_FindUpperThreshold)
     << std::endl;
  os << indent << "Thresholding Failed: "
     << static_cast<typename NumericTraits<bool>::PrintType>(m_ThresholdingFailed)
     << std::endl;
}

template <class TInputImage, class TOutputImage>
void 
IsolatedConnectedImageFilter<TInputImage,TOutputImage>
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

template <class TInputImage, class TOutputImage>
void 
IsolatedConnectedImageFilter<TInputImage,TOutputImage>
::EnlargeOutputRequestedRegion(DataObject *output)
{
  Superclass::EnlargeOutputRequestedRegion(output);
  output->SetRequestedRegionToLargestPossibleRegion();
}

template <class TInputImage, class TOutputImage>
void 
IsolatedConnectedImageFilter<TInputImage,TOutputImage>
::GenerateData()
{
  InputImageConstPointer inputImage = this->GetInput();
  OutputImagePointer outputImage = this->GetOutput();
  typedef typename NumericTraits<InputImagePixelType>::AccumulateType AccumulateType;

  // Zero the output
  OutputImageRegionType region = outputImage->GetRequestedRegion() ;
  outputImage->SetBufferedRegion( region );
  outputImage->Allocate();
  outputImage->FillBuffer ( NumericTraits<OutputImagePixelType>::Zero );
  
  typedef BinaryThresholdImageFunction<InputImageType> FunctionType;
  typedef FloodFilledImageFunctionConditionalIterator<OutputImageType, FunctionType> IteratorType;

  typename FunctionType::Pointer function = FunctionType::New();
  function->SetInputImage ( inputImage );

  float progressWeight;
  float cumulatedProgress;
  IteratorType it = IteratorType ( outputImage, function, m_Seeds1 );
  IterationReporter iterate( this, 0, 1);

  // If the upper threshold has not been set, find it.
  if (m_FindUpperThreshold)
    {
    AccumulateType lower = static_cast<AccumulateType>(m_Lower);
    AccumulateType upper = static_cast<AccumulateType>(m_Upper);
    AccumulateType guess = upper;

    // do a binary search to find an upper threshold that separates the
    // two sets of seeds.
    const unsigned int maximumIterationsInBinarySearch = 
      static_cast< unsigned int > (
        vcl_log(( static_cast<float>( upper ) - static_cast< float >( lower ) ) /
             static_cast<float>( m_IsolatedValueTolerance ) )  / vcl_log(2.0 ) );

    progressWeight = 1.0f / static_cast<float>( maximumIterationsInBinarySearch + 2 );
    cumulatedProgress = 0.0f;

    while (lower + m_IsolatedValueTolerance < guess)
      {
      ProgressReporter progress( this, 0, region.GetNumberOfPixels(), 100, cumulatedProgress, progressWeight );
      cumulatedProgress += progressWeight;
      outputImage->FillBuffer ( NumericTraits<OutputImagePixelType>::Zero );
      function->ThresholdBetween ( m_Lower, static_cast<InputImagePixelType>(guess));
      it.GoToBegin();
      while( !it.IsAtEnd())
        {
        it.Set(m_ReplaceValue);
        if (it.GetIndex() == *m_Seeds2.begin())
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
      InputRealType seedIntensitySum = 0;
      typename SeedsContainerType::const_iterator si = m_Seeds2.begin();
      typename SeedsContainerType::const_iterator li = m_Seeds2.end();
      while( si != li )
        {
        const InputRealType value = 
          static_cast< InputRealType >( outputImage->GetPixel( *si ) );
        seedIntensitySum += value;
        si++;
        }

      if (seedIntensitySum != 0)
        {
        upper = guess;
        }
      // Otherwise, increase the lower bound.
      else
        {
        lower = guess;
        }
      guess = (upper + lower) /2;
      iterate.CompletedStep();
      }

    m_IsolatedValue = static_cast<InputImagePixelType>(lower); //the lower bound on the upper threshold guess
    }


  // If the lower threshold has not been set, find it.
  else if (!m_FindUpperThreshold)
    {
    AccumulateType lower = static_cast<AccumulateType>(m_Lower);
    AccumulateType upper = static_cast<AccumulateType>(m_Upper);
    AccumulateType guess = lower;

    // do a binary search to find a lower threshold that separates the
    // two sets of seeds.
    const unsigned int maximumIterationsInBinarySearch = 
      static_cast< unsigned int > (
        vcl_log(( static_cast<float>( upper ) - static_cast< float >( lower ) ) /
             static_cast<float>( m_IsolatedValueTolerance ) )  / vcl_log(2.0 ) );

    progressWeight = 1.0f / static_cast<float>( maximumIterationsInBinarySearch + 2 );
    cumulatedProgress = 0.0f;

    while (guess < upper - m_IsolatedValueTolerance)
      {
      ProgressReporter progress( this, 0, region.GetNumberOfPixels(), 100, cumulatedProgress, progressWeight );
      cumulatedProgress += progressWeight;
      outputImage->FillBuffer ( NumericTraits<OutputImagePixelType>::Zero );
      function->ThresholdBetween ( static_cast<InputImagePixelType>(guess), m_Upper );
      it.GoToBegin();
      while( !it.IsAtEnd())
        {
        it.Set(m_ReplaceValue);
        if (it.GetIndex() == *m_Seeds2.begin())
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
      InputRealType seedIntensitySum = 0;
      typename SeedsContainerType::const_iterator si = m_Seeds2.begin();
      typename SeedsContainerType::const_iterator li = m_Seeds2.end();
      while( si != li )
        {
        const InputRealType value = 
          static_cast< InputRealType >( outputImage->GetPixel( *si ) );
        seedIntensitySum += value;
        si++;
        }

      if (seedIntensitySum != 0)
        {
        lower = guess;
        }
      // Otherwise, decrease the upper bound.
      else
        {
        upper = guess;
        }
      guess = (upper + lower) /2;
      iterate.CompletedStep();
      }

    m_IsolatedValue = static_cast<InputImagePixelType>(upper); //the upper bound on the lower threshold guess
    }

  // now rerun the algorithm with the thresholds that separate the seeds.
  ProgressReporter progress( this, 0, region.GetNumberOfPixels(), 100, cumulatedProgress, progressWeight );

  outputImage->FillBuffer ( NumericTraits<OutputImagePixelType>::Zero );
  if (m_FindUpperThreshold)
    {
    function->ThresholdBetween ( m_Lower, m_IsolatedValue);
    }
  else if (!m_FindUpperThreshold)
    {
    function->ThresholdBetween ( m_IsolatedValue, m_Upper);
    }
  it.GoToBegin();
  while( !it.IsAtEnd())
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
  InputRealType seed1IntensitySum = 0;
  InputRealType seed2IntensitySum = 0;
  typename SeedsContainerType::const_iterator si1 = m_Seeds1.begin();
  typename SeedsContainerType::const_iterator li1 = m_Seeds1.end();
  while( si1 != li1 )
    {
    const InputRealType value = 
      static_cast< InputRealType >( outputImage->GetPixel( *si1 ) );
    seed1IntensitySum += value;
    si1++;
    }
  typename SeedsContainerType::const_iterator si2 = m_Seeds2.begin();
  typename SeedsContainerType::const_iterator li2 = m_Seeds2.end();
  while( si2 != li2 )
    {
    const InputRealType value = 
      static_cast< InputRealType >( outputImage->GetPixel( *si2 ) );
    seed2IntensitySum += value;
    si2++;
    }
  if (seed1IntensitySum != m_ReplaceValue*m_Seeds1.size() || seed2IntensitySum != 0) 
    {
    m_ThresholdingFailed = true;
    }
  iterate.CompletedStep();
}

} // end namespace itk

#endif
