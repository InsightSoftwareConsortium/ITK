/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkIsolatedConnectedImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
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
  m_Seed1.Fill(0);
  m_Seed2.Fill(0);
  m_ReplaceValue = NumericTraits<OutputImagePixelType>::One;
  m_IsolatedValueTolerance = NumericTraits<InputImagePixelType >::One;
  m_UpperValueLimit = NumericTraits<InputImagePixelType>::max();
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
  os << indent << "Lower: " << m_Lower << std::endl;
  os << indent << "UpperValueLimit: "
     << static_cast<typename NumericTraits<InputImagePixelType>::PrintType>(m_UpperValueLimit)
     << std::endl;
  os << indent << "ReplaceValue: "
     << static_cast<typename NumericTraits<OutputImagePixelType>::PrintType>(m_ReplaceValue)
     << std::endl;
  os << indent << "Seed1: " << m_Seed1 << std::endl;
  os << indent << "Seed2: " << m_Seed2 << std::endl;
  os << indent << "IsolatedValue: "
     << static_cast<typename NumericTraits<InputImagePixelType>::PrintType>(m_IsolatedValue)
     << std::endl;
  os << indent << "IsolatedValueTolerance: "
     << static_cast<typename NumericTraits<InputImagePixelType>::PrintType>(m_IsolatedValueTolerance)
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

  // Zero the output
  OutputImageRegionType region = outputImage->GetRequestedRegion() ;
  outputImage->SetBufferedRegion( region );
  outputImage->Allocate();
  outputImage->FillBuffer ( NumericTraits<OutputImagePixelType>::Zero );
  
  typedef BinaryThresholdImageFunction<InputImageType> FunctionType;
  typedef FloodFilledImageFunctionConditionalIterator<OutputImageType, FunctionType> IteratorType;

  typename FunctionType::Pointer function = FunctionType::New();
    function->SetInputImage ( inputImage );

  InputImagePixelType lower = m_Lower;
  InputImagePixelType upper = m_UpperValueLimit;
  InputImagePixelType guess = upper;
  IteratorType it = IteratorType ( outputImage, function, m_Seed1 );

  const unsigned int estimatedNumberOfIterations =
                         log( ( upper - lower ) / m_IsolatedValueTolerance ) / 
                         log ( 2.0 );

  // Worst-case scenario for the estimation of time to be completed.
  ProgressReporter progress( this, 0, region.GetNumberOfPixels() * 
                                      estimatedNumberOfIterations );

  // do a binary search to find an upper threshold that separates the
  // two seeds.
  itkDebugMacro (<< "GetPixel(m_Seed1): " << inputImage->GetPixel(m_Seed1));
  itkDebugMacro (<< "GetPixel(m_Seed2): " << inputImage->GetPixel(m_Seed2));
  try
    {
    while (lower + m_IsolatedValueTolerance < guess)
      {
      itkDebugMacro( << "lower, upper, guess: " << lower << ", " << upper << ", " << guess);
      outputImage->FillBuffer ( NumericTraits<OutputImagePixelType>::Zero );
      function->ThresholdBetween ( m_Lower, guess );
      it.GoToBegin();
      while( !it.IsAtEnd())
        {
        it.Set(m_ReplaceValue);
        ++it;
        progress.CompletedPixel(); // potential exception thrown here
        }
      if (outputImage->GetPixel(m_Seed2) == m_ReplaceValue)
        {
        upper = guess;
        }
      else
        {
        lower = guess;
        }
      guess = (upper + lower) /2;

      }

    // now rerun the algorithm with the threshold that separates the seeds.
    outputImage->FillBuffer ( NumericTraits<OutputImagePixelType>::Zero );
    function->ThresholdBetween ( m_Lower, lower);
    it.GoToBegin();
    while( !it.IsAtEnd())
      {
      it.Set(m_ReplaceValue);
      ++it;
      progress.CompletedPixel(); // potential exception thrown here
      }
    m_IsolatedValue = lower;

    }
  catch( ProcessAborted & excp )
    {
    this->InvokeEvent( AbortEvent() );
    this->ResetPipeline();
    throw ProcessAborted(__FILE__,__LINE__);
    }
}

} // end namespace itk

#endif
