/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConfidenceConnectedImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkConfidenceConnectedImageFilter_txx_
#define __itkConfidenceConnectedImageFilter_txx_

#include "itkConfidenceConnectedImageFilter.h"
#include "itkExceptionObject.h"
#include "itkImageRegionIterator.h"
#include "itkMeanImageFunction.h"
#include "itkVarianceImageFunction.h"
#include "itkBinaryThresholdImageFunction.h"
#include "itkFloodFilledImageFunctionConditionalIterator.h"
#include "itkFloodFilledImageFunctionConditionalConstIterator.h"
#include "itkNumericTraits.h"

namespace itk
{

/**
 * Constructor
 */
template <class TInputImage, class TOutputImage>
ConfidenceConnectedImageFilter<TInputImage, TOutputImage>
::ConfidenceConnectedImageFilter()
{
  m_Multiplier = 2.5;
  m_NumberOfIterations = 4;
  m_Seed.Fill(0);
  m_InitialNeighborhoodRadius = 1;
  m_ReplaceValue = NumericTraits<OutputImagePixelType>::One;
}

/**
 * Standard PrintSelf method.
 */
template <class TInputImage, class TOutputImage>
void
ConfidenceConnectedImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);
  os << indent << "Number of iterations: " << m_NumberOfIterations
     << std::endl;
  os << indent << "Multiplier for confidence interval: " << m_Multiplier
     << std::endl;
  os << indent << "ReplaceValue: "
     << static_cast<typename NumericTraits<OutputImagePixelType>::PrintType>(m_ReplaceValue)
     << std::endl;
  os << indent << "InitialNeighborhoodRadius: " << m_InitialNeighborhoodRadius 
     << std::endl;
     
}

template <class TInputImage, class TOutputImage>
void 
ConfidenceConnectedImageFilter<TInputImage,TOutputImage>
::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();
  if ( this->GetInput() )
    {
    InputImagePointer input =
        const_cast< TInputImage * >( this->GetInput() );
    input->SetRequestedRegionToLargestPossibleRegion();
    }
}

template <class TInputImage, class TOutputImage>
void 
ConfidenceConnectedImageFilter<TInputImage,TOutputImage>
::EnlargeOutputRequestedRegion(DataObject *output)
{
  Superclass::EnlargeOutputRequestedRegion(output);
  output->SetRequestedRegionToLargestPossibleRegion();
}

template <class TInputImage, class TOutputImage>
void 
ConfidenceConnectedImageFilter<TInputImage,TOutputImage>
::GenerateData()
{
  typedef typename NumericTraits<ITK_TYPENAME InputImageType::PixelType>::RealType InputRealType;
  typedef BinaryThresholdImageFunction<InputImageType> FunctionType;
  typedef BinaryThresholdImageFunction<OutputImageType> SecondFunctionType;
  typedef FloodFilledImageFunctionConditionalIterator<OutputImageType, FunctionType> IteratorType;
  typedef FloodFilledImageFunctionConditionalConstIterator<InputImageType, SecondFunctionType> SecondIteratorType;

  unsigned int loop;
  unsigned long num;
  
  typename Superclass::InputImageConstPointer inputImage  = this->GetInput();
  typename Superclass::OutputImagePointer     outputImage = this->GetOutput();

  // Zero the output
  outputImage->SetBufferedRegion( outputImage->GetRequestedRegion() );
  outputImage->Allocate();
  outputImage->FillBuffer ( NumericTraits<OutputImagePixelType>::Zero );

  // Compute the statistics of the seed point
  typename MeanImageFunction<InputImageType>::Pointer meanFunction
    = MeanImageFunction<InputImageType>::New();
  meanFunction->SetInputImage( inputImage );
  meanFunction->SetNeighborhoodRadius( m_InitialNeighborhoodRadius );
  typename VarianceImageFunction<InputImageType>::Pointer varianceFunction
    = VarianceImageFunction<InputImageType>::New();
  varianceFunction->SetInputImage( inputImage );
  varianceFunction->SetNeighborhoodRadius( m_InitialNeighborhoodRadius );
  
  // Set up the image function used for connectivity
  typename FunctionType::Pointer function = FunctionType::New();
  function->SetInputImage ( inputImage );

  InputRealType lower, upper, seedIntensity;
  InputRealType mean, variance;
  mean = meanFunction->EvaluateAtIndex( m_Seed );
  variance = varianceFunction->EvaluateAtIndex( m_Seed );
  seedIntensity = static_cast<InputRealType>(inputImage->GetPixel(m_Seed));

  lower = mean - m_Multiplier * sqrt(variance);
  upper = mean + m_Multiplier * sqrt(variance);
  
  // Adjust lower and upper to always contain the seed's intensity, otherwise, no pixels will be
  // returned by the iterator and a zero variance will result
  if (lower > seedIntensity)
    {
    lower = seedIntensity;
    }
  if (upper < seedIntensity)
    {
    upper = seedIntensity;
    }

  if (lower < static_cast<InputRealType>(NumericTraits<InputImagePixelType>::NonpositiveMin()))
    {
    lower = static_cast<InputRealType>(NumericTraits<InputImagePixelType>::NonpositiveMin());
    }
  if (upper > static_cast<InputRealType>(NumericTraits<InputImagePixelType>::max()))
    {
    upper = static_cast<InputRealType>(NumericTraits<InputImagePixelType>::max());
    }
  function->ThresholdBetween(static_cast<InputImagePixelType>(lower),
                             static_cast<InputImagePixelType>(upper));

    itkDebugMacro(<< "\nLower intensity = " << lower << ", Upper intensity = " << upper << "\nmean = " << mean << " , sqrt(variance) = " << sqrt(variance));
  
  IteratorType it = IteratorType ( outputImage, function, m_Seed );
  while( !it.IsAtEnd())
    {
    it.Set(m_ReplaceValue);
    ++it;
    }

  for (loop = 0; loop < m_NumberOfIterations; ++loop)
    {
    // Now that we have an initial segmentation, let's recalculate the
    // statistics Since we have already labelled the output, we walk the
    // output for candidate pixels and calculate the statistics from
    // the input image
    typename SecondFunctionType::Pointer secondFunction = SecondFunctionType::New();
    secondFunction->SetInputImage ( outputImage );
    secondFunction->ThresholdBetween( m_ReplaceValue, m_ReplaceValue );

    typename NumericTraits<ITK_TYPENAME InputImageType::PixelType>::RealType sum, sumOfSquares;
    sum = NumericTraits<InputRealType>::Zero;
    sumOfSquares = NumericTraits<InputRealType>::Zero;
    num = 0;
    
    SecondIteratorType sit
      = SecondIteratorType ( inputImage, secondFunction, m_Seed );
    while( !sit.IsAtEnd())
      {
      sum += static_cast<InputRealType>(sit.Get());
      sumOfSquares += (static_cast<InputRealType>(sit.Get())
                       * static_cast<InputRealType>(sit.Get()));
      ++num;
      ++sit;
      }
    mean = sum / double(num);
    variance = (sumOfSquares - (sum*sum / double(num))) / (double(num) - 1.0);
    // if the variance is zero, there is no point in continuing
    if (variance == 0)
      {
      itkDebugMacro(<< "\nLower intensity = " << lower << ", Upper intensity = " << upper << "\nmean = " << mean << ", variance = " << variance << " , sqrt(variance) = " << sqrt(variance));
      itkDebugMacro(<< "\nsum = " << sum << ", sumOfSquares = " << sumOfSquares << "\nnum = " << num);
      break;
      }
    lower = mean - m_Multiplier * sqrt(variance);
    upper = mean + m_Multiplier * sqrt(variance);

    // Adjust lower and upper to always contain the seed's intensity, otherwise, no pixels will be
    // returned by the iterator and a zero variance will result
    if (lower > seedIntensity)
      {
      lower = seedIntensity;
      }
    if (upper < seedIntensity)
      {
      upper = seedIntensity;
      }

    // Make sure the lower and upper limit are not outside the valid range of the input 
    if (lower < static_cast<InputRealType>(NumericTraits<InputImagePixelType>::NonpositiveMin()))
      {
      lower = static_cast<InputRealType>(NumericTraits<InputImagePixelType>::NonpositiveMin());
      }
    if (upper > static_cast<InputRealType>(NumericTraits<InputImagePixelType>::max()))
      {
      upper = static_cast<InputRealType>(NumericTraits<InputImagePixelType>::max());
      }
    function->ThresholdBetween(static_cast<InputImagePixelType>(lower),
                               static_cast<InputImagePixelType>(upper));
    
    itkDebugMacro(<< "\nLower intensity = " << lower << ", Upper intensity = " << upper << "\nmean = " << mean << ", variance = " << variance << " , sqrt(variance) = " << sqrt(variance));
    itkDebugMacro(<< "\nsum = " << sum << ", sumOfSquares = " << sumOfSquares << "\nnum = " << num);
    
    // Rerun the segmentation
    outputImage->FillBuffer ( NumericTraits<OutputImagePixelType>::Zero );
    IteratorType thirdIt = IteratorType ( outputImage, function, m_Seed );
    while( !thirdIt.IsAtEnd())
      {
      thirdIt.Set(m_ReplaceValue);
      ++thirdIt;
      }
    }

}


} // end namespace itk

#endif
