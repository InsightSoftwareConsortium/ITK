/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkConfidenceConnectedImageFilter.txx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
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
#include "itkSumOfSquaresImageFunction.h"
#include "itkBinaryThresholdImageFunction.h"
#include "itkFloodFilledImageFunctionConditionalIterator.h"
#include "itkFloodFilledImageFunctionConditionalConstIterator.h"
#include "itkNumericTraits.h"
#include "itkProgressReporter.h"

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
  m_Seeds.clear();
  m_InitialNeighborhoodRadius = 1;
  m_ReplaceValue = NumericTraits<OutputImagePixelType>::One;
  m_Mean     = NumericTraits< InputRealType >::Zero;
  m_Variance = NumericTraits< InputRealType >::Zero;
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
  os << indent << "Mean of the connected region: " << m_Mean 
     << std::endl;
  os << indent << "Variance of the connected region: " << m_Variance 
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
  typedef BinaryThresholdImageFunction<InputImageType> FunctionType;
  typedef BinaryThresholdImageFunction<OutputImageType> SecondFunctionType;
  typedef FloodFilledImageFunctionConditionalIterator<OutputImageType, FunctionType> IteratorType;
  typedef FloodFilledImageFunctionConditionalConstIterator<InputImageType, SecondFunctionType> SecondIteratorType;

  unsigned int loop;
  unsigned long num;
  
  typename Superclass::InputImageConstPointer inputImage  = this->GetInput();
  typename Superclass::OutputImagePointer     outputImage = this->GetOutput();

  // Zero the output
  OutputImageRegionType region = outputImage->GetRequestedRegion();
  outputImage->SetBufferedRegion( region );
  outputImage->Allocate();
  outputImage->FillBuffer ( NumericTraits<OutputImagePixelType>::Zero );

  // Compute the statistics of the seed point
  typename MeanImageFunction<InputImageType>::Pointer meanFunction
    = MeanImageFunction<InputImageType>::New();
  meanFunction->SetInputImage( inputImage );
  meanFunction->SetNeighborhoodRadius( m_InitialNeighborhoodRadius );
  typename SumOfSquaresImageFunction<InputImageType>::Pointer sumOfSquaresFunction
    = SumOfSquaresImageFunction<InputImageType>::New();
  sumOfSquaresFunction->SetInputImage( inputImage );
  sumOfSquaresFunction->SetNeighborhoodRadius( m_InitialNeighborhoodRadius );
  
  // Set up the image function used for connectivity
  typename FunctionType::Pointer function = FunctionType::New();
  function->SetInputImage ( inputImage );

  InputRealType lower;
  InputRealType upper;

  m_Mean     = itk::NumericTraits<InputRealType>::Zero;
  m_Variance = itk::NumericTraits<InputRealType>::Zero;

  if( m_InitialNeighborhoodRadius > 0 )
    {
    InputRealType sumOfSquares = itk::NumericTraits<InputRealType>::Zero;

    typename SeedsContainerType::const_iterator si = m_Seeds.begin();
    typename SeedsContainerType::const_iterator li = m_Seeds.end();
    while( si != li )
      {
      m_Mean     += meanFunction->EvaluateAtIndex( *si );
      sumOfSquares += sumOfSquaresFunction->EvaluateAtIndex( *si );
      si++;
      }
    const unsigned int num = m_Seeds.size();
    const unsigned int totalNum = num * sumOfSquaresFunction->GetNeighborhoodSize();
    m_Mean     /= num;
    m_Variance  = (sumOfSquares - (m_Mean * m_Mean * double(totalNum))) / (double(totalNum) - 1.0);
    }
  else 
    {
    InputRealType sum = itk::NumericTraits<InputRealType>::Zero;
    InputRealType sumOfSquares = itk::NumericTraits<InputRealType>::Zero;

    typename SeedsContainerType::const_iterator si = m_Seeds.begin();
    typename SeedsContainerType::const_iterator li = m_Seeds.end();
    while( si != li )
      {
      const InputRealType value = 
        static_cast< InputRealType >( inputImage->GetPixel( *si ) );

      sum += value;
      sumOfSquares += value * value;
      si++;
      }
    const unsigned int num = m_Seeds.size();
    m_Mean      = sum/double(num);
    m_Variance  = (sumOfSquares - (sum*sum / double(num))) / (double(num) - 1.0);
    }


  lower = m_Mean - m_Multiplier * vcl_sqrt(m_Variance );
  upper = m_Mean + m_Multiplier * vcl_sqrt(m_Variance );
  
  // Find the highest and lowest seed intensity.
  InputRealType lowestSeedIntensity = itk::NumericTraits<InputImagePixelType>::max();
  InputRealType highestSeedIntensity = itk::NumericTraits<InputImagePixelType>::Zero;
  typename SeedsContainerType::const_iterator si = m_Seeds.begin();
  typename SeedsContainerType::const_iterator li = m_Seeds.end();
  while( si != li )
    {
    const InputRealType seedIntensity = 
      static_cast<InputRealType>(inputImage->GetPixel( *si ));

    if (lowestSeedIntensity > seedIntensity)
      {
      lowestSeedIntensity = seedIntensity;
      }
    if (highestSeedIntensity < seedIntensity)
      {
      highestSeedIntensity = seedIntensity;
      }

    si++;
    }


  // Adjust lower and upper to always contain the seed's intensity, otherwise, no pixels will be
  // returned by the iterator and a zero variance will result
  if (lower > lowestSeedIntensity)
    {
    lower = lowestSeedIntensity;
    }
  if (upper < highestSeedIntensity)
    {
    upper = highestSeedIntensity;
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

  itkDebugMacro(<< "\nLower intensity = " << lower << ", Upper intensity = " << upper << "\nmean = " << m_Mean << " , vcl_sqrt(variance) = " << vcl_sqrt(m_Variance ));


  // Segment the image, the iterator walks the output image (so Set()
  // writes into the output image), starting at the seed point.  As
  // the iterator walks, if the corresponding pixel in the input image
  // (accessed via the "function" assigned to the iterator) is within
  // the [lower, upper] bounds prescribed, the pixel is added to the
  // output segmentation and its neighbors become candidates for the
  // iterator to walk.
  IteratorType it = IteratorType ( outputImage, function, m_Seeds );
  it.GoToBegin();
  while( !it.IsAtEnd())
    {
    it.Set(m_ReplaceValue);
    ++it;
    }

  ProgressReporter progress(this, 0, region.GetNumberOfPixels() * m_NumberOfIterations );

  for (loop = 0; loop < m_NumberOfIterations; ++loop)
    {
    // Now that we have an initial segmentation, let's recalculate the
    // statistics.  Since we have already labelled the output, we visit
    // pixels in the input image that have been set in the output image.
    // Essentially, we flip the iterator around, so we walk the input
    // image (so Get() will get pixel values from the input) and constrain
    // iterator such it only visits pixels that were set in the output.
    typename SecondFunctionType::Pointer secondFunction = SecondFunctionType::New();
    secondFunction->SetInputImage ( outputImage );
    secondFunction->ThresholdBetween( m_ReplaceValue, m_ReplaceValue );

    typename NumericTraits<ITK_TYPENAME InputImageType::PixelType>::RealType sum, sumOfSquares;
    sum = NumericTraits<InputRealType>::Zero;
    sumOfSquares = NumericTraits<InputRealType>::Zero;
    num = 0;
    
    SecondIteratorType sit
      = SecondIteratorType ( inputImage, secondFunction, m_Seeds );
    sit.GoToBegin();
    while( !sit.IsAtEnd())
      {
      const InputRealType value = static_cast<InputRealType>(sit.Get());
      sum += value;
      sumOfSquares += value * value;
      ++num;
      ++sit;
      }
    m_Mean      = sum / double(num);
    m_Variance  = (sumOfSquares - (sum*sum / double(num))) / (double(num) - 1.0);
    // if the variance is zero, there is no point in continuing
    if ( m_Variance == 0 )
      {
      itkDebugMacro(<< "\nLower intensity = " << lower << ", Upper intensity = " << upper << "\nmean = " << m_Mean << ", variance = " << m_Variance << " , vcl_sqrt(variance) = " << vcl_sqrt(m_Variance));
      itkDebugMacro(<< "\nsum = " << sum << ", sumOfSquares = " << sumOfSquares << "\nnum = " << num);
      break;
      }
    lower = m_Mean - m_Multiplier * vcl_sqrt(m_Variance );
    upper = m_Mean + m_Multiplier * vcl_sqrt(m_Variance );

    // Adjust lower and upper to always contain the seed's intensity, otherwise, no pixels will be
    // returned by the iterator and a zero variance will result
    if (lower > lowestSeedIntensity)
      {
      lower = lowestSeedIntensity;
      }
    if (upper < highestSeedIntensity)
      {
      upper = highestSeedIntensity;
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
    
    itkDebugMacro(<< "\nLower intensity = " << lower << ", Upper intensity = " << upper << "\nmean = " << m_Mean << ", variance = " << m_Variance << " , vcl_sqrt(variance) = " << vcl_sqrt(m_Variance ));
    itkDebugMacro(<< "\nsum = " << sum << ", sumOfSquares = " << sumOfSquares << "\nnum = " << num);
    


    // Rerun the segmentation, the iterator walks the output image,
    // starting at the seed point.  As the iterator walks, if the
    // corresponding pixel in the input image (accessed via the
    // "function" assigned to the iterator) is within the [lower,
    // upper] bounds prescribed, the pixel is added to the output
    // segmentation and its neighbors become candidates for the
    // iterator to walk.
    outputImage->FillBuffer ( NumericTraits<OutputImagePixelType>::Zero );
    IteratorType thirdIt = IteratorType ( outputImage, function, m_Seeds );
    thirdIt.GoToBegin();
    try
      {
      while( !thirdIt.IsAtEnd())
        {
        thirdIt.Set(m_ReplaceValue);
        ++thirdIt;
        progress.CompletedPixel();  // potential exception thrown here
        }

      } 
    catch( ProcessAborted & )
      {
      break; // interrupt the iterations loop
      }
    
    }  // end iteration loop

  if( this->GetAbortGenerateData() )
    {
    ProcessAborted e(__FILE__,__LINE__);
    e.SetLocation(ITK_LOCATION);
    e.SetDescription("Process aborted.");
    throw ProcessAborted(__FILE__,__LINE__);
    }
}


} // end namespace itk

#endif
