/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkIsolatedWatershedImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkIsolatedWatershedImageFilter_txx_
#define __itkIsolatedWatershedImageFilter_txx_

#include "itkIsolatedWatershedImageFilter.h"
#include "itkWatershedImageFilter.h"
#include "itkProgressReporter.h"
#include "itkIterationReporter.h"

namespace itk
{

/**
 * Constructor
 */
template <class TInputImage, class TOutputImage>
IsolatedWatershedImageFilter<TInputImage, TOutputImage>
::IsolatedWatershedImageFilter()
{
  m_Threshold = NumericTraits<InputImagePixelType>::Zero;
  m_Seed1.Fill(0);
  m_Seed2.Fill(0);
  m_ReplaceValue1 = NumericTraits<OutputImagePixelType>::One;
  m_ReplaceValue2 = NumericTraits<OutputImagePixelType>::Zero;
  m_IsolatedValue = 0.0;
  m_IsolatedValueTolerance = 0.001;
  m_UpperValueLimit = 1.0;
  m_GradientMagnitude = GradientMagnitudeType::New();
  m_Watershed = WatershedType::New();
}

/**
 * Standard PrintSelf method.
 */
template <class TInputImage, class TOutputImage>
void
IsolatedWatershedImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);
  os << indent << "Threshold: "
     << m_Threshold
     << std::endl;
  os << indent << "UpperValueLimit: "
     << m_UpperValueLimit
     << std::endl;
  os << indent << "ReplaceValue1: "
     << static_cast<typename NumericTraits<OutputImagePixelType>::PrintType>(m_ReplaceValue1)
     << std::endl;
  os << indent << "ReplaceValue2: "
     << static_cast<typename NumericTraits<OutputImagePixelType>::PrintType>(m_ReplaceValue2)
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

template <class TInputImage, class TOutputImage>
void 
IsolatedWatershedImageFilter<TInputImage,TOutputImage>
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
IsolatedWatershedImageFilter<TInputImage,TOutputImage>
::EnlargeOutputRequestedRegion(DataObject *output)
{
  Superclass::EnlargeOutputRequestedRegion(output);
  output->SetRequestedRegionToLargestPossibleRegion();
}

template <class TInputImage, class TOutputImage>
void 
IsolatedWatershedImageFilter<TInputImage,TOutputImage>
::GenerateData()
{
  InputImageConstPointer inputImage = this->GetInput();
  OutputImagePointer outputImage = this->GetOutput();
  OutputImageRegionType region = outputImage->GetRequestedRegion() ;

  // Set up the pipeline
  m_GradientMagnitude->SetInput (inputImage);

  // Set up the Watershed
  m_Watershed->SetInput (m_GradientMagnitude->GetOutput());
  m_Watershed->SetThreshold (m_Threshold);
  m_Watershed->SetLevel (m_UpperValueLimit);

  // Allocate the output
  this->AllocateOutputs();
  
  double lower = m_Threshold;
  double upper = m_UpperValueLimit;
  double guess = upper;

  const unsigned int maximumIterationsInBinarySearch = 
      static_cast< unsigned int > (
         vcl_log(( static_cast<float>( upper ) - static_cast< float >( lower ) ) /
                static_cast<float>( m_IsolatedValueTolerance ) )  / vcl_log(2.0 ) );

  const float progressWeight = 1.0f / static_cast<float>( maximumIterationsInBinarySearch + 2 );
  float cumulatedProgress = 0.0f;

  IterationReporter iterate( this, 0, 1);

  // Do a binary search to find an upper waterlevel that separates the
  // two seeds.
  while (lower + m_IsolatedValueTolerance < guess)
    {
    ProgressReporter progress( this, 0, region.GetNumberOfPixels(), 100, cumulatedProgress, progressWeight );
    cumulatedProgress += progressWeight;
    m_Watershed->SetLevel (guess);
    m_Watershed->Update ();
    if (m_Watershed->GetOutput()->GetPixel(m_Seed1) == m_Watershed->GetOutput()->GetPixel(m_Seed2))
      {
      upper = guess;
      }
    else
      {
      lower = guess;
      }
    guess = (upper + lower) /2;
    iterate.CompletedStep();
    }

  // now produce an output image with the two seeded basins labeled

  ProgressReporter progress( this, 0, region.GetNumberOfPixels(), 100, cumulatedProgress, progressWeight );

  ImageRegionIterator<OutputImageType> ot =
    ImageRegionIterator<OutputImageType>(outputImage, region);
  ImageRegionIterator<ITK_TYPENAME WatershedType::OutputImageType>  it =
    ImageRegionIterator<ITK_TYPENAME WatershedType::OutputImageType>(m_Watershed->GetOutput(), region);

  unsigned long seed1Label = m_Watershed->GetOutput()->GetPixel(m_Seed1);
  unsigned long seed2Label = m_Watershed->GetOutput()->GetPixel(m_Seed2);
  unsigned long value;

  it.GoToBegin();
  ot.GoToBegin();
  while( !it.IsAtEnd())
    {
    value = it.Get();
    if (value == seed1Label)
      {
      ot.Set(m_ReplaceValue1);
      }
    else if (value == seed2Label)
      {
      ot.Set(m_ReplaceValue2);
      }
    else
      {
      ot.Set(NumericTraits<OutputImagePixelType>::Zero);
      }
    ++it;
    ++ot;
    progress.CompletedPixel(); // potential exception thrown here
    }
  m_IsolatedValue = lower;
  iterate.CompletedStep();
}

} // end namespace itk

#endif
