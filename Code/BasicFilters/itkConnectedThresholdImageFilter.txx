/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConnectedThresholdImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkConnectedThresholdImageFilter_txx_
#define __itkConnectedThresholdImageFilter_txx_

#include "itkConnectedThresholdImageFilter.h"
#include "itkBinaryThresholdImageFunction.h"
#include "itkFloodFilledImageFunctionConditionalIterator.h"
#include "itkProgressReporter.h"

namespace itk
{

/**
 * Constructor
 */
template <class TInputImage, class TOutputImage>
ConnectedThresholdImageFilter<TInputImage, TOutputImage>
::ConnectedThresholdImageFilter()
{
  m_Lower = NumericTraits<InputImagePixelType>::NonpositiveMin();
  m_Upper = NumericTraits<InputImagePixelType>::max();
  m_ReplaceValue = NumericTraits<OutputImagePixelType>::One;
}

/**
 * Standard PrintSelf method.
 */
template <class TInputImage, class TOutputImage>
void
ConnectedThresholdImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  this->Superclass::PrintSelf(os, indent);
  os << indent << "Upper: " << m_Upper << std::endl;
  os << indent << "Lower: " << m_Lower << std::endl;
  os << indent << "ReplaceValue: "
     << static_cast<typename NumericTraits<OutputImagePixelType>::PrintType>(m_ReplaceValue)
     << std::endl;
}

template <class TInputImage, class TOutputImage>
void 
ConnectedThresholdImageFilter<TInputImage,TOutputImage>
::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();
  if ( this->GetInput() )
    {
    InputImagePointer image = 
      const_cast< InputImageType * >( this->GetInput() );
    image->SetRequestedRegionToLargestPossibleRegion();
    }
}

template <class TInputImage, class TOutputImage>
void 
ConnectedThresholdImageFilter<TInputImage,TOutputImage>
::EnlargeOutputRequestedRegion(DataObject *output)
{
  Superclass::EnlargeOutputRequestedRegion(output);
  output->SetRequestedRegionToLargestPossibleRegion();
}

template <class TInputImage, class TOutputImage>
void 
ConnectedThresholdImageFilter<TInputImage,TOutputImage>
::GenerateData()
{
  InputImageConstPointer inputImage = this->GetInput();
  OutputImagePointer outputImage = this->GetOutput();

  // Zero the output
  OutputImageRegionType region =  outputImage->GetRequestedRegion();
  outputImage->SetBufferedRegion( region );
  outputImage->Allocate();
  outputImage->FillBuffer ( NumericTraits<OutputImagePixelType>::Zero );
  
  typedef BinaryThresholdImageFunction<InputImageType> FunctionType;
  typedef FloodFilledImageFunctionConditionalIterator<OutputImageType, FunctionType> IteratorType;

  typename FunctionType::Pointer function = FunctionType::New();
    function->SetInputImage ( inputImage );
    function->ThresholdBetween ( m_Lower, m_Upper );

  ProgressReporter progress(this, 0, region.GetNumberOfPixels());

  IteratorType it ( outputImage, function, m_SeedList );
  it.GoToBegin();

  while( !it.IsAtEnd())
    {
    it.Set(m_ReplaceValue);
    ++it;
    progress.CompletedPixel();  // potential exception thrown here
    }
}


} // end namespace itk

#endif
