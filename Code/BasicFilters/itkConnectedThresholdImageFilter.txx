/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConnectedThresholdImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
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

  typename InputPixelObjectType::Pointer lower = InputPixelObjectType::New();
  lower->Set( NumericTraits< InputImagePixelType >::NonpositiveMin() );
  this->ProcessObject::SetNthInput( 1, lower );

  typename InputPixelObjectType::Pointer upper = InputPixelObjectType::New();
  upper->Set( NumericTraits< InputImagePixelType >::max() );
  this->ProcessObject::SetNthInput( 2, upper );

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
  os << indent << "Upper: "
     << static_cast<typename NumericTraits<OutputImagePixelType>::PrintType>(m_Upper)
     << std::endl;
  os << indent << "Lower: "
     << static_cast<typename NumericTraits<OutputImagePixelType>::PrintType>(m_Lower)
     << std::endl;
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
::SetLowerInput( const InputPixelObjectType * input )
{
  if (input != this->GetLowerInput())
    {
    this->ProcessObject::SetNthInput(1,
                                     const_cast<InputPixelObjectType*>(input));
    this->Modified();
    }
}


template <class TInputImage, class TOutputImage>
void 
ConnectedThresholdImageFilter<TInputImage,TOutputImage>
::SetUpperInput( const InputPixelObjectType * input )
{
  if (input != this->GetUpperInput())
    {
    this->ProcessObject::SetNthInput(2,
                                     const_cast<InputPixelObjectType*>(input));
    this->Modified();
    }
}

template <class TInputImage, class TOutputImage>
typename ConnectedThresholdImageFilter<TInputImage, TOutputImage>::InputPixelObjectType *
ConnectedThresholdImageFilter<TInputImage,TOutputImage>
::GetLowerInput()
{
  typename InputPixelObjectType::Pointer lower
    = static_cast<InputPixelObjectType *>(this->ProcessObject::GetInput(1));
  if (!lower)
    {
    // no input object available, create a new one and set it to the
    // default threshold
    lower = InputPixelObjectType::New();
    lower->Set( NumericTraits<InputImagePixelType>::NonpositiveMin() );
    this->ProcessObject::SetNthInput( 1, lower );
    }
    
  return lower;
}


template <class TInputImage, class TOutputImage>
typename ConnectedThresholdImageFilter<TInputImage, TOutputImage>::InputPixelObjectType *
ConnectedThresholdImageFilter<TInputImage,TOutputImage>
::GetUpperInput()
{
  typename InputPixelObjectType::Pointer upper
    = static_cast<InputPixelObjectType *>(this->ProcessObject::GetInput(1));
  if (!upper)
    {
    // no input object available, create a new one and set it to the
    // default threshold
    upper = InputPixelObjectType::New();
    upper->Set( NumericTraits<InputImagePixelType>::NonpositiveMin() );
    this->ProcessObject::SetNthInput( 2, upper );
    }
    
  return upper;
}


template <class TInputImage, class TOutputImage>
void 
ConnectedThresholdImageFilter<TInputImage,TOutputImage>
::GenerateData()
{
  InputImageConstPointer inputImage = this->GetInput();
  OutputImagePointer outputImage = this->GetOutput();

  typename InputPixelObjectType::Pointer lowerThreshold=this->GetLowerInput();
  typename InputPixelObjectType::Pointer upperThreshold=this->GetUpperInput();

  m_Lower = lowerThreshold->Get();
  m_Upper = upperThreshold->Get();

std::cout << "Lower = " << m_Lower << std::endl;
std::cout << "Upper = " << m_Upper << std::endl;

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
