/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkThresholdImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkThresholdImageFilter.h"
#include "itkNumericTraits.h"
#include "itkImageTraits.h"
#include "itkObjectFactory.h"

namespace itk
{

/**
 *
 */
template <class TImage>
ThresholdImageFilter<TImage>
::ThresholdImageFilter()
{
  m_OutsideValue = NumericTraits<ImageTraits<TImage>::PixelType>::Zero;
  m_Lower = NumericTraits<PixelType>::min();
  m_Upper = NumericTraits<PixelType>::max();

  typename TImage::Pointer output = TImage::New();
  this->ProcessObject::SetNumberOfRequiredOutputs(2);
  this->ProcessObject::SetNthOutput(1, output.GetPointer());
  output->UnderConstructionOn();
}


/**
 *
 */
template <class TImage>
void 
ThresholdImageFilter<TImage>
::PrintSelf(std::ostream& os, Indent indent)
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Outside Value: " << m_OutsideValue << std::endl;
}

/**
 * The values greater than or equal to the value are set to OutsideValue
 */
template <class TImage>
void 
ThresholdImageFilter<TImage>
::ThresholdAbove(PixelType &thresh)
{
  if (m_Upper != thresh || m_Lower > NumericTraits<PixelType>::min())
    {
    m_Lower = NumericTraits<PixelType>::min();
    m_Upper = thresh;
    this->Modified();
    }
}



/**
 * The values less than or equal to the value are set to OutsideValue
 */
template <class TImage>
void 
ThresholdImageFilter<TImage>
::ThresholdBelow(PixelType &thresh)
{
  if (m_Lower != thresh || m_Upper < NumericTraits<PixelType>::max())
    {
    m_Lower = thresh;
    m_Upper = NumericTraits<PixelType>::max();
    this->Modified();
    }
}


/**
 * The values outside the range are set to OutsideValue
 */
template <class TImage>
void 
ThresholdImageFilter<TImage>
::ThresholdOutside(PixelType &lower, PixelType &upper)
{
  if (lower > upper)
    {
    itkErrorMacro(<<"Lower threshold cannot be greater than upper threshold.");
    return;
    }
  
  if (m_Lower != lower || m_Upper != upper)
    {
    m_Lower = lower;
    m_Upper = upper;
    this->Modified();
    }
}


/**
 *
 */
template <class TImage>
void 
ThresholdImageFilter<TImage>
::ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                       int threadId)
{
  unsigned long i;
  itkDebugMacro(<<"Actually executing");

  // Get the input and output pointers
  InputImagePointer  inputPtr = this->GetInput();
  OutputImagePointer outputPtr = this->GetOutput(0);
  OutputImagePointer outputInversePtr = this->GetOutput(1);

  // Define/declare an iterator that will walk the output region for this
  // thread.
  typedef
    ImageRegionIterator<InputImagePixelType, TImage::ImageDimension>
    InputIterator;
  typedef
    ImageRegionIterator<OutputImagePixelType, TImage::ImageDimension>
    OutputIterator;

  InputIterator  inIt(inputPtr, outputRegionForThread);
  OutputIterator outIt(outputPtr, outputRegionForThread);
  OutputIterator outInverseIt(outputInversePtr, outputRegionForThread);

  // support progress methods/callbacks
  unsigned long updateVisits = 0;
  if ( threadId == 0 )
    {
    updateVisits = 
      outputPtr->GetRequestedRegion().GetNumberOfPixels()/10;
    }
        
  // walk the regions, threshold each pixel
  for (i=0; !outIt.IsAtEnd(); ++inIt, ++outIt, ++outInverseIt, i++ )
    {
    if ( threadId == 0 && !(i % updateVisits ) )
      {
      this->UpdateProgress((float)i/(float(updateVisits)*10.0));
      }

    if (m_Lower <= *inIt && *inIt <= m_Upper)
      {
      // pixel passes to output unchanged and is replaced by m_OutsideValue in
      // the inverse output image
      *outIt = *inIt;
      *outInverseIt = m_OutsideValue;
      }
    else
      {
      *outIt = m_OutsideValue;
      *outInverseIt = *inIt;
      }
    }
  
}

} // end namespace itk
