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
  m_OutsideValue = NumericTraits<typename TImage::PixelType>::Zero;

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
 *
 */
template <class TImage>
void 
ThresholdImageFilter<TImage>
::ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                       int threadId)
{
  itkDebugMacro(<<"Actually executing");
}

} // end namespace itk
