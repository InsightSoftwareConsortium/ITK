/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkShrinkImage.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkShrinkImage.h"

//------------------------------------------------------------------------
template <class TInputImage, class TOutputImage>
itkShrinkImage<TInputImage,TOutputImage>::Pointer 
itkShrinkImage<TInputImage,TOutputImage>
::New()
{
  return itkShrinkImage::Pointer(
    new itkShrinkImage<TInputImage,TOutputImage>);
}

//----------------------------------------------------------------------------
template <class TInputImage, class TOutputImage>
itkShrinkImage<TInputImage,TOutputImage>
::itkShrinkImage()
{
  m_ShrinkFactor = 1;
}

//----------------------------------------------------------------------------
template <class TInputImage, class TOutputImage>
void itkShrinkImage<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, itkIndent indent)
{
  itkImageToImageFilter<TInputImage,TOutputImage>::PrintSelf(os,indent);

  os << indent << "Shrink Factor: " << m_ShrinkFactor << endl;
}

//----------------------------------------------------------------------------
template <class TInputImage, class TOutputImage>
void itkShrinkImage<TInputImage,TOutputImage>
::Execute()
{
  itkDebugMacro(<<"Actually executing");
}
