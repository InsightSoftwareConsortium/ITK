/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkShrinkImage.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkShrinkImage.h"
#include "itkObjectFactory.h"

namespace itk
{

/**
 *
 */
template <class TInputImage, class TOutputImage>
ShrinkImage<TInputImage,TOutputImage>
::ShrinkImage()
{
  m_ShrinkFactor = 1;
}


/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
ShrinkImage<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent)
{
  FilterImageToImage<TInputImage,TOutputImage>::PrintSelf(os,indent);

  os << indent << "Shrink Factor: " << m_ShrinkFactor << std::endl;
}


/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
ShrinkImage<TInputImage,TOutputImage>
::Execute()
{
  itkDebugMacro(<<"Actually executing");
}

} // namespace itk
