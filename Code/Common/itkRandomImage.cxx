/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRandomImage.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkRandomImage.h"

//------------------------------------------------------------------------
template <class TOutputImage>
itkRandomImage<TOutputImage>::Pointer 
itkRandomImage<TOutputImage>
::New()
{
  return itkRandomImage::Pointer(new itkRandomImage<TOutputImage>);
}

//----------------------------------------------------------------------------
template <class TOutputImage>
itkRandomImage<TOutputImage>
::itkRandomImage()
{
}

//----------------------------------------------------------------------------
template <class TOutputImage>
void itkRandomImage<TOutputImage>
::PrintSelf(std::ostream& os, itkIndent indent)
{
  itkImageSource<TOutputImage>::PrintSelf(os,indent);

}

//----------------------------------------------------------------------------
template <class TOutputImage>
void itkRandomImage<TOutputImage>
::Execute()
{
  itkDebugMacro(<<"Actually executing");
}
