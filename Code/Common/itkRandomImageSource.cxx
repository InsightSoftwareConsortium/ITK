/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRandomImageSource.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkRandomImageSource.h"

//------------------------------------------------------------------------
template <class TOutputImage>
itkRandomImageSource<TOutputImage>::Pointer 
itkRandomImageSource<TOutputImage>
::New()
{
  return itkRandomImageSource::Pointer(new itkRandomImageSource<TOutputImage>);
}

//----------------------------------------------------------------------------
template <class TOutputImage>
itkRandomImageSource<TOutputImage>
::itkRandomImageSource()
{
}

//----------------------------------------------------------------------------
template <class TOutputImage>
void itkRandomImageSource<TOutputImage>
::PrintSelf(std::ostream& os, itkIndent indent)
{
  itkImageSource<TOutputImage>::PrintSelf(os,indent);

}

//----------------------------------------------------------------------------
template <class TOutputImage>
void itkRandomImageSource<TOutputImage>
::Execute()
{
  itkDebugMacro(<<"Actually executing");
}
