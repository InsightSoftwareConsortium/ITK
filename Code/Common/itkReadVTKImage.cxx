/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkReadVTKImage.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkReadVTKImage.h"
#include "itkObjectFactory.h"

//------------------------------------------------------------------------
template <class TOutputImage>
itkReadVTKImage<TOutputImage>::Pointer itkReadVTKImage<TOutputImage>
::New()
{
  itkReadVTKImage<TOutputImage>* ret = 
    itkObjectFactory< itkReadVTKImage<TOutputImage> >::Create();
  if ( ret )
    {
    return ret;
    }
  return new itkReadVTKImage<TOutputImage>;
}

//----------------------------------------------------------------------------
template <class TOutputImage>
itkReadVTKImage<TOutputImage>
::itkReadVTKImage()
{
  m_FileName = "";
}

//----------------------------------------------------------------------------
template <class TOutputImage>
void 
itkReadVTKImage<TOutputImage>
::Execute()
{
}

//----------------------------------------------------------------------------
template <class TOutputImage>
void 
itkReadVTKImage<TOutputImage>
::PrintSelf(std::ostream& os, itkIndent indent)
{
  itkImageSource<TOutputImage>::PrintSelf(os,indent);

}


