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
#include "itkVTKImageReader.h"

//------------------------------------------------------------------------
template <class TOutputImage>
itkVTKImageReader<TOutputImage>::Pointer itkVTKImageReader<TOutputImage>
::New()
{
  return itkVTKImageReader::Pointer(new itkVTKImageReader<TOutputImage>);
}

//----------------------------------------------------------------------------
template <class TOutputImage>
itkVTKImageReader<TOutputImage>
::itkVTKImageReader()
{
  m_FileName = "";
}

//----------------------------------------------------------------------------
template <class TOutputImage>
void itkVTKImageReader<TOutputImage>
::Execute()
{
}

//----------------------------------------------------------------------------
template <class TOutputImage>
void itkVTKImageReader<TOutputImage>
::PrintSelf(std::ostream& os, itkIndent indent)
{
  itkImageSource<TOutputImage>::PrintSelf(os,indent);

}


