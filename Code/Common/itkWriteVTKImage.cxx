/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWriteVTKImage.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkVTKImageWriter.h"

//------------------------------------------------------------------------
template <class TInputImage>
itkVTKImageWriter<TInputImage>::Pointer itkVTKImageWriter
::New()
{
  return itkVTKImageWriter::Pointer(new itkVTKImageWriter<TInputImage>);
}

//----------------------------------------------------------------------------
template <class TInputImage>
void itkVTKImageWriter<TInputImage>
::WriteData()
{


//----------------------------------------------------------------------------
template <class TInputImage>
void itkVTKImageWriter<TInputImage>
::PrintSelf(std::ostream& os, itkIndent indent)
{
  itkImageWriter<TInputImage>::PrintSelf(os,indent);

}

