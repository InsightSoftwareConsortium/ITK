/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWriteVTKImage.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkWriteVTKImage.h"
#include "itkObjectFactory.h"

//------------------------------------------------------------------------
template <class TInputImage>
itkWriteVTKImage<TInputImage>::Pointer itkWriteVTKImage<TInputImage>
::New()
{
  itkWriteVTKImage<TInputImage>* ret = 
    itkObjectFactory< itkWriteVTKImage<TInputImage> >::Create();
  if ( ret )
    {
    return ret;
    }
  return new itkWriteVTKImage<TInputImage>;
}

//----------------------------------------------------------------------------
template <class TInputImage>
void 
itkWriteVTKImage<TInputImage>
::WriteData()
{
  itkDebugMacro(<<"Writing image in VTK format");
}

//----------------------------------------------------------------------------
template <class TInputImage>
void 
itkWriteVTKImage<TInputImage>
::PrintSelf(std::ostream& os, itkIndent indent)
{
  itkWriteImage<TInputImage>::PrintSelf(os,indent);

}


