/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRandomImageSource.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkRandomImageSource.h"
#include "itkObjectFactory.h"

//------------------------------------------------------------------------
template <class TOutputImage>
itkRandomImageSource<TOutputImage>::Pointer 
itkRandomImageSource<TOutputImage>
::New()
{
  itkRandomImageSource<TOutputImage>* ret = 
    itkObjectFactory< itkRandomImageSource<TOutputImage> >::Create();
  if ( ret )
    {
    return ret;
    }
  return 
    itkRandomImageSource<TOutputImage>::Pointer(
      new itkRandomImageSource<TOutputImage>);
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
  TOutputImage *image=this->GetOutput(0);
  TOutputImage::Index ind;
  long index[2];

  itkDebugMacro(<<"Generating random image");
  
  index[0] = 0;
  index[1] = 0;
  ind.SetIndex(index);
  
  
}
