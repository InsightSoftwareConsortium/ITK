/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageSource.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkImageSource.h"
#include "itkObjectFactory.h"

//------------------------------------------------------------------------
template<class TOutputImage>
itkImageSource<TOutputImage>::Pointer 
itkImageSource<TOutputImage>
::New()
{
  itkImageSource<TOutputImage>* ret = 
    itkObjectFactory< itkImageSource<TOutputImage> >::Create();
  if ( ret )
    {
    return ret;
    }
  return new itkImageSource<TOutputImage>;
}

//----------------------------------------------------------------------------
template<class TOutputImage>
itkImageSource<TOutputImage>
::itkImageSource()
{
  // Create the output
  this->itkProcessObject::SetNumberOfRequiredOutputs(1);
  this->itkProcessObject::SetNthOutput(0, TOutputImage::New());

  m_ExecutePiece = 0;
  m_ExecuteNumberOfPieces = 0;
}

//----------------------------------------------------------------------------
template<class TOutputImage>
TOutputImage *
itkImageSource<TOutputImage>
::GetOutput()
{
  if (this->GetNumberOfOutputs() < 1)
    {
    return 0;
    }
  
  return (TOutputImage *)(this->GetOutput(0));
}

  
//----------------------------------------------------------------------------
template<class TOutputImage>
TOutputImage *
itkImageSource<TOutputImage>
::GetOutput(int idx)
{
  return static_cast<TOutputImage *>(this->itkProcessObject::GetOutput(idx));
}

//----------------------------------------------------------------------------
template<class TOutputImage>
void 
itkImageSource<TOutputImage>
::SetOutput(TOutputImage *output)
{
  this->itkProcessObject::SetNthOutput(0, output);
}

//----------------------------------------------------------------------------
template<class TOutputImage>
void 
itkImageSource<TOutputImage>
::ComputeInputUpdateExtents(itkDataObject *data)
{
}

//----------------------------------------------------------------------------
template<class TOutputImage>
void 
itkImageSource<TOutputImage>
::PrintSelf(std::ostream& os, itkIndent indent)
{
  itkProcessObject::PrintSelf(os,indent);
}

  








