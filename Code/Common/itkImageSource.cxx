/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageSource.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkImageSource.h"


//------------------------------------------------------------------------
itkImageSource::Pointer<T> itkImageSource::New()
{
  return itkImageSource::Pointer(new itkImageSource<T>);
}

//----------------------------------------------------------------------------
itkImageSource::itkImageSource()
{
  this->itkProcessObject::SetNthOutput(0, itkImage::New());

  // Releasing data for pipeline parallism.
  // Filters will know it is empty. 
  this->GetOutput(0)->ReleaseData();
  this->GetOutput(0)->Delete();

  m_ExecutePiece = 0;
  m_ExecuteNumberOfPieces = 0;
}

//----------------------------------------------------------------------------
itkImage<T> *itkImageSource::GetOutput()
{
  if (this->NumberOfOutputs < 1)
    {
    return 0;
    }
  
  return (itkImage<T> *)(this->GetOutput(0));
}

//----------------------------------------------------------------------------
void itkImageSource::SetOutput(itkImage<T> *output)
{
  this->itkProcessObject::SetNthOutput(0, output);
}


//----------------------------------------------------------------------------
void itkImageSource::ComputeInputUpdateExtents(itkDataObject *data)
{
}

  








