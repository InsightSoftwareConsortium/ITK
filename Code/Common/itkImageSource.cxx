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
#include "itkObjectFactory.h"

//------------------------------------------------------------------------
<<<<<<< itkImageSource.cxx
itkImageSource::Pointer<T,TDim> itkImageSource::New()
=======
template<class TOutputImage>
itkImageSource<TOutputImage>::Pointer 
itkImageSource<TOutputImage>
::New()
>>>>>>> 1.11
{
<<<<<<< itkImageSource.cxx
  return itkImageSource::Pointer(new itkImageSource<T,TDim>);
=======
  itkImageSource<TOutputImage>* ret = 
    itkObjectFactory< itkImageSource<TOutputImage> >::Create();
  if ( ret )
    {
    return ret;
    }
  return new itkImageSource<TOutputImage>;
>>>>>>> 1.11
}

//----------------------------------------------------------------------------
template<class TOutputImage>
itkImageSource<TOutputImage>
::itkImageSource()
{
<<<<<<< itkImageSource.cxx
=======
  // Create the output
  typename TOutputImage::Pointer output = TOutputImage::New();
  this->itkProcessObject::SetNumberOfRequiredOutputs(1);
  this->itkProcessObject::SetNthOutput(0, output.GetPointer());

>>>>>>> 1.11
  m_ExecutePiece = 0;
  m_ExecuteNumberOfPieces = 0;
}

//----------------------------------------------------------------------------
<<<<<<< itkImageSource.cxx
itkImage<T,TDim> *itkImageSource::GetOutput()
=======
template<class TOutputImage>
TOutputImage *
itkImageSource<TOutputImage>
::GetOutput()
>>>>>>> 1.11
{
  if (this->GetNumberOfOutputs() < 1)
    {
    return 0;
    }
  
  return (TOutputImage *)(this->GetOutput(0));
}

  
<<<<<<< itkImageSource.cxx
  return (itkImage<T,TDim> *)(this->GetOutput(0));
=======
//----------------------------------------------------------------------------
template<class TOutputImage>
TOutputImage *
itkImageSource<TOutputImage>
::GetOutput(unsigned int idx)
{
  return static_cast<TOutputImage *>(this->itkProcessObject::GetOutput(idx));
>>>>>>> 1.11
}

//----------------------------------------------------------------------------
<<<<<<< itkImageSource.cxx
void itkImageSource::SetOutput(itkImage<T,TDim> *output)
=======
template<class TOutputImage>
void 
itkImageSource<TOutputImage>
::SetOutput(TOutputImage *output)
>>>>>>> 1.11
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

  








