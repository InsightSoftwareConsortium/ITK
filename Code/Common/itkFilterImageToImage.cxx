/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterImageToImage.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkFilterImageToImage.h"
#include "itkObjectFactory.h"

//------------------------------------------------------------------------
template <class TInputImage, class TOutputImage>
itkFilterImageToImage<TInputImage,TOutputImage>::Pointer 
itkFilterImageToImage<TInputImage,TOutputImage>
::New()
{
  itkFilterImageToImage<TInputImage,TOutputImage>* ret = 
    itkObjectFactory< itkFilterImageToImage<TInputImage,TOutputImage> >::Create();
  if ( ret )
    {
    return ret;
    }
  return new itkFilterImageToImage<TInputImage, TOutputImage>();
}

//----------------------------------------------------------------------------
template <class TInputImage, class TOutputImage>
itkFilterImageToImage<TInputImage,TOutputImage>
::itkFilterImageToImage()
{
  // Modify superclass default values, can be overridden by subclasses
  this->SetNumberOfRequiredInputs(1);

}

//----------------------------------------------------------------------------
template <class TInputImage, class TOutputImage>
void 
itkFilterImageToImage<TInputImage,TOutputImage>
::SetInput(TInputImage *input)
{
  this->itkProcessObject::SetNthInput(0, input);
}

//----------------------------------------------------------------------------
template <class TInputImage, class TOutputImage>
TInputImage *
itkFilterImageToImage<TInputImage,TOutputImage>
::GetInput()
{
  if (this->NumberOfInputs < 1)
    {
    return 0;
    }
  
  return (TInputImage *)(this->GetInput(0));
}

  
template <class TInputImage, class TOutputImage>
TInputImage *
itkFilterImageToImage<TInputImage,TOutputImage>
::GetInput(unsigned int idx)
{
  return static_cast<TInputImage *>(this->itkProcessObject::GetInput(idx));
}

//----------------------------------------------------------------------------
template <class TInputImage, class TOutputImage>
void 
itkFilterImageToImage<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, itkIndent indent)
{
  itkImageSource<TOutputImage>::PrintSelf(os,indent);
}

  








