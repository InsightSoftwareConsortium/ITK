/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToImageFilter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkImageToImageFilter.h"
#include "itkObjectFactory.h"

//------------------------------------------------------------------------
template <class TInputImage, class TOutputImage>
itkImageToImageFilter<TInputImage,TOutputImage>::Pointer 
itkImageToImageFilter<TInputImage,TOutputImage>
::New()
{
  itkImageToImageFilter<TInputImage,TOutputImage>* ret = 
    itkObjectFactory< itkImageToImageFilter<TInputImage,TOutputImage> >::Create();
  if ( ret )
    {
    return ret;
    }
  return
    itkImageToImageFilter<TInputImage,TOutputImage>::Pointer(
      new itkImageToImageFilter<TInputImage, TOutputImage>);
}

//----------------------------------------------------------------------------
template <class TInputImage, class TOutputImage>
itkImageToImageFilter<TInputImage,TOutputImage>
::itkImageToImageFilter()
{
  // Modify superclass default values, can be overridden by subclasses
  this->SetNumberOfRequiredInputs(1);

}

//----------------------------------------------------------------------------
template <class TInputImage, class TOutputImage>
void itkImageToImageFilter<TInputImage,TOutputImage>
::SetInput(TInputImage *input)
{
  this->itkProcessObject::SetNthInput(0, input);
}

//----------------------------------------------------------------------------
template <class TInputImage, class TOutputImage>
TInputImage *itkImageToImageFilter<TInputImage,TOutputImage>
::GetInput()
{
  if (this->NumberOfInputs < 1)
    {
    return 0;
    }
  
  return (TInputImage *)(this->GetInput(0));
}

//----------------------------------------------------------------------------
template <class TInputImage, class TOutputImage>
void itkImageToImageFilter<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, itkIndent indent)
{
  itkImageSource<TOutputImage>::PrintSelf(os,indent);
}

  








