/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkImageToImageFilter_txx
#define _itkImageToImageFilter_txx
#include "itkImageToImageFilter.h"


namespace itk
{

/**
 *
 */
template <class TInputImage, class TOutputImage>
ImageToImageFilter<TInputImage,TOutputImage>
::ImageToImageFilter()
{
  // Modify superclass default values, can be overridden by subclasses
  this->SetNumberOfRequiredInputs(1);
}

/**
 *
 */
template <class TInputImage, class TOutputImage>
ImageToImageFilter<TInputImage,TOutputImage>
::~ImageToImageFilter()
{
}
  

/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
ImageToImageFilter<TInputImage,TOutputImage>
::SetInput(InputImageType *input)
{
  this->ProcessObject::SetNthInput(0, input);
}


/**
 *
 */
template <class TInputImage, class TOutputImage>
ImageToImageFilter<TInputImage,TOutputImage>::InputImagePointer
ImageToImageFilter<TInputImage,TOutputImage>
::GetInput()
{
  if (this->GetNumberOfInputs() < 1)
    {
    return 0;
    }
  
  return static_cast<TInputImage*>
                     (this->ProcessObject::GetInput(0).GetPointer());
}
  
/**
 *
 */
template <class TInputImage, class TOutputImage>
ImageToImageFilter<TInputImage,TOutputImage>::InputImagePointer
ImageToImageFilter<TInputImage,TOutputImage>
::GetInput(unsigned int idx)
{
  return static_cast<TInputImage*>
                     (this->ProcessObject::GetInput(idx).GetPointer());
}


/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
ImageToImageFilter<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent)
{
  Superclass::PrintSelf(os,indent);
}

//-----------------------------------------------------------------------
//
template<class TInputImage, class TOutputImage>
void 
ImageToImageFilter<TInputImage,TOutputImage>
::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();

  for (unsigned int idx = 0; idx < this->GetNumberOfInputs(); ++idx)
    {
    if (this->GetInput(idx))
      {
      this->GetInput(idx)
        ->SetRequestedRegion(this->GetOutput()->GetRequestedRegion());
      }
    }  
}

} // end namespace itk

#endif
