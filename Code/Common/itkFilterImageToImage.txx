/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFilterImageToImage.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkFilterImageToImage.h"


namespace itk
{
  
/**
 *
 */
template <class TInputImage, class TOutputImage>
FilterImageToImage<TInputImage,TOutputImage>
::FilterImageToImage()
{
  // Modify superclass default values, can be overridden by subclasses
  this->SetNumberOfRequiredInputs(1);

}


/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
FilterImageToImage<TInputImage,TOutputImage>
::SetInput(TInputImage *input)
{
  this->ProcessObject::SetNthInput(0, input);
}


/**
 *
 */
template <class TInputImage, class TOutputImage>
FilterImageToImage<TInputImage,TOutputImage>::InputImagePointer
FilterImageToImage<TInputImage,TOutputImage>
::GetInput()
{
  if (this->NumberOfInputs < 1)
    {
    return 0;
    }
  
  return static_cast<TInputImage*>
                    ((DataObject*)(this->ProcessObject::GetInput(0)));
}
  
/**
 *
 */
template <class TInputImage, class TOutputImage>
FilterImageToImage<TInputImage,TOutputImage>::InputImagePointer
FilterImageToImage<TInputImage,TOutputImage>
::GetInput(unsigned int idx)
{
  return static_cast<TInputImage*>
                    ((DataObject*)(this->ProcessObject::GetInput(idx)));
}


/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
FilterImageToImage<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent)
{
  ImageSource<TOutputImage>::PrintSelf(os,indent);
}

} // end namespace itk
