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
// #include "itkImageSource.h"

namespace itk
{

/**
 *
 */
template<class TOutputImage>
ImageSource<TOutputImage>
::ImageSource()
{
  /**
   * Create the output
   */
  typename TOutputImage::Pointer output = TOutputImage::New();
  this->ProcessObject::SetNumberOfRequiredOutputs(1);
  this->ProcessObject::SetNthOutput(0, output.GetPointer());
}


/**
 *
 */
template<class TOutputImage>
ImageSource<TOutputImage>::OutputImagePointer 
ImageSource<TOutputImage>
::GetOutput()
{
  if (this->GetNumberOfOutputs() < 1)
    {
    return 0;
    }
  
  return static_cast<TOutputImage*>
                     (this->ProcessObject::GetOutput(0).GetPointer());
}

  
/**
 *
 */
template<class TOutputImage>
ImageSource<TOutputImage>::OutputImagePointer 
ImageSource<TOutputImage>
::GetOutput(unsigned int idx)
{
  return static_cast<TOutputImage*>
                     (this->ProcessObject::GetOutput(idx).GetPointer());
}


/**
 *
 */
template<class TOutputImage>
void 
ImageSource<TOutputImage>
::SetOutput(TOutputImage *output)
{
  this->ProcessObject::SetNthOutput(0, output);
}


/**
 *
 */
template<class TOutputImage>
void 
ImageSource<TOutputImage>
::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();
}


/**
 *
 */
template<class TOutputImage>
void 
ImageSource<TOutputImage>
::PrintSelf(std::ostream& os, Indent indent)
{
  Superclass::PrintSelf(os,indent);
}

} // end namespace itk
