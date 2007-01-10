/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToPathFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageToPathFilter_txx
#define _itkImageToPathFilter_txx
#include "itkImageToPathFilter.h"

namespace itk
{

/**
 *
 */
template <class TInputImage, class TOutputPath>
ImageToPathFilter<TInputImage,TOutputPath>
::ImageToPathFilter()
{
  // Modify superclass default values, can be overridden by subclasses
  this->SetNumberOfRequiredInputs(1);
}

/**
 *
 */
template <class TInputImage, class TOutputPath>
ImageToPathFilter<TInputImage,TOutputPath>
::~ImageToPathFilter()
{
}
  

/**
 *
 */
template <class TInputImage, class TOutputPath>
void 
ImageToPathFilter<TInputImage,TOutputPath>
::SetInput(const InputImageType *input)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput(0, 
                                   const_cast< InputImageType * >( input ) );
}


/**
 * Connect one of the operands for pixel-wise addition
 */
template <class TInputImage, class TOutputPath>
void
ImageToPathFilter<TInputImage,TOutputPath>
::SetInput( unsigned int index, const TInputImage * image ) 
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput(index, 
                                   const_cast< TInputImage *>( image ) );
}



/**
 *
 */
template <class TInputImage, class TOutputPath>
const typename ImageToPathFilter<TInputImage,TOutputPath>::InputImageType *
ImageToPathFilter<TInputImage,TOutputPath>
::GetInput(void) 
{
  if (this->GetNumberOfInputs() < 1)
    {
    return 0;
    }
  
  return static_cast<const TInputImage * >
    (this->ProcessObject::GetInput(0) );
}
  
/**
 *
 */
template <class TInputImage, class TOutputPath>
const typename ImageToPathFilter<TInputImage,TOutputPath>::InputImageType *
ImageToPathFilter<TInputImage,TOutputPath>
::GetInput(unsigned int idx)
{
  return static_cast< const TInputImage * >
    (this->ProcessObject::GetInput(idx));
}


template<class TInputImage, class TOutputPath>
void 
ImageToPathFilter<TInputImage,TOutputPath>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}



} // end namespace itk

#endif
