/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPathAndImageToPathFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkPathAndImageToPathFilter_txx
#define __itkPathAndImageToPathFilter_txx

#include "itkPathAndImageToPathFilter.h"


namespace itk
{

/**
 *
 */
template <class TInputPath, class TInputImage, class TOutputPath>
PathAndImageToPathFilter<TInputPath,TInputImage,TOutputPath>
::PathAndImageToPathFilter()
{
  // Modify superclass default values, can be overridden by subclasses
  this->SetNumberOfRequiredInputs(2);
}

/**
 *
 */
template <class TInputPath, class TInputImage, class TOutputPath>
void
PathAndImageToPathFilter<TInputPath,TInputImage,TOutputPath>
::SetPathInput(const InputPathType *path)
{
  // We have 2 inputs:  a path and an image
  if( 2 > this->GetNumberOfInputs() )
    {
    this->SetNumberOfRequiredInputs( 2 );
    }
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput(0, 
                                   const_cast< InputPathType * >( path ) );
}

template <class TInputPath, class TInputImage, class TOutputPath>
const typename PathAndImageToPathFilter<TInputPath,TInputImage,TOutputPath>::InputPathType *
PathAndImageToPathFilter<TInputPath,TInputImage,TOutputPath>
::GetPathInput(void) 
{
  if (this->GetNumberOfInputs() < 1)
    {
    return 0;
    }
  
  return static_cast<const TInputPath * >
    (this->ProcessObject::GetInput(0) );
}
  
template <class TInputPath, class TInputImage, class TOutputPath>
void
PathAndImageToPathFilter<TInputPath,TInputImage,TOutputPath>
::SetImageInput(const InputImageType *image)
{
  // We have 2 inputs:  a path and an image
  if( 2 > this->GetNumberOfInputs() )
    {
    this->SetNumberOfRequiredInputs( 2 );
    }
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput(1, 
                                   const_cast< InputImageType * >( image ) );
}

template <class TInputPath, class TInputImage, class TOutputPath>
const typename PathAndImageToPathFilter<TInputPath,TInputImage,TOutputPath>::InputImageType *
PathAndImageToPathFilter<TInputPath,TInputImage,TOutputPath>
::GetImageInput(void) 
{
  if (this->GetNumberOfInputs() < 2)
    {
    return 0;
    }
  
  return static_cast<const TInputImage * >
    (this->ProcessObject::GetInput(1) );
}
  
/**
 *
 */
template <class TInputPath, class TInputImage, class TOutputPath>
void
PathAndImageToPathFilter<TInputPath,TInputImage,TOutputPath>
::GenerateInputRequestedRegion()
{
  // ProcessObject::GenerateInputRequestedRegion() will (for each input) call
  // Path::SetRequestedRegionToLargestPossibleRegion(), which is empty.
  Superclass::GenerateInputRequestedRegion();
}

/**
 *
 */
template <class TInputPath, class TInputImage, class TOutputPath>
void
PathAndImageToPathFilter<TInputPath,TInputImage,TOutputPath>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}


} // end namespace itk

#endif
