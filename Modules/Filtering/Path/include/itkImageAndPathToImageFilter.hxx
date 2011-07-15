/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef __itkImageAndPathToImageFilter_hxx
#define __itkImageAndPathToImageFilter_hxx

#include "itkImageAndPathToImageFilter.h"

namespace itk
{
/**
 *
 */
template< class TInputImage, class TInputPath, class TOutputImage >
ImageAndPathToImageFilter< TInputImage, TInputPath, TOutputImage >
::ImageAndPathToImageFilter()
{
  // Modify superclass default values, can be overridden by subclasses
  this->SetNumberOfRequiredInputs(2);
}

/**
 *
 */
template< class TInputImage, class TInputPath, class TOutputImage >
void
ImageAndPathToImageFilter< TInputImage, TInputPath, TOutputImage >
::SetImageInput(const InputImageType *image)
{
  // We have 2 inputs:  a path and an image

  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput( 0,
                                    const_cast< InputImageType * >( image ) );
}

template< class TInputImage, class TInputPath, class TOutputImage >
const typename ImageAndPathToImageFilter< TInputImage, TInputPath, TOutputImage >::InputImageType *
ImageAndPathToImageFilter< TInputImage, TInputPath, TOutputImage >
::GetImageInput(void)
{
  if ( this->GetNumberOfInputs() < 1 )
    {
    return 0;
    }

  return static_cast< const TInputImage * >
         ( this->ProcessObject::GetInput(0) );
}

template< class TInputImage, class TInputPath, class TOutputImage >
void
ImageAndPathToImageFilter< TInputImage, TInputPath, TOutputImage >
::SetPathInput(const InputPathType *path)
{
  // We have 2 inputs:  a path and an image

  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput( 1,
                                    const_cast< InputPathType * >( path ) );
}

template< class TInputImage, class TInputPath, class TOutputImage >
const typename ImageAndPathToImageFilter< TInputImage, TInputPath, TOutputImage >::InputPathType *
ImageAndPathToImageFilter< TInputImage, TInputPath, TOutputImage >
::GetPathInput(void)
{
  if ( this->GetNumberOfInputs() < 2 )
    {
    return 0;
    }

  return static_cast< const TInputPath * >
         ( this->ProcessObject::GetInput(1) );
}

/**
 *
 */
template< class TInputImage, class TInputPath, class TOutputImage >
void
ImageAndPathToImageFilter< TInputImage, TInputPath, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}
} // end namespace itk

#endif
