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
#ifndef itkImageAndPathToImageFilter_hxx
#define itkImageAndPathToImageFilter_hxx

#include "itkImageAndPathToImageFilter.h"

namespace itk
{
/**
 *
 */
template< typename TInputImage, typename TInputPath, typename TOutputImage >
ImageAndPathToImageFilter< TInputImage, TInputPath, TOutputImage >
::ImageAndPathToImageFilter()
{
  // Modify superclass default values, can be overridden by subclasses
  this->SetNumberOfRequiredInputs(2);
}

/**
 *
 */
template< typename TInputImage, typename TInputPath, typename TOutputImage >
void
ImageAndPathToImageFilter< TInputImage, TInputPath, TOutputImage >
::SetImageInput(const InputImageType *image)
{
  // We have 2 inputs:  a path and an image

  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput( 0,
                                    const_cast< InputImageType * >( image ) );
}

template< typename TInputImage, typename TInputPath, typename TOutputImage >
const typename ImageAndPathToImageFilter< TInputImage, TInputPath, TOutputImage >::InputImageType *
ImageAndPathToImageFilter< TInputImage, TInputPath, TOutputImage >
::GetImageInput(void)
{
  return this->GetNonConstImageInput();
}

template< typename TInputImage, typename TInputPath, typename TOutputImage >
typename ImageAndPathToImageFilter< TInputImage, TInputPath, TOutputImage >::InputImageType *
ImageAndPathToImageFilter< TInputImage, TInputPath, TOutputImage >
::GetNonConstImageInput(void)
{
  TInputImage * temp_return=dynamic_cast< TInputImage * >( this->ProcessObject::GetInput(0) );
  if(temp_return == ITK_NULLPTR)
    {
    itkExceptionMacro("Invalid type conversion in GetNonConstImageInput()")
    }
  return temp_return;
}

template< typename TInputImage, typename TInputPath, typename TOutputImage >
void
ImageAndPathToImageFilter< TInputImage, TInputPath, TOutputImage >
::SetPathInput(const InputPathType *path)
{
  // We have 2 inputs:  a path and an image

  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput( 1, const_cast< InputPathType * >( path ) );
}

template< typename TInputImage, typename TInputPath, typename TOutputImage >
const typename ImageAndPathToImageFilter< TInputImage, TInputPath, TOutputImage >::InputPathType *
ImageAndPathToImageFilter< TInputImage, TInputPath, TOutputImage >
::GetPathInput(void)
{
  return this->GetNonConstPathInput();
}

template< typename TInputImage, typename TInputPath, typename TOutputImage >
typename ImageAndPathToImageFilter< TInputImage, TInputPath, TOutputImage >::InputPathType *
ImageAndPathToImageFilter< TInputImage, TInputPath, TOutputImage >
::GetNonConstPathInput(void)
{
  TInputPath * temp_return = dynamic_cast< TInputPath * >( this->ProcessObject::GetInput(1) );
  if(temp_return == ITK_NULLPTR)
    {
    itkExceptionMacro("Invalid type conversion in GetNonConstPathInput()")
    }
  return temp_return;

}

/**
 *
 */
template< typename TInputImage, typename TInputPath, typename TOutputImage >
void
ImageAndPathToImageFilter< TInputImage, TInputPath, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}
} // end namespace itk

#endif
