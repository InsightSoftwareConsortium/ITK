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
#ifndef itkPathAndImageToPathFilter_hxx
#define itkPathAndImageToPathFilter_hxx

#include "itkPathAndImageToPathFilter.h"

namespace itk
{
/**
 *
 */
template< typename TInputPath, typename TInputImage, typename TOutputPath >
PathAndImageToPathFilter< TInputPath, TInputImage, TOutputPath >
::PathAndImageToPathFilter()
{
  // Modify superclass default values, can be overridden by subclasses
  this->SetNumberOfRequiredInputs(2);
}

/**
 *
 */
template< typename TInputPath, typename TInputImage, typename TOutputPath >
void
PathAndImageToPathFilter< TInputPath, TInputImage, TOutputPath >
::SetPathInput(const InputPathType *path)
{
  // We have 2 inputs:  a path and an image

  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput( 0,
                                    const_cast< InputPathType * >( path ) );
}

template< typename TInputPath, typename TInputImage, typename TOutputPath >
const typename PathAndImageToPathFilter< TInputPath, TInputImage, TOutputPath >::InputPathType *
PathAndImageToPathFilter< TInputPath, TInputImage, TOutputPath >
::GetPathInput(void)
{
  return itkDynamicCastInDebugMode< const TInputPath * >( this->GetPrimaryInput() );
}

template< typename TInputPath, typename TInputImage, typename TOutputPath >
void
PathAndImageToPathFilter< TInputPath, TInputImage, TOutputPath >
::SetImageInput(const InputImageType *image)
{
  // We have 2 inputs:  a path and an image

  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput( 1,
                                    const_cast< InputImageType * >( image ) );
}

template< typename TInputPath, typename TInputImage, typename TOutputPath >
const typename PathAndImageToPathFilter< TInputPath, TInputImage, TOutputPath >::InputImageType *
PathAndImageToPathFilter< TInputPath, TInputImage, TOutputPath >
::GetImageInput(void)
{
  return static_cast< const TInputImage * >( this->ProcessObject::GetInput(1) );
}

/**
 *
 */
template< typename TInputPath, typename TInputImage, typename TOutputPath >
void
PathAndImageToPathFilter< TInputPath, TInputImage, TOutputPath >
::GenerateInputRequestedRegion()
{
  // ProcessObject::GenerateInputRequestedRegion() will (for each input) call
  // Path::SetRequestedRegionToLargestPossibleRegion(), which is empty.
  Superclass::GenerateInputRequestedRegion();
}

/**
 *
 */
template< typename TInputPath, typename TInputImage, typename TOutputPath >
void
PathAndImageToPathFilter< TInputPath, TInputImage, TOutputPath >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}
} // end namespace itk

#endif
