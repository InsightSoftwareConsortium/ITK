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
#ifndef itkPathToPathFilter_hxx
#define itkPathToPathFilter_hxx

#include "itkPathToPathFilter.h"

namespace itk
{
/**
 *
 */
template< typename TInputPath, typename TOutputPath >
PathToPathFilter< TInputPath, TOutputPath >
::PathToPathFilter()
{
  // Let the superclass do everything
}

/**
 *
 */
template< typename TInputPath, typename TOutputPath >
void
PathToPathFilter< TInputPath, TOutputPath >
::SetInput(const InputPathType *path)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput( 0,
                                    const_cast< InputPathType * >( path ) );
}

/**
 * Connect one of the operands for pixel-wise addition
 */
template< typename TInputPath, typename TOutputPath >
void
PathToPathFilter< TInputPath, TOutputPath >
::SetInput(unsigned int index, const TInputPath *path)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput( index,
                                    const_cast< TInputPath * >( path ) );
}

/**
 *
 */
template< typename TInputPath, typename TOutputPath >
const typename PathToPathFilter< TInputPath, TOutputPath >::InputPathType *
PathToPathFilter< TInputPath, TOutputPath >
::GetInput(void)
{
  return itkDynamicCastInDebugMode< const TInputPath * >( this->GetPrimaryInput() );
}

/**
 *
 */
template< typename TInputPath, typename TOutputPath >
const typename PathToPathFilter< TInputPath, TOutputPath >::InputPathType *
PathToPathFilter< TInputPath, TOutputPath >
::GetInput(unsigned int idx)
{
  return itkDynamicCastInDebugMode< const TInputPath * > ( this->ProcessObject::GetInput(idx) );
}

/**
 *
 */
template< typename TInputPath, typename TOutputPath >
void
PathToPathFilter< TInputPath, TOutputPath >
::GenerateInputRequestedRegion()
{
  // ProcessObject::GenerateInputRequestedRegion() will (for each input) call
  // Path::SetRequestedRegionToLargestPossibleRegion(), which is empty.
  Superclass::GenerateInputRequestedRegion();
}

/**
 *
 */
template< typename TInputPath, typename TOutputPath >
void
PathToPathFilter< TInputPath, TOutputPath >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}
} // end namespace itk

#endif
