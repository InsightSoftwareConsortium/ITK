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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkPadLabelMapFilter_hxx
#define itkPadLabelMapFilter_hxx
#include "itkPadLabelMapFilter.h"

namespace itk
{
template< typename TInputImage >
void
PadLabelMapFilter< TInputImage >
::GenerateOutputInformation()
{
  const TInputImage *inputPtr = this->GetInput();

  if ( !inputPtr )
    {
    return;
    }

  // Compute the new region size.
  RegionType croppedRegion;
  SizeType   size;
  IndexType  index;

  SizeType  inputSize = inputPtr->GetLargestPossibleRegion().GetSize();
  IndexType inputIndex = inputPtr->GetLargestPossibleRegion().GetIndex();

  SizeType originalPadSize = m_UpperBoundaryPadSize + m_LowerBoundaryPadSize;

  index = inputIndex - m_LowerBoundaryPadSize;
  size = inputSize + ( originalPadSize );

  croppedRegion.SetSize(size);
  croppedRegion.SetIndex(index);

  // Set extraction region in the superclass.
  this->SetRegion(croppedRegion);

  Superclass::GenerateOutputInformation();
}

template< typename TInputImage >
void
PadLabelMapFilter< TInputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "UpperBoundaryPadSize: " << m_UpperBoundaryPadSize << std::endl;
  os << indent << "LowerBoundaryPadSize: " << m_LowerBoundaryPadSize << std::endl;
}
} // end namespace itk

#endif
