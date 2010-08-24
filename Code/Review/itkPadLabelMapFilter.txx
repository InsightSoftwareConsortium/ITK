/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPadLabelMapFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkPadLabelMapFilter_txx
#define __itkPadLabelMapFilter_txx
#include "itkPadLabelMapFilter.h"

namespace itk
{
template< class TInputImage >
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

template< class TInputImage >
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
