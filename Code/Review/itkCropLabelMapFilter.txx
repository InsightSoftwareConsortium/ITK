/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCropLabelMapFilter.txx
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
#ifndef __itkCropLabelMapFilter_txx
#define __itkCropLabelMapFilter_txx

#include "itkCropLabelMapFilter.h"

namespace itk
{
template< class TInputImage >
void
CropLabelMapFilter< TInputImage >
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

  SizeType originalCropSize = m_UpperBoundaryCropSize + m_LowerBoundaryCropSize;

  index = inputIndex + m_LowerBoundaryCropSize;
  size  = inputSize  - ( originalCropSize );

  croppedRegion.SetSize(size);
  croppedRegion.SetIndex(index);

  // Set extraction region in the superclass.
  this->SetRegion(croppedRegion);

  Superclass::GenerateOutputInformation();
}

template< class TInputImage >
void
CropLabelMapFilter< TInputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "UpperBoundaryCropSize: " << m_UpperBoundaryCropSize << std::endl;
  os << indent << "LowerBoundaryCropSize: " << m_LowerBoundaryCropSize << std::endl;
}
} // end namespace itk

#endif
