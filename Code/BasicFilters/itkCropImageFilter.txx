/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCropImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkCropImageFilter_txx
#define _itkCropImageFilter_txx

#include "itkCropImageFilter.h"

namespace itk
{

template <class TInputImage, class TOutputImage>
void
CropImageFilter<TInputImage, TOutputImage>
::GenerateOutputInformation()
{
  const TInputImage * inputPtr = this->GetInput();
  if( !inputPtr )
    {
    return;
    }
  
  // Compute the new region size.
  OutputImageRegionType croppedRegion;
  SizeType   sz;
  OutputImageIndexType   idx;

  InputImageSizeType input_sz =
    inputPtr->GetLargestPossibleRegion().GetSize();
  InputImageIndexType input_idx =
    inputPtr->GetLargestPossibleRegion().GetIndex();
  
  idx = input_idx + m_LowerBoundaryCropSize; 
  sz  = input_sz  - (m_UpperBoundaryCropSize + m_LowerBoundaryCropSize); 

  croppedRegion.SetSize(sz);
  croppedRegion.SetIndex(idx);

  // Set extraction region in the superclass.
  this->SetExtractionRegion(croppedRegion);

  // 
  Superclass::GenerateOutputInformation();
}
  
/**
 *
 */
template <class TInputImage, class TOutputImage>
void 
CropImageFilter<TInputImage,TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "UpperBoundaryCropSize: " << m_UpperBoundaryCropSize <<
    std::endl;
  os << indent << "LowerBoundaryCropSize: " << m_LowerBoundaryCropSize <<
    std::endl;
}

 

  
} // end namespace itk

#endif
