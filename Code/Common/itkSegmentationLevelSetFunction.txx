/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSegmentationLevelSetFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSegmentationLevelSetFunction_txx_
#define __itkSegmentationLevelSetFunction_txx_

#include "itkSegmentationLevelSetFunction.h"

namespace itk {

template <class TImageType, class TFeatureImageType>
void SegmentationLevelSetFunction<TImageType, TFeatureImageType>
::Initialize(const RadiusType &r)
{
  Superclass::Initialize(r);  
}

template <class TImageType, class TFeatureImageType>
void SegmentationLevelSetFunction<TImageType, TFeatureImageType>
::AllocateSpeedImage()
{
  m_SpeedImage->SetRequestedRegion(m_FeatureImage->GetRequestedRegion());
  m_SpeedImage->SetBufferedRegion(m_FeatureImage->GetBufferedRegion());
  m_SpeedImage->SetLargestPossibleRegion(m_FeatureImage->GetLargestPossibleRegion());
  m_SpeedImage->Allocate();
}

} // end namespace itk


#endif
