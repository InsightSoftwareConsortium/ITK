/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBloxCoreAtomItem.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBloxCoreAtomItem_txx
#define __itkBloxCoreAtomItem_txx

#include "itkBloxCoreAtomItem.h"

namespace itk
{

template <unsigned int VImageDimension>
BloxCoreAtomItem<VImageDimension>
::BloxCoreAtomItem()
{

}

template <unsigned int VImageDimension>
BloxCoreAtomItem<VImageDimension>
::~BloxCoreAtomItem()
{

}

template <unsigned int VImageDimension>
void
BloxCoreAtomItem<VImageDimension>
::CalcCenterAndDiameter()
{
  // Get boundary points
  PositionType P1 = m_BoundaryPointA->GetPosition();
  PositionType P2 = m_BoundaryPointB->GetPosition();

  // Calculate the center of the core atom  
  m_CenterPosition = P1 + (P2 - P1) / 2;

  // Calculate the diameter of the core atom
  m_Diameter = (P1-P2).GetNorm();
  
}

} // end namespace itk

#endif
