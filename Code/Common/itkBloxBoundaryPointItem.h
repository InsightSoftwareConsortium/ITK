/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBloxBoundaryPointItem.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBloxBoundaryPointItem_h
#define __itkBloxBoundaryPointItem_h

#include "itkCovariantVector.h"
#include "itkPoint.h"
#include "itkBloxItem.h"

namespace itk
{

/**
 * \class BloxBoundaryPointItem
 * \brief A boundary point, stored in a BloxPixel.
 * \ingroup ImageObjects
 *
 * */

template <unsigned int VImageDimension>
class ITK_EXPORT BloxBoundaryPointItem: public BloxItem
{
public:
  /** The type of vector used to store the position of the BoundaryPointItem * */
  typedef Point<double, VImageDimension> PositionType;
  
  /** The type of vector used to store the gradient of the BoundaryPointItem * */
  typedef CovariantVector<double, VImageDimension> GradientType;

  /** Set the position of the boundary point in physical space * */
  void SetPhysicalPosition(PositionType physPos){m_PhysicalPosition = physPos;};

  /** Get the position of the boundary point in physical space * */
  PositionType GetPhysicalPosition(){return m_PhysicalPosition;};

  /** Set the gradient of the boundary point * */
  void SetGradient(GradientType grad){m_Gradient = grad;};

  /** Get the gradient of the boundary point * */
  GradientType GetGradient(){return m_Gradient;};
  
  BloxBoundaryPointItem();
  ~BloxBoundaryPointItem();

private:

  /** The position of the boundary point in the coordinate system of the
   * physical image in which the boundary pixel was located * */
  PositionType m_PhysicalPosition;

  /** The gradient of the boundary point (non-normalized) * */
  GradientType m_Gradient;

};

} // end namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBloxBoundaryPointItem.txx"
#endif

#endif
