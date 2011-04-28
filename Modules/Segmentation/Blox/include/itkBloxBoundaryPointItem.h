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
 *
 * \ingroup ITK-Blox
 */

template< unsigned int VImageDimension >
class ITK_EXPORT BloxBoundaryPointItem:public BloxItem
{
public:
  /** The type of vector used to store the position of the
   *  BoundaryPointItem */
  typedef Point< double, VImageDimension > PositionType;

  /** The type of vector used to store the gradient of the
   *  BoundaryPointItem */
  typedef CovariantVector< double, VImageDimension > GradientType;

  /** Set the position of the boundary point in physical space */
  void SetPhysicalPosition(PositionType physPos)
  {
    m_PhysicalPosition = physPos;
  }

  /** Get the position of the boundary point in physical space */
  PositionType GetPhysicalPosition()
  {
    return m_PhysicalPosition;
  }

  /** Set the gradient of the boundary point */
  void SetGradient(GradientType grad)
  {
    m_Gradient = grad;
  }

  /** Get the gradient of the boundary point */
  GradientType GetGradient()
  {
    return m_Gradient;
  }

  BloxBoundaryPointItem();
  ~BloxBoundaryPointItem();
private:

  /** The position of the boundary point in the coordinate system of the
   * physical image in which the boundary pixel was located */
  PositionType m_PhysicalPosition;

  /** The gradient of the boundary point (non-normalized) */
  GradientType m_Gradient;
};
} // end namespace itk

// Define instantiation macro for this template.
#define ITK_TEMPLATE_BloxBoundaryPointItem(_, EXPORT, TypeX, TypeY)     \
  namespace itk                                                         \
  {                                                                     \
  _( 1 ( class EXPORT BloxBoundaryPointItem< ITK_TEMPLATE_1 TypeX > ) ) \
  namespace Templates                                                   \
  {                                                                     \
  typedef BloxBoundaryPointItem< ITK_TEMPLATE_1 TypeX >                 \
  BloxBoundaryPointItem##TypeY;                                       \
  }                                                                     \
  }

#if ITK_TEMPLATE_EXPLICIT
#include "Templates/itkBloxBoundaryPointItem+-.h"
#endif

#if ITK_TEMPLATE_TXX
#include "itkBloxBoundaryPointItem.txx"
#endif

#endif
