/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBloxBoundaryPointItem.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
 * \brief A boundary point, stored in a BloxPixel
 *
 * */

template <unsigned int VImageDimension>
class BloxBoundaryPointItem: public BloxItem
{

public:
  
  /**
   * The type of vector used to store the position of the BoundaryPointItem
   * */
  typedef Point<double, VImageDimension> TPositionType;
  
  /**
   * The type of vector used to store the gradient of the BoundaryPointItem
   * */
  typedef CovariantVector<double, VImageDimension> TGradientType;

  /**
   * Set the position of the boundary point in physical space
   * */
  void SetPhysicalPosition(TPositionType physPos){m_PhysicalPosition = physPos;};

  /**
   * Get the position of the boundary point in physical space
   * */
  TPositionType GetPhysicalPosition(){return m_PhysicalPosition;};

  /**
   * Set the gradient of the boundary point
   * */
  void SetGradient(TGradientType grad){m_Gradient = grad;};

  /**
   * Get the gradient of the boundary point
   * */
  TGradientType GetGradient(){return m_Gradient;};
  
  BloxBoundaryPointItem();
  ~BloxBoundaryPointItem();

private:

  /**
   * The position of the boundary point in the coordinate system of the
   * physical image in which the boundary pixel was located
   * */
  TPositionType m_PhysicalPosition;

  /**
   * The gradient of the boundary point (non-normalized)
   * */
  TGradientType m_Gradient;

};

} // end namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBloxBoundaryPointItem.txx"
#endif

#endif
