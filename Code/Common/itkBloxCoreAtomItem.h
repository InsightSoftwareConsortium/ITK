/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBloxCoreAtomItem.h
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
#ifndef __itkBloxCoreAtomItem_h
#define __itkBloxCoreAtomItem_h

#include "vnl/vnl_vector_fixed.h"
#include "itkBloxBoundaryPointItem.h"
#include "itkBloxItem.h"

namespace itk
{

/**
 * \class BloxCoreAtomItem
 * \brief A core atom object, stored in a BloxPixel
 *
 * A core atom is two boundary points whose gradients face each other
 * They store pointers to the two boundary points and a vnl_vector_fixed
 * representing the "center" of the core atom (i.e. the midpoint along the
 * vector between the two boundary points).
 * \ingroup ImageObject
 * */

template <unsigned int VImageDimension>
class BloxCoreAtomItem: public BloxItem
{

public:
  
  /**
   * The point type used to store the position of the CoreAtomItem
   * */
  typedef Point<double, VImageDimension> TPositionType;

  /**
   * The type of boundary point item we store pointers to
   * */
  typedef BloxBoundaryPointItem<VImageDimension> TBPItemType;

  /**
   * Set the position of the first boundary point in physical space
   * */
  void SetBoundaryPointA(TBPItemType* pointA){
    m_BoundaryPointA = pointA; }

  /**
   * Get the position of the first boundary point in physical space
   * */
  TBPItemType* GetBoundaryPointA(){
    return m_BoundaryPointA; }

  /**
   * Set the position of the second boundary point in physical space
   * */
  void SetBoundaryPointB(TBPItemType* pointB){
    m_BoundaryPointB = pointB; }

  /**
   * Get the position of the first boundary point in physical space
   * */
  TBPItemType* GetBoundaryPointB(){
    return m_BoundaryPointB; }

  /**
   * Set the position of the center of the core atom in physical space
   * */
  void SetCenterPosition(TPositionType pos){
    m_CenterPosition = pos; }

  /**
   * Get the position of the center of the core atom in physical space
   * */
  TPositionType GetCenterPosition(){
    return m_CenterPosition; }

  /**
   * Calculate the position of the center of the core atome in physical
   * space (assumes that the two boundary points are initialized)
   * */
  void CalcCenterPosition();
  
  BloxCoreAtomItem();
  ~BloxCoreAtomItem();

private:

  /**
   * The position of the center of the core atom
   */
  TPositionType m_CenterPosition;

  /**
   * The first boundary point that's part of the core atom
   */
  TBPItemType* m_BoundaryPointA;

  /**
   * The second boundary point that's part of the core atom
   */
  TBPItemType* m_BoundaryPointB;

};

} // end namespace itk


#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBloxCoreAtomItem.txx"
#endif

#endif
