/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBoundingBox.h
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
#ifndef __itkBoundingBox_h
#define __itkBoundingBox_h

#include "itkObject.h"
#include "itkPoint.h"
#include "itkNumericTraits.h"
#include "itkVectorContainer.h"

namespace itk
{

/** \class BoundingBox
 * \brief Represent and compute information about bounding boxes.
 *
 * BoundingBox is a supporting class that represents, computes, and
 * caches information about bounding boxes. The bounding box can
 * be computed from several sources, including manual specification
 * and computation from an input points container.
 *
 * This is a templated, n-dimensional version of the bounding box.
 * Bounding boxes are represented by n pairs of (min,max) pairs,
 * where min is the minimum coordinate value and max is the
 * maximum coordinate value for coordinate axis i.
 *
 * Template parameters for BoundingBox:
 *
 * TPointIdentifier =
 *     The type used to access a particular point (i.e., a point's id)
 *
 * TCoordRep =
 *     Numerical type with which to represent each coordinate value.
 *
 * VPointDimension =
 *    Geometric dimension of space.
 */
  
template <
  typename TPointIdentifier = unsigned long,
  int VPointDimension = 3,
  typename TCoordRep = float,
  typename TPointsContainer = 
    VectorContainer< TPointIdentifier,Point<TCoordRep, VPointDimension> >
  >
class ITK_EXPORT BoundingBox : public Object
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef BoundingBox         Self;
  
  /**
   * Standard "Superclass" typedef.
   */
  typedef Object  Superclass;

  /**
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);

  //@{
  /**
   * Hold on to the type information specified by the template parameters.
   */
  typedef TPointIdentifier PointIdentifier;
  typedef TCoordRep CoordRepType;
  enum { PointDimension = VPointDimension };
  typedef TPointsContainer PointsContainer;
  typedef typename PointsContainer::Pointer PointsContainerPointer;
  typedef Point< CoordRepType, VPointDimension >  PointType;
  //@}

  //@{
  /**
   * Convenient typedefs.
   */
  typedef typename
          PointsContainer::ConstIterator        PointsContainerConstIterator;
  typedef typename
          PointsContainer::Iterator             PointsContainerIterator;
  //@}
  
  /**
   * Set the points from which the bounding box should be computed. The 
   * bounding box is cached and is not recomputed if the points are not 
   * changed.
   */
  void SetPoints(PointsContainer *);
  PointsContainerPointer GetPoints(void);

  /**
   * Method that actually computes bounding box.
   */
  bool ComputeBoundingBox(void);

  /**
   * Get the bounding box. NULL is returned if the bounding box cannot be
   * computed. (This may happen if the user never specifies something to
   * compute the bounding box from.)
   */
  CoordRepType* GetBoundingBox(CoordRepType bounds[PointDimension*2]);

  /**
   * Get the center of the bounding box. Returns NULL if bounding box
   * cannot be computed.
   */
  CoordRepType* GetCenter(CoordRepType bounds[PointDimension]);

  //@{
  /**
   * Get the length squared of the diagonal of the bounding box.
   * Returns zero if bounding box cannot be computed. Note that the
   * Accumulate type is used to represent the length.
   */
  typedef typename NumericTraits<CoordRepType>::AccumulateType AccumulateType;
  AccumulateType GetDiagonalLength2(void);
  //@}

#if 0
  /**
   * Intersect this bounding box (bounds[PointDimension*2]) with a line
   * given by an origin (origin[PointDimension]) and direction
   * (direction[PointDimension]). Get the following results if the
   * corresponding pointers are not NULL:
   *
   *  - The intersection point's geometric coordinates (returned through
   *     pointer to array: coords[PointDimension]).
   *
   *  - The line's parametric coordinate of the intersection point
   *     (returned through "t" pointer).
   *
   * Returns whether an intersection exists.
   */
  bool IntersectWithLine(CoordRepType origin[PointDimension],
			 CoordRepType direction[PointDimension],
			 CoordRepType coords[PointDimension],
			 CoordRepType* t);
  
#endif

protected:
  BoundingBox(); 
  virtual ~BoundingBox(); 
  BoundingBox(const Self&) {}
  void operator=(const Self&) {}

  void PrintSelf(std::ostream& os, Indent indent);

  typedef typename PointsContainer::ConstIterator  ConstIterator; 

private:
  PointsContainerPointer m_PointsContainer;
  CoordRepType *m_Bounds;
  TimeStamp m_BoundsMTime; //The last time the bounds were computed.

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBoundingBox.txx"
#endif
  
#endif
