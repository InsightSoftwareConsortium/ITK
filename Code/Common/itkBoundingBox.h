/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBoundingBox.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBoundingBox_h
#define __itkBoundingBox_h

#include "itkObject.h"
#include "itkPoint.h"
#include "itkNumericTraits.h"
#include "itkVectorContainer.h"
#include "itkFixedArray.h"


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
 * 
 * \ingroup DataRepresentation 
 * \ingroup ImageObjects 
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
  /** Standard class typedefs. */
  typedef BoundingBox         Self;
  typedef Object  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
    
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Hold on to the type information specified by the template parameters. */
  typedef TPointIdentifier PointIdentifier;
  typedef TCoordRep CoordRepType;
  typedef TPointsContainer PointsContainer;
  typedef typename PointsContainer::Pointer PointsContainerPointer;
  typedef typename PointsContainer::ConstPointer PointsContainerConstPointer;
  typedef Point< CoordRepType, VPointDimension >  PointType;
  typedef FixedArray< CoordRepType, VPointDimension*2 >  BoundsArrayType;
  
  /** Hold on to the dimensions specified by the template parameters. */
  itkStaticConstMacro(PointDimension, unsigned int,  VPointDimension);

  /** Convenient typedefs.*/
  typedef typename
          PointsContainer::ConstIterator        PointsContainerConstIterator;
  typedef typename
          PointsContainer::Iterator             PointsContainerIterator;
    
  /** Set/Get the points from which the bounding box should be computed. The 
   * bounding box is cached and is not recomputed if the points are not 
   * changed. */
  void SetPoints(const PointsContainer *);
  const PointsContainer * GetPoints(void) const;
  
  /** Method that actually computes bounding box. */
  bool ComputeBoundingBox(void);

  /** Get the bounding box. NULL is returned if the bounding box cannot be
   * computed. (This may happen if the user never specifies something to
   * compute the bounding box from.) */
  itkGetConstMacro( Bounds, BoundsArrayType );

  /** Get the center of the bounding box. Returns NULL if bounding box
   * cannot be computed. */
  PointType GetCenter(void);

  /** Get the minimum point of the bounding box. Returns NULL if bounding box
   * cannot be computed. */
  PointType GetMinimum(void);

  /** Set the minimum point of the bounding box. May not be valid for the given
   * set of points.   Will be preserved until this filter's (i.e., the point
   * set's) modified time changes. */
  void      SetMinimum(const PointType & );

  /** Get the maximum point of the bounding box. Returns NULL if bounding box
   * cannot be computed. */
  PointType GetMaximum(void);

  /** Set the maximum point of the bounding box. May not be valid for the given
   * set of points.   Will be preserved until this filter's (i.e., the point
   * set's) modified time changes. */
  void      SetMaximum(const PointType & );

  /** Adjust bounds (if necessary) as if the given point was in the set
   * of points being considered.   Does not add the given point to the set.
   * Therefore, this point not considered in future computeboundingbox/gets
   * once the point set is changed. */
  void ConsiderPoint( const PointType & );

  /** Get the length squared of the diagonal of the bounding box.
   * Returns zero if bounding box cannot be computed. Note that the
   * Accumulate type is used to represent the length. */
  typedef typename NumericTraits<CoordRepType>::AccumulateType AccumulateType;
  AccumulateType GetDiagonalLength2(void);
  
  /** Method that checks if a point is inside the bounding box. */
  bool IsInside( const PointType & );

  /** Method Compute the Modified Time based on changed to the components. */
  unsigned long GetMTime( void ) const;

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
  void PrintSelf(std::ostream& os, Indent indent) const;

  typedef typename PointsContainer::ConstIterator  ConstIterator; 

private:
  BoundingBox(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  PointsContainerConstPointer m_PointsContainer;
  BoundsArrayType m_Bounds;
  TimeStamp m_BoundsMTime; //The last time the bounds were computed.

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBoundingBox.txx"
#endif
  
#endif
