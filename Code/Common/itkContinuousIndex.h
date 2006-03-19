/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkContinuousIndex.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkContinuousIndex_h
#define __itkContinuousIndex_h

#include "itkPoint.h"
#include "itkIndex.h"

namespace itk
{

/** \class ContinuousIndex
 * \brief A templated class holding a point in n-Dimensional image space.
 * 
 * ContinuousIndex is a templated class that holds a set of coordinates 
 * (components).
 * The template parameter TCoordRep can be any data type that behaves 
 * like a primitive (or atomic) data type (int, short, float, complex).  
 * The VIndexDimension defines the number of  components in the continous 
 * index array. 
 * 
 * \sa Point
 * \sa Index
 *
 * \ingroup ImageAccess
 * \ingroup ImageObjects
 *
 */
template<class TCoordRep = double, unsigned int VIndexDimension=2>
class ContinuousIndex : public Point< TCoordRep, VIndexDimension >
{
public:
  /** Standard class typedefs. */
  typedef ContinuousIndex                   Self;
  typedef Point<TCoordRep,VIndexDimension>  Superclass;
  
  /** ValueType can be used to declare a variable that is the same type
   * as a data element held in an Point.   */
  typedef TCoordRep ValueType;
  typedef TCoordRep CoordRepType;
  
  /** Dimension of the Space */
  itkStaticConstMacro(IndexDimension, unsigned int, VIndexDimension);

  /** Corresponding discrete index type */
  typedef Index<VIndexDimension> IndexType;

  /** The Array type from which this Vector is derived. */
  typedef typename Superclass::BaseArray            BaseArray;
  typedef typename BaseArray::Iterator              Iterator;
  typedef typename BaseArray::ConstIterator         ConstIterator;
  
  /** Default constructor has nothing to do. */
  ContinuousIndex() {}

  /** Pass-through constructor to the Point base class. */
  ContinuousIndex(const Self& r): Superclass(r) {}
  ContinuousIndex(const ValueType r[IndexDimension]): Superclass(r) {}  
  
  /** Construct from discrete index type */
  ContinuousIndex(const IndexType& index )
    {
    for( unsigned int i = 0; i < VIndexDimension; i++ )
      {
      (*this)[i] = TCoordRep( index[i] );
      }
    }
};

} // namespace itk

#endif 
