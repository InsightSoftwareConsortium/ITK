/*==========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkContinuousIndex.h
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
#ifndef __itkContinuousIndex_h
#define __itkContinuousIndex_h

#include "itkPoint.h"
#include "itkIndex.h"

namespace itk
{

/** \class ContinuousIndex
 * \brief A templated class holding a point in n-Dimensional image space.
 * 
 * ContinuousIndex is a templated class that holds a set of coordinates (components).
 * The template parameter TCoordRep can
 * be any data type that behaves like a primitive (or atomic) data type (int,
 * short, float, complex).  The VIndexDimension defines the number of
 * components in the continous index array. 
 *
 * 
 * \sa Point
 * \sa Index
 *
 */

template<class TCoordRep = double, unsigned int VIndexDimension=2>
class ContinuousIndex : public Point< TCoordRep, VIndexDimension >
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef ContinuousIndex  Self;
  
  /**
   * Standard "Superclass" typedef.
   */
  typedef Point<TCoordRep,VIndexDimension>  Superclass;


  /**
   * ValueType can be used to declare a variable that is the same type
   * as a data element held in an Point.  
   */
  typedef TCoordRep ValueType;
  typedef TCoordRep CoordRepType;

  /**
   * Dimension of the Space
   */
  enum { IndexDimension = VIndexDimension };

  /**
   * Corresponding discrete index type
   */
  typedef Index<VIndexDimension> IndexType;

  /**
   * The Array type from which this Vector is derived.
   */
  typedef typename Superclass::BaseArray            BaseArray;
  typedef typename BaseArray::ArrayCommaListCopier  ArrayCommaListCopier;
  typedef typename BaseArray::Iterator              Iterator;
  typedef typename BaseArray::ConstIterator         ConstIterator;

  /**
   * Default constructor has nothing to do.
   */
  ContinuousIndex() {}

  /*@{
   * Pass-through constructor to the Point base class.
   */
  ContinuousIndex(const Self& r): Superclass(r) {}
  ContinuousIndex(const Superclass& r) : Superclass(r) {}
  ContinuousIndex(const typename BaseArray::Reference& r): Superclass(r) {}
  ContinuousIndex(const typename BaseArray::ConstReference& r): Superclass(r) {}
  ContinuousIndex(const ValueType r[IndexDimension]): Superclass(r) {}  
  //@}

  /**
   * Construct from discrete index type
   */
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
