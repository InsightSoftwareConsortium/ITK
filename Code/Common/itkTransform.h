/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTransform.h
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
#ifndef __itkTransform_h
#define __itkTransform_h

#include "itkObject.h"
#include "itkPoint.h"
#include "itkVector.h"
#include "itkCovariantVector.h"
#include "vnl/vnl_vector_fixed.h"


namespace itk
{
  
/** \class Transform
 * \brief Generic concept of transformation methods
 *
 * This Abstract Class define the generic interface for a transformation. 
 * It contains a Transform method.
 *
 * \ingroup Transforms
 *
 */

template <class TScalarType,unsigned int NDimensions>
class ITK_EXPORT  Transform 
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef Transform  Self;

  /**
   * Dimension of the domain space
   */
  enum { SpaceDimension     = NDimensions };

  /**
   * Standard vector type for this class
   */
  typedef Vector<TScalarType, SpaceDimension> VectorType;

  /**
   * Standard covariant vector type for this class
   */
  typedef CovariantVector<TScalarType, SpaceDimension> CovariantVectorType;

  /**
   * Standard vnl_vector type for this class
   */
  typedef vnl_vector_fixed<TScalarType, SpaceDimension> VnlVectorType;

  /**
   * Standard coordinate point type for this class
   */
  typedef Point<TScalarType, SpaceDimension> PointType;

  /**
   *  Method to transform a Point
   */
  virtual PointType     TransformPoint(const PointType  &point ) const
    { return point; }

  /**
   *  Method to transform a vector
   */
  virtual VectorType    TransformVector(const VectorType &vector) const
    { return vector; }

  /**
   *  Method to transform a vnl_vector
   */
  virtual VnlVectorType TransformVector(const VnlVectorType &vector) const
    { return vector; }

  /**
   *  Method to transform a CovariantVector
   */
  virtual CovariantVectorType TransformVector(
    const CovariantVectorType &vector) const
    { return vector; }

  
  Transform();
  virtual ~Transform() {};
  Transform(const Self&);
  const Self & operator=(const Self&);

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTransform.txx"
#endif

#endif



