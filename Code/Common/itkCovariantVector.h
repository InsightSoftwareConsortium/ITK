/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCovariantVector.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkCovariantVector_h
#define __itkCovariantVector_h


#include "itkFixedArray.h"
#include "vnl/vnl_vector_ref.h"
#include "itkIndent.h"
#include "itkVector.h"


namespace itk
{

/** \class CovariantVector
 * \brief A templated class holding a n-Dimensional covariant vector.
 * 
 * CovariantVector is a templated class that holds a single vector (i.e., an array
 * of values).  CovariantVector can be used as the data type held at each pixel in
 * an Image or at each vertex of an Mesh. The template parameter T can
 * be any data type that behaves like a primitive (or atomic) data type (int,
 * short, float, complex).  The NVectorDimension defines the number of
 * components in the vector array. 
 *
 * CovariantVector is not a dynamically extendible array like std::vector. It is
 * intended to be used like a mathematical vector.
 *
 * If you wish a simpler pixel types, you can use Scalar, which represents
 * a single data value at a pixel. There is also the more complex type
 * ScalarCovariantVector, which supports (for a given pixel) a single scalar value
 * plus an array of vector values. (The scalar and vectors can be of
 * different data type.)
 * 
 * CovariantVector is the type that should be used for representing normals
 * to surfaces and gradients of functions. AffineTransform transform
 * covariant vectors different than vectors.
 *
 * \ingroup Geometry
 * \ingroup DataRepresentation
 * 
 * \sa Image
 * \sa Mesh
 * \sa Point
 * \sa CovariantCovariantVector
 * \sa Matrix
 */

template<class T, unsigned int NVectorDimension=3>
class CovariantVector : public FixedArray<T,NVectorDimension> {
 public:
  /** Standard class typedefs. */
  typedef CovariantVector  Self;
  typedef FixedArray<T,NVectorDimension>  Superclass;
    
  /** ValueType can be used to declare a variable that is the same type
   * as a data element held in an CovariantVector.   */
  typedef T ValueType;

  /** Dimension of the Space */
  itkStaticConstMacro(VectorDimension, unsigned int,
                      NVectorDimension);

  /** I am a covariant vector. */
  typedef Self CovariantVectorType;
  
  /** The Array type from which this CovariantVector is derived. */
  typedef FixedArray<T, NVectorDimension>                BaseArray;
    
  /** Get the dimension (size) of the vector. */
  static unsigned int GetCovariantVectorDimension() 
    { return NVectorDimension; }  

  /** Set a vnl_vector_ref referencing the same memory block */
  void Set_vnl_vector( const vnl_vector<T> & );

  /** Get a vnl_vector_ref referencing the same memory block */
  vnl_vector_ref<T> Get_vnl_vector( void );

  /** Get a vnl_vector with a copy of the internal memory block */
  vnl_vector<T> Get_vnl_vector( void ) const;

  /** Default constructor has nothing to do. */
  CovariantVector() {}

  /** Pass-through constructor for the Array base class. */
  CovariantVector(const Self& r): BaseArray(r) {}
  CovariantVector(const ValueType r[VectorDimension]): BaseArray(r) {}  
    
  /** Pass-through assignment operator for the Array base class. */
  CovariantVector& operator= (const Self& r);
  CovariantVector& operator= (const ValueType r[VectorDimension]);
    
  /** Scalar operator*=.  Scales elements by a scalar. */
  const Self& operator*=(const ValueType &value);

  /** Scalar operator/=.  Scales (divides) elements by a scalar. */
  const Self& operator/=(const ValueType &value);

  /** CovariantVector operator+=.  Adds a vectors to the current vector. */
  const Self& operator+=(const Self &vec);

  /** CovariantVector operator-=.  Subtracts a vector from a current vector. */
  const Self& operator-=(const Self &vec);

  /** CovariantVector negation.  Negate all the elements of a vector. Return a new vector */
  Self operator-() const;
  
  /** CovariantVector addition. Add two vectors. Return a new vector. */
  Self operator+(const Self &vec) const;
  
  /** CovariantVector subtraction. Subtract two vectors. Return a new vector. */
  Self operator-(const Self &vec) const;
  
  /** Scalar operator*. Scale the elements of a vector by a scalar.
   * Return a new vector. */
  Self operator*(const ValueType& val) const;

  /** CovariantVector operator*.  Performs the inner product of two covariant vectors.
   * \warning This is equivalent to the scalar product only if the reference
   * system has orthogonal axis and equal scales.  */
  ValueType operator*(const Self &vec) const;

  /** operator*.  Performs the scalar product with a vector (contravariant).
   * This scalar product is invariant under affine transformations */
  ValueType operator*(const Vector<T,NVectorDimension> &vec) const;

  /** Scalar operator/. Scale (divide) the elements of a vector by a scalar.
   * Return a new vector. */
  Self operator/(const ValueType& val) const;

  /** Returns the Euclidean Norm of the vector  */
  ValueType GetNorm( void ) const;

  /** Returns vector's Squared Euclidean Norm  */
  ValueType GetSquaredNorm( void ) const;

  /** Print content */
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Copy from another CovariantVector with a different representation type. 
   *  Casting is done with C-Like rules  */
  template < typename TCoordRepB >
  void CastFrom( const CovariantVector<TCoordRepB,NVectorDimension> & pa )
  {
    for(unsigned int i=0; i<NVectorDimension; i++ )
      {
      (*this)[i] = static_cast<T>( pa[i] );
      }
  }


};


} // end namespace itk
  

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkCovariantVector.txx"
#endif


#endif 
