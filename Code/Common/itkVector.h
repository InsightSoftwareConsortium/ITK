/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVector.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVector_h
#define __itkVector_h

#include "itkFixedArray.h"
#include "vnl/vnl_vector_ref.h"
#include "itkIndent.h"

namespace itk
{

/** \class Vector
 * \brief A templated class holding a n-Dimensional vector.
 * 
 * Vector is a templated class that holds a single vector (i.e., an array
 * of values).  Vector can be used as the data type held at each pixel in
 * an Image or at each vertex of an Mesh. The template parameter T can
 * be any data type that behaves like a primitive (or atomic) data type (int,
 * short, float, complex).  The NVectorDimension defines the number of
 * components in the vector array. 
 *
 * Vector is not a dynamically extendible array like std::vector. It is
 * intended to be used like a mathematical vector.
 *
 * If you wish a simpler pixel types, you can use Scalar, which represents
 * a single data value at a pixel. There is also the more complex type
 * ScalarVector, which supports (for a given pixel) a single scalar value
 * plus an array of vector values. (The scalar and vectors can be of
 * different data type.)
 * 
 * \ingroup Geometry
 * \ingroup DataRepresentation
 * 
 * \sa Image
 * \sa Mesh
 * \sa Point
 * \sa CovariantVector
 * \sa Matrix
 */
template<class T, unsigned int NVectorDimension=3>
class Vector : public FixedArray<T,NVectorDimension>
{
public:
  /** Standard class typedefs. */
  typedef Vector  Self;
  typedef FixedArray<T,NVectorDimension>  Superclass;
  
  /** ValueType can be used to declare a variable that is the same type
   * as a data element held in an Vector.   */
  typedef T ValueType;

  /** Dimension of the vector space. */
  itkStaticConstMacro(Dimension, unsigned int, NVectorDimension);

  /** I am a vector type. */
  typedef Self VectorType;
  
  /** The Array type from which this vector is derived. */
  typedef FixedArray<T, NVectorDimension>                BaseArray;
    
  /** Get the dimension (size) of the vector. */
  static unsigned int GetVectorDimension() 
    { return NVectorDimension; }  

  /** Set a vnl_vector_ref referencing the same memory block. */
  void Set_vnl_vector( const vnl_vector<T> & );

  /** Get a vnl_vector_ref referencing the same memory block. */
  vnl_vector_ref<T> Get_vnl_vector( void );

  /** Get a vnl_vector with a copy of the internal memory block. */
  vnl_vector<T> Get_vnl_vector( void ) const;

  /** Default constructor and copy constructors. */
  Vector(): BaseArray() { }
  Vector(const ValueType& r);
  
  /** Pass-through constructor for the Array base class. */
  Vector(const Self& r): BaseArray(r) {}
  Vector(const ValueType r[Dimension]): BaseArray(r) {}  
    
  /** Pass-through assignment operator for the Array base class. */
  Vector& operator= (const Self& r);
  Vector& operator= (const ValueType r[Dimension]);
    
  /** Scalar operator*=.  Scales elements by a scalar. */
  const Self& operator*=(const ValueType &value);

  /** Scalar operator/=.  Scales (divides) elements by a scalar. */
  const Self& operator/=(const ValueType &value);

  /** Vector operator+=.  Adds a vectors to the current vector. */
  const Self& operator+=(const Self &vec);

  /** Vector operator-=.  Subtracts a vector from a current vector. */
  const Self& operator-=(const Self &vec);

  /** Vector negation.  Negate all the elements of a vector. Return a new
   *  vector */
  Self operator-() const;

  /** Vector addition. Add two vectors. Return a new vector. */
  Self operator+(const Self &vec) const;

  /** Vector subtraction. Subtract two vectors. Return a new vector. */
  Self operator-(const Self &vec) const;

  /** Vector operator*.  Performs the inner product of two vectors.
   * this is also known as the scalar product. */
  ValueType operator*(const Self &vec) const;

  /** Scalar operator*. Scale the elements of a vector by a scalar.
   * Return a new vector. */
  Self operator*(const ValueType& val) const;

  /** Scalar operator/. Scale (divide) the elements of a vector by a scalar.
   * Return a new vector. */
  Self operator/(const ValueType& val) const;

  /** Operators == and != compare a vector component by component. All
   * components must be equal for two vectors to be equal. (Of course
   * compile-time constraints on the template parameters length and type
   * prevent comparisons between vectors of different type and length.) */
  bool operator==(const Self& v) const
    { return Superclass::operator==(v); }
  bool operator!=(const Self& v) const
    { return !operator==(v); }
   
  /** Returns the Euclidean Norm of the vector  */
  ValueType GetNorm( void ) const;

  /** Returns vector's Squared Euclidean Norm  */
  ValueType GetSquaredNorm( void ) const; 

  /** Divides the vector componets by the vector norm */
  void Normalize(void);

  /** Copy from another Vector with a different representation type. 
   *  Casting is done with C-Like rules  */
  template < typename TCoordRepB >
  void CastFrom( const Vector<TCoordRepB,NVectorDimension> & pa )
  {
    for(unsigned int i=0; i<NVectorDimension; i++ )
      {
      (*this)[i] = static_cast<T>( pa[i] );
      }
  }

};

template< class T, unsigned int NVectorDimension >  
ITK_EXPORT std::ostream& operator<<(std::ostream& os, 
                                    const Vector<T,NVectorDimension> & v); 

template< class T, unsigned int NVectorDimension >  
ITK_EXPORT std::istream& operator>>(std::istream& is, 
                                    Vector<T,NVectorDimension> & v); 

ITK_EXPORT Vector<double,3> operator^( const Vector<double,3> &,
                                       const Vector<double,3> &  );

ITK_EXPORT Vector<float,3> operator^( const Vector<float,3> &,
                                      const Vector<float,3> &  );

ITK_EXPORT Vector<int,3> operator^( const Vector<int,3> &,
                                    const Vector<int,3> &  );

} // end namespace itk
  

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVector.txx"
#endif


#endif 
