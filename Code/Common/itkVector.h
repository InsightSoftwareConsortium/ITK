/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVector.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVector_h
#define __itkVector_h

#include "itkFixedArray.h"

#include "itkNumericTraits.h"   // RealValueType type
#include <vnl/vnl_vector_ref.h> // Get_vnl_vector method return


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
  typedef typename NumericTraits< ValueType >::RealType   RealValueType;

  /** Dimension of the vector space. */
  itkStaticConstMacro(Dimension, unsigned int, NVectorDimension);

  /** I am a vector type. */
  typedef Self VectorType;

  /** Component value type */
  typedef T ComponentType;

  /** The Array type from which this vector is derived. */
  typedef FixedArray<T, NVectorDimension>                BaseArray;
    
  /** Get the dimension (size) of the vector. */
  static unsigned int GetVectorDimension() 
    { return NVectorDimension; }  

  /** Set a vnl_vector_ref referencing the same memory block. */
  void SetVnlVector( const vnl_vector<T> & );

  /** Get a vnl_vector_ref referencing the same memory block. */
  vnl_vector_ref<T> GetVnlVector( void );

  /** Get a vnl_vector with a copy of the internal memory block. */
  vnl_vector<T> GetVnlVector( void ) const;


  /** Set a vnl_vector_ref referencing the same memory block.
   * \deprecated Use SetVnlVector() instead. */
  void Set_vnl_vector( const vnl_vector<T> & );

  /** Get a vnl_vector_ref referencing the same memory block. 
   * \deprecated Use GetVnlVector() instead. */
  vnl_vector_ref<T> Get_vnl_vector( void );

  /** Get a vnl_vector with a copy of the internal memory block. 
   * \deprecated Use GetVnlVector() instead. */
  vnl_vector<T> Get_vnl_vector( void ) const;

  /** Default constructor and copy constructors. */
  Vector(): BaseArray() { }
  Vector(const ValueType& r);
  
  /** Pass-through constructor for the Array base class. */
  template< class TVectorValueType >
  Vector(const Vector< TVectorValueType, NVectorDimension>& r): BaseArray(r) {}
  Vector(const ValueType r[Dimension]): BaseArray(r) {}  
    
  /** Pass-through assignment operator for the Array base class. */
  template< class TVectorValueType >
  Vector& operator= (const Vector< TVectorValueType, NVectorDimension> & r)
    {
    BaseArray::operator=(r);
    return *this;
    }
 
  Vector& operator= (const ValueType r[NVectorDimension]);
    
  /** Scalar operator*=.  Scales elements by a scalar. */
  template< class Tt > inline const Self& operator*=(const Tt &value)
    {
    for( unsigned int i=0; i<NVectorDimension; i++)
      {
      (*this)[i] = static_cast< ValueType >((*this)[i] * value);
      }
    return *this;
    }

  /** Scalar operator/=.  Scales (divides) elements by a scalar. */
  template< class Tt > inline const Self& operator/=(const Tt &value)
    {
    for( unsigned int i=0; i<NVectorDimension; i++)
      {
      (*this)[i] = static_cast< ValueType >((*this)[i] / value);
      }
    return *this;
    }

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
  inline Self operator*(const ValueType& value) const
    {
    Self result;
    for( unsigned int i=0; i<NVectorDimension; i++) 
      {
      result[i] = static_cast< ValueType >((*this)[i] * value);
      }
    return result;
    }

  /** Scalar operator/. Scale (divide) the elements of a vector by a scalar.
   * Return a new vector. */
  template< class Tt > inline Self operator/(const Tt& value) const
    {
    Self result;
    for( unsigned int i=0; i<NVectorDimension; i++) 
      {
      result[i] = static_cast< ValueType >((*this)[i] / value);
      }
    return result;
    }

  /** Operators == and != compare a vector component by component. All
   * components must be equal for two vectors to be equal. (Of course
   * compile-time constraints on the template parameters length and type
   * prevent comparisons between vectors of different type and length.) */
  bool operator==(const Self& v) const
    { return Superclass::operator==(v); }
  bool operator!=(const Self& v) const
    { return !operator==(v); }
   
  /** Returns the Euclidean Norm of the vector  */
  RealValueType GetNorm( void ) const;

  /** Returns vector's Squared Euclidean Norm  */
  RealValueType GetSquaredNorm( void ) const; 

  /** Returns the number of components in this vector type */
  static unsigned int GetNumberOfComponents(){ return NVectorDimension;}
  
  /** Divides the vector componets by the vector norm */
  void Normalize(void);

  void SetNthComponent(int c, const ComponentType& v)  
    {  this->operator[](c) = v; }
  
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

/** Premultiply Operator for product of a vector and a scalar. 
 *  Vector< T, N >  =  T * Vector< T,N > */
template< class T, unsigned int NVectorDimension >
inline
Vector<T,NVectorDimension>
operator*(const T &scalar, const  Vector<T,NVectorDimension> & v)
{
  return v * scalar;
}

template< class T, unsigned int NVectorDimension >  
std::ostream& operator<<(std::ostream& os, 
                                    const Vector<T,NVectorDimension> & v); 

template< class T, unsigned int NVectorDimension >  
std::istream& operator>>(std::istream& is, 
                                    Vector<T,NVectorDimension> & v); 

ITKCommon_EXPORT Vector<double,3> CrossProduct( const Vector<double,3> &,
                                          const Vector<double,3> &  );

ITKCommon_EXPORT Vector<float,3> CrossProduct( const Vector<float,3> &,
                                         const Vector<float,3> &  );

ITKCommon_EXPORT Vector<int,3> CrossProduct( const Vector<int,3> &,
                                       const Vector<int,3> &  );

} // end namespace itk


// Define instantiation macro for this template.
#define ITK_TEMPLATE_Vector(_, EXPORT, x, y) namespace itk { \
  _(2(class EXPORT Vector< ITK_TEMPLATE_2 x >)) \
  _(1(EXPORT std::ostream& operator<<(std::ostream&, \
                                      const Vector< ITK_TEMPLATE_2 x >&))) \
  _(1(EXPORT std::istream& operator>>(std::istream&, \
                                      Vector< ITK_TEMPLATE_2 x >&))) \
  namespace Templates { typedef Vector< ITK_TEMPLATE_2 x > Vector##y; } \
  }

#if ITK_TEMPLATE_EXPLICIT
# include "Templates/itkVector+-.h"
#endif

#if ITK_TEMPLATE_TXX
# include "itkVector.txx"
#endif


#if ITK_TEMPLATE_EXPLICIT
  #include "itkNumericTraitsVectorPixel.h"
#endif


#endif
