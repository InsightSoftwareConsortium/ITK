/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVariableLengthVector.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVariableLengthVector_h
#define __itkVariableLengthVector_h

#include "itkMacro.h"
#include "itkNumericTraits.h"

namespace itk
{
/** \class VariableLengthVector 
 * \brief VariableLengthVector is intended to reperesent an array whose
 * length can be defined at run-time. 
 *
 * This class is templated over the data type. This data-type is meant
 * to a scalar, such as float, double etc...
 *
 * \note
 * ITK itself provides several classes that can serve as \c Arrays.
 * 1. FixedArray - Compile time fixed length arrays that's intended to 
 * represent an enumerated collection of \c n entities. 
 * 2. Array - Run time resizeable array that is intended to hold a collection
 * of \c n entities
 * 3. Vector - Compile time fixed length array that is intended to hold
 * a collection of \c n data types. A vector usually has a mathematical meaning. 
 * It should only be used when mathematical operations such as addition, 
 * multiplication by a scalar, product etc make sense. 
 * 4. VariableLengthVector - Run time array that is intended to hold a collection
 * of scalar data types. Again, it should be used only when mathematical
 * operations on it are relevant. If not, use an Array. 
 * 5. Point - Represents the spatial coordinates of a spatial location. Operators
 * on Point reflect geometrical concepts.
 *
 * \par For the reasons listed above, you cannot instantiate 
 * \code VariableLengthVector< bool > \endcode.
 *
 * \par
 * Design Considerations: We do not derive from \c vnl_vector to avoid being
 * limited by the explicit template instantiations of vnl_vector and other 
 * hacks that vnl folks have been forced to use.
 * 
 * \note
 * This work is part of the National Alliance for Medical Image Computing 
 * (NAMIC), funded by the National Institutes of Health through the NIH Roadmap
 * for Medical Research, Grant U54 EB005149.
 *
 * \sa CovariantVector
 * \sa SymmetricSecondRankTensor
 * \sa RGBPixel
 * \sa DiffusionTensor3D
 * \ingroup DataRepresentation 
 */
template <typename TValueType >
class VariableLengthVector
{
public:
 
  /** The element type stored at each location in the Array. */
  typedef TValueType           ValueType;
  typedef TValueType           ComponentType;
  typedef typename NumericTraits< ValueType >::RealType RealValueType;
  typedef VariableLengthVector Self;

  /** Typedef used to indicate the number of elements in the vector */
  typedef unsigned int ElementIdentifier;
  
  /** Default constructor. It is created with an empty array
   *  it has to be allocated later by assignment              */
  VariableLengthVector(); 

  /** Constructor with size. Size can only be changed by assignment */
  VariableLengthVector(unsigned int dimension);

  /** Constructor that initializes array with contents from a user supplied
   * buffer. The pointer to the buffer and the length is specified. By default,
   * the array does not manage the memory of the buffer. It merely points to 
   * that location and it is the user's responsibility to delete it. 
   * If "LetArrayManageMemory" is true, then this class will free the
   * memory when this object is destroyed. */ 
  VariableLengthVector( ValueType* data, unsigned int sz, 
                                        bool LetArrayManageMemory = false);
  
  /** Constructor that initializes array with contents from a user supplied
   * buffer. The pointer to the buffer and the length is specified. By default,
   * the array does not manage the memory of the buffer. It merely points to 
   * that location and it is the user's responsibility to delete it. 
   * If "LetArrayManageMemory" is true, then this class will free the
   * memory when this object is destroyed. */ 
  VariableLengthVector( const ValueType* data, unsigned int sz, 
                                        bool LetArrayManageMemory = false);


  /** Copy constructor. The reason why the copy constructor and the assignment
   * operator are templated is that it will allow implicit casts to be 
   * performed. For instance
   * \code
   * VariableLengthVector< int > vI;
   * VariableLengthVector< float > vF( vI );
   * or for instance vF = static_cast< VariableLengthVector< float > >( vI );
   * \endcode
   **/
  template< class T >
  VariableLengthVector(const VariableLengthVector< T > & v)
    {
    m_NumElements = v.Size();
    m_Data = this->AllocateElements(m_NumElements);
    m_LetArrayManageMemory = true;
    for( ElementIdentifier i=0; i< v.Size(); i++ )
      {
      this->m_Data[i] = static_cast< ValueType >( v[i] );
      }
    }

  /** Copy constructer.. Override the default non-templated copy constructor
   * that the compiler provides */
  VariableLengthVector(const VariableLengthVector< TValueType > & v);
  
  /** Set the all the elements of the array to the specified value */
  void Fill (TValueType const& v); 

  /** Assignment operator  **/
  template< class T >
  const VariableLengthVector< TValueType > & operator= 
                          (const VariableLengthVector< T > & v)
    {
    if( m_Data == static_cast< void * >(const_cast< T * >
              ((const_cast< VariableLengthVector< T > & >(v)).GetDataPointer())) )
      {
      return *this;
      }
    this->SetSize( v.Size() );
    for( ElementIdentifier i=0; i< v.Size(); i++ )
      {
      this->m_Data[i] = static_cast< ValueType >( v[i] );
      }
    return *this;
    }
  
  /** Assignment operator  **/
  const Self & operator=(const Self & v);
     
  /** Return the number of elements in the Array  */
  inline unsigned int Size (void ) const 
      { return m_NumElements; }
  inline unsigned int GetNumberOfElements(void) const 
      { return m_NumElements; }

  /** Return reference to the element at specified index. No range checking. */
  TValueType       & operator[](unsigned int i) { return this->m_Data[i]; }
  /** Return reference to the element at specified index. No range checking. */
  TValueType const & operator[](unsigned int i) const { return this->m_Data[i]; }

  /** Get one element */
  inline const TValueType & GetElement( unsigned int i ) const
    { return m_Data[i]; }

  /** Set one element */
  void SetElement( unsigned int i, const TValueType & value )
    { m_Data[i] = value; }

  /** Set the size to that given. 
   * 
   * If \c destroyExistingData is \c false:
   * If the array already contains data, the existing data is copied over and 
   * new space is allocated, if necessary. If the length to reserve is less 
   * than the current number of elements, then an appropriate number of elements
   * are discarded. 
   *    If \c true, the size is set destructively to the length given. If the 
   * length is different from the current length, existing data will be lost.
   * The default is \c true. */
  void SetSize(unsigned int sz, bool destroyExistingData=true);
  inline unsigned int GetSize(void) const 
      { return m_NumElements; }

  /** Set the pointer from which the data is imported.
   * If "LetArrayManageMemory" is false, then the application retains
   * the responsibility of freeing the memory for this data.  If
   * "LetArrayManageMemory" is true, then this class will free the
   * memory when this object is destroyed. */
  void SetData(TValueType* data,bool LetArrayManageMemory = false);

  /** Similar to the previous method. In the above method, the size must be 
   * seperately set prior to using user-supplied data. This introduces an
   * unnecessary allocation step to be performed. This method avoids it 
   * and should be used to import data whereever possible to avoid this.
   * Set the pointer from which the data is imported.
   * If "LetArrayManageMemory" is false, then the application retains
   * the responsibility of freeing the memory for this data.  If
   * "LetArrayManageMemory" is true, then this class will free the
   * memory when this object is destroyed. */
  void SetData(TValueType* data, unsigned int sz, bool LetArrayManageMemory = false);

  
  /** This destructor is not virtual for performance reasons. However, this
   * means that subclasses cannot allocate memory. */
  ~VariableLengthVector();


  /** Reserves memory of a certain length. 
   *
   * If the array already contains data, the existing data is copied over and 
   * new space is allocated, if necessary. If the length to reserve is less 
   * than the current number of elements, then an appropriate number of elements
   * are discarded. */
  void Reserve( ElementIdentifier );

  /** Allocate memory of certain size and return it.  */
  TValueType * AllocateElements( ElementIdentifier size ) const;

  const TValueType* GetDataPointer() const { return m_Data; }

  /** Mathematical operators.
   * \note For efficiency, the operators do not check to see of the length of 
   * the vectors are the same. For instance it is assumed that if you are adding
   * VariableLengthVector a and b, they are of the same length. */
  template< class T > 
  inline Self operator+(const VariableLengthVector< T > &v) const
    {
    // if( m_NumElements != v.GetSize() ) 
    //   { 
    //   itkGenericExceptionMacro( << "Cannot add VariableLengthVector of length " 
    //                             << m_NumElements " and " << v.GetSize() );
    //   }      
    const ElementIdentifier length = v.Size();
    Self result( length );
    for( ElementIdentifier i=0; i< length; i++ )
      {
      result[i] = (*this)[i] + static_cast< ValueType >( v[i] );
      }
    return result;
    }
  template< class T > 
  inline Self operator-(const VariableLengthVector< T > &v) const
    {
    // if( m_NumElements != v.GetSize() ) 
    //   { 
    //   itkGenericExceptionMacro( << "Cannot add VariableLengthVector of length " 
    //                             << m_NumElements " and " << v.GetSize() );
    //   }      
    const ElementIdentifier length = v.Size();
    Self result( length );
    for( ElementIdentifier i=0; i< length; i++ )
      {
      result[i] = (*this)[i] - static_cast< ValueType >( v[i] );
      }
    return result;
    }
  template< class T > inline Self operator*( T s ) const
    {
    Self result( m_NumElements );
    for( ElementIdentifier i=0; i< m_NumElements; i++ )
      {
      result[i] = m_Data[i] * static_cast< ValueType >( s );
      }
    return result;
    }
  template< class T > inline Self operator/( T s ) const
    {
    Self result( m_NumElements );
    for( ElementIdentifier i=0; i< m_NumElements; i++ )
      {
      result[i] = m_Data[i] / (static_cast< ValueType >( s ));
      }
    return result;
    }
  inline Self operator+( TValueType s ) const
    {
    Self result( m_NumElements );
    for( ElementIdentifier i=0; i< m_NumElements; i++ )
      {
      result[i] = m_Data[i] + s;
      }
    return result;
    }
  inline Self operator-( TValueType s ) const
    {
    Self result( m_NumElements );
    for( ElementIdentifier i=0; i< m_NumElements; i++ )
      {
      result[i] = m_Data[i] - s;
      }
    return result;
    }
  inline Self& operator--()
    {
    for( ElementIdentifier i=0; i< m_NumElements; i++ )
      {
      this->m_Data[i] -= static_cast< ValueType >( 1.0 );
      }
    return *this;
    }
  inline Self& operator++() // prefix operator ++v;
    {
    for( ElementIdentifier i=0; i< m_NumElements; i++ )
      {
      this->m_Data[i] += static_cast< ValueType >( 1.0 );
      }
    return *this;
    }
  inline Self operator--(int) // postfix operator v--;
    {
      Self tmp(*this);
      --tmp;
      return tmp;
    }
  inline Self operator++(int) // postfix operator v++;
    {
      Self tmp(*this);
      ++tmp;
      return tmp;
    }
   template< class T > inline Self& operator-=
                  ( const VariableLengthVector< T > &v )
    {
    for( ElementIdentifier i=0; i< m_NumElements; i++ )
      {
      m_Data[i] -= static_cast< ValueType >( v[i] );
      }
    return *this;
    }
  inline Self& operator-=( TValueType s )
    {
    for( ElementIdentifier i=0; i< m_NumElements; i++ )
      {
      m_Data[i] -= s ;
      }
    return *this;
    }
  template< class T > inline Self& operator+=
                  ( const VariableLengthVector< T > &v )
    {
    for( ElementIdentifier i=0; i< m_NumElements; i++ )
      {
      m_Data[i] += static_cast< ValueType >( v[i] );
      }
    return *this;
    }
  inline Self& operator+=( TValueType s )
    {
    for( ElementIdentifier i=0; i< m_NumElements; i++ )
      {
      m_Data[i] += s;
      }
    return *this;
    }
  template< class T > inline Self& operator*=( T s )
    {
    for( ElementIdentifier i=0; i< m_NumElements; i++ )
      {
      m_Data[i] *= (static_cast< ValueType >( s ));
      }
    return *this;
    }
  template< class T > inline Self& operator/=( T s )
    {
    for( ElementIdentifier i=0; i< m_NumElements; i++ )
      {
      m_Data[i] /= (static_cast< ValueType >( s ));
      }
    return *this;
    }
  Self & operator- (); // negation operator
  bool operator==( const Self &v) const;
  bool operator!=( const Self &v) const;
    
  /** Returns vector's Squared Euclidean Norm  */
  RealValueType GetSquaredNorm() const;

private:

  bool m_LetArrayManageMemory; // if true, the array is responsible for memory 
                               // of data
  TValueType *m_Data; // Array to hold data
  ElementIdentifier m_NumElements;
};

/** Premultiply Operator for product of a VariableLengthVector and a scalar. 
 *  VariableLengthVector< TValueType >  =  T * VariableLengthVector< TValueType > 
 */
template< class TValueType, class T >
inline
VariableLengthVector<TValueType>
operator*(const T &scalar, const VariableLengthVector<TValueType> &v)
{
  return v * scalar;
}

  
template <typename TValueType >
std::ostream & operator<<(std::ostream &os, const VariableLengthVector<TValueType> &arr)
{
  const unsigned int length = arr.Size();
  const signed int last   = (unsigned int) length - 1;

  os << "[";
  for (signed int i=0; i < last; ++i)
    {
    os << arr[i] << ", ";
    }
  if (length >= 1)
    {
    os << arr[last];
    }
  os << "]";
  return os;
}

} // namespace itk

#include "itkNumericTraitsVariableLengthVectorPixel.h"

// Define instantiation macro for this template.
#define ITK_TEMPLATE_VariableLengthVector(_, EXPORT, x, y) namespace itk { \
  _(1(class EXPORT VariableLengthVector< ITK_TEMPLATE_1 x >)) \
  namespace Templates { typedef VariableLengthVector< ITK_TEMPLATE_1 x > VariableLengthVector##y; } \
  }

#if ITK_TEMPLATE_EXPLICIT
# include "Templates/itkVariableLengthVector+-.h"
#endif

#if ITK_TEMPLATE_TXX
# include "itkVariableLengthVector.txx"
#endif

#endif
