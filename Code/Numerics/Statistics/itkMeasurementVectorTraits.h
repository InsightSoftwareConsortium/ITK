/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMeasurementVectorTraits.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMeasurementVectorTraits_h
#define __itkMeasurementVectorTraits_h

#include "itkMacro.h"
#include "itkArray.h"
#include "itkVector.h"
#include "itkFixedArray.h"
#include "vnl/vnl_vector_fixed.h"
#include "itkPoint.h"
#include "itkRGBPixel.h"
#include "itkMatrix.h"
#include "itkVariableSizeMatrix.h"
#include "itkNumericTraits.h"


namespace itk
{

/** \class MeasurementVectorTraits
 * \brief Define traits that set/return the length of a measurement vector.
 * This class uses traits to enforce consistency when dealing with measurement
 * vector lengths across different classes like std::vector, Array, Vector, 
 * FixedArray etc.
 *
 * The class is primarily intended to be used by developers adding statistics
 * classes to the toolkit.
 *
 * For instance, the developer can create a measurement vector as
 *
 * \code
 * typename SampleType:: MeasurementVectorType m_MeasurementVector 
 * = MeasurementVectorTraits< typename 
 *    SampleType::MeasurementVectorType >::SetSize( s ) );
 * \endcode
 *
 * This will create a measurement vector of length s if it is a FixedArray or 
 * a vnl_vector_fixed, itkVector etc.. If not it returns an array of length 0
 * for the appropriate type. Other useful typedefs are defined to get the 
 * length of the vector, for the MeanType, RealType for compuatations etc
 *
 * To get the length of a measurement vector, the user would
 *
 * \code
 * MeasurementVectorTraits< MeasurementVectorType >::GetSize( &mv )
 * \endcode
 * 
 * This calls the appropriate functions for the MeasurementVectorType to return
 * the size of the measurement vector mv.
 *
 * \code
 * MeasurementVectorTraits< MeasurementVectorType >::GetSize()
 * \endcode
 *
 * This returns the length of MeasurementVectorType, which will be the true
 * length of a FixedArray, Vector, vnl_vector_fixed, Point etc and 0 otherwise
 * 
 * \ingroup Statistics 
 */

template <class T>
class MeasurementVectorTraits {
public:
  typedef T    MeasurementVectorType;

  /** Generic method to create a container T of the specified length. This is
   * set via assignment. So the container should also support the assignment
   * operator. ie
   * \code
   * MeasurementVectorType mv = MeasurementVectorTraits< 
   *         MeasurementVectorType >::SetSize( 3 );
   * \endcode
   */
  static  T    SetSize( unsigned int );
  
  /** Generic method to get the size of a containter */
  static unsigned int GetSize( const MeasurementVectorType * );
  
  /** Same as the StaticConstMacro MeasurementVectorLength.
   * \code
   * MeasurementVectorTraits< MeasurementVectorType >::MeasurementVectorLength
   * \endcode
   * will have a value = length of the container for fixed length containters and
   * zero otherwise. 
   *
   * \c MeasurementVectorTraits<MeasurementVectorType>::GetSize() returns the same.
   */
  static unsigned int GetSize();
};

template<class TValueType, unsigned int TLength>
class MeasurementVectorTraits<FixedArray<TValueType, TLength > >
{
public:
  itkStaticConstMacro( MeasurementVectorLength, unsigned int, TLength );
  typedef TValueType                                       ValueType;
  typedef double                                           RealValueType;
  typedef FixedArray< ValueType, MeasurementVectorLength > MeasurementVectorType;
  typedef FixedArray< RealValueType, 
                                 MeasurementVectorLength > RealMeasurementVectorType;
  typedef Matrix< RealValueType, MeasurementVectorLength, 
                                 MeasurementVectorLength > RealMatrixType;
  typedef Vector< RealValueType, MeasurementVectorLength > MeanType;
  typedef Vector< RealValueType, MeasurementVectorLength > OriginType;
  typedef FixedArray< bool,      MeasurementVectorLength > BooleanArrayType;
  typedef FixedArray< RealValueType, MeasurementVectorLength > RealArrayType;

  static MeasurementVectorType SetSize( unsigned int s )
    {
    if( s != MeasurementVectorLength )
      {
      itkGenericExceptionMacro( << "Cannot set the size of a FixedArray of length " 
          << MeasurementVectorLength << " to " << s );
      }
    MeasurementVectorType m;
    m.Fill( NumericTraits< ValueType >::Zero );
    return m;
    }

  static unsigned int GetSize( const MeasurementVectorType *m )
    {
    return MeasurementVectorLength;
    }

  static unsigned int GetSize()
    {
    return MeasurementVectorLength;
    }

  static RealMatrixType RealMatrix( unsigned int r, unsigned int c )
    {
    RealMatrixType m;
    return m;
    }
};

template< class TValueType >
class MeasurementVectorTraits< Array< TValueType > >
{
public:
  typedef TValueType                   ValueType;
  typedef double                       RealValueType;
  typedef Array< ValueType >           MeasurementVectorType;
  typedef Array< RealValueType >       RealMeasurementVectorType;
  typedef VariableSizeMatrix< double > RealMatrixType;
  typedef RealMeasurementVectorType    MeanType;
  typedef RealMeasurementVectorType    OriginType;
  typedef Array< bool >                BooleanArrayType;
  typedef Array< RealValueType >       RealArrayType;
  itkStaticConstMacro( MeasurementVectorLength, unsigned int, 0 );

  static MeasurementVectorType SetSize( unsigned int s )
    {
    MeasurementVectorType m( s );
    return m;
    }
  
  static unsigned int GetSize( const MeasurementVectorType *m )
    {
    if( m == NULL )
      {
      itkGenericExceptionMacro( << "MeasurementVector is NULL" );
      }
    return m->Size();
    }

  static unsigned int GetSize()
    {
    return 0;
    }

  static RealMatrixType RealMatrix( unsigned int r, unsigned int c )
    {
    RealMatrixType m;
    m.SetSize( r, c );
    return m;
    }
};


template< class TValueType, unsigned int TLength >
class MeasurementVectorTraits< Vector< TValueType, TLength > >
{
public:
  typedef TValueType ValueType;
  typedef double     RealValueType;
  itkStaticConstMacro( MeasurementVectorLength, unsigned int, TLength );
  typedef Vector< RealValueType, 
          MeasurementVectorLength > RealMeasurementVectorType;
  typedef Matrix< RealValueType, MeasurementVectorLength, 
                                 MeasurementVectorLength > RealMatrixType;
  typedef RealMeasurementVectorType                        MeanType;
  typedef RealMeasurementVectorType                        OriginType;
  typedef Vector< ValueType, MeasurementVectorLength >     MeasurementVectorType;
  typedef FixedArray< bool,  MeasurementVectorLength >     BooleanArrayType;
  typedef FixedArray< RealValueType, MeasurementVectorLength > RealArrayType;

  static MeasurementVectorType SetSize( unsigned int s )
    {
    if( s != MeasurementVectorLength )
      {
      itkGenericExceptionMacro( << "Cannot set the size of a Vector of length " 
          << MeasurementVectorLength << " to " << s );
      }
    MeasurementVectorType m;
    m.Fill( NumericTraits< ValueType >::Zero );
    return m;
    }

  static unsigned int GetSize( const MeasurementVectorType *m )
    {
    return MeasurementVectorLength; 
    }

  static unsigned int GetSize()
    {
    return MeasurementVectorLength;
    }

  static RealMatrixType RealMatrix( unsigned int r, unsigned int c )
    {
    RealMatrixType m;
    return m;
    }
};

template< class TValueType >
class MeasurementVectorTraits< vnl_vector< TValueType > >
{
public:
  typedef TValueType                   ValueType;
  typedef double                       RealValueType;
  typedef vnl_vector< ValueType >      MeasurementVectorType;
  typedef vnl_vector< RealValueType >  RealMeasurementVectorType;
  typedef VariableSizeMatrix< double > RealMatrixType;
  typedef RealMeasurementVectorType    MeanType;
  typedef RealMeasurementVectorType    OriginType;
  typedef Array< bool >                BooleanArrayType;
  typedef Array< RealValueType >       RealArrayType;
  itkStaticConstMacro( MeasurementVectorLength, unsigned int, 0 );

  static MeasurementVectorType SetSize( unsigned int s )
    {
    MeasurementVectorType m( s );
    return m;
    }
  
  static unsigned int GetSize( const MeasurementVectorType *m )
    {
    if( m == NULL )
      {
      itkGenericExceptionMacro( << "MeasurementVector is NULL" );
      }
    return m->size();
    }

  static unsigned int GetSize()
    {
    return 0;
    }

  static RealMatrixType RealMatrix( unsigned int r, unsigned int c )
    {
    RealMatrixType m;
    m.SetSize( r, c );
    return m;
    }
};


template<class TValueType, unsigned int TLength>
class MeasurementVectorTraits< vnl_vector_fixed<TValueType, TLength > >
{
public:
  typedef TValueType ValueType;
  typedef double     RealValueType;
  itkStaticConstMacro( MeasurementVectorLength, unsigned int, TLength );
  typedef vnl_vector_fixed< ValueType, MeasurementVectorLength > 
                                        MeasurementVectorType;
  typedef vnl_vector_fixed< RealValueType, 
          MeasurementVectorLength > RealMeasurementVectorType;
  typedef Matrix< RealValueType, MeasurementVectorLength, 
                                 MeasurementVectorLength > RealMatrixType;
  typedef RealMeasurementVectorType                        MeanType;
  typedef Vector< RealValueType, MeasurementVectorLength > OriginType;
  typedef FixedArray< bool,      MeasurementVectorLength > BooleanArrayType;
  typedef FixedArray< RealValueType, MeasurementVectorLength > RealArrayType;
  
  
  static MeasurementVectorType SetSize( unsigned int s )
    {
    if( s != MeasurementVectorLength )
      {
      itkGenericExceptionMacro( << "Cannot set the size of a vnl_vector_fixed of length " 
          << MeasurementVectorLength << " to " << s );
      }
    MeasurementVectorType m;
    return m;
    }
  
  static unsigned int GetSize( const MeasurementVectorType *m )
    {
    return MeasurementVectorLength; 
    }

  static unsigned int GetSize()
    {
    return MeasurementVectorLength;
    }

  static RealMatrixType RealMatrix( unsigned int r, unsigned int c )
    {
    RealMatrixType m;
    return m;
    }
};

template< class TValueType >
class MeasurementVectorTraits< std::vector< TValueType > >
{
public:
  typedef TValueType ValueType;
  typedef double     RealValueType;
  typedef std::vector< ValueType > MeasurementVectorType;
  typedef std::vector< RealValueType > RealMeasurementVectorType;
  typedef VariableSizeMatrix< double > RealMatrixType;
  typedef RealMeasurementVectorType    MeanType;
  typedef RealMeasurementVectorType    OriginType;
  typedef Array< bool >                BooleanArrayType;
  typedef Array< RealValueType >       RealArrayType;
  itkStaticConstMacro( MeasurementVectorLength, unsigned int, 0 );

  static MeasurementVectorType SetSize( unsigned int s )
    {
    MeasurementVectorType m( s );
    return m;
    }
  
  static unsigned int GetSize( const MeasurementVectorType *m )
    {
    if( m == NULL )
      {
      itkGenericExceptionMacro( << "MeasurementVector is NULL" );
      }
    return m->size();
    }

  static unsigned int GetSize()
    {
    return 0;
    }

  static RealMatrixType RealMatrix( unsigned int r, unsigned int c )
    {
    RealMatrixType m;
    m.SetSize( r, c );
    return m;
    }
};

template<class TValueType, unsigned int TLength>
class MeasurementVectorTraits< Point< TValueType, TLength > >
{
public:
  typedef TValueType ValueType;
  typedef double     RealValueType;
  itkStaticConstMacro( MeasurementVectorLength, unsigned int, TLength );
  typedef Point< ValueType, MeasurementVectorLength > MeasurementVectorType;
  typedef Point< RealValueType, MeasurementVectorLength > RealMeasurementVectorType;
  typedef Matrix< RealValueType, MeasurementVectorLength, 
                                 MeasurementVectorLength > RealMatrixType;
  typedef Vector< RealValueType, MeasurementVectorLength>  MeanType;
  typedef Vector< RealValueType, MeasurementVectorLength>  OriginType;
  typedef FixedArray< bool,      MeasurementVectorLength > BooleanArrayType;
  typedef FixedArray< RealValueType, MeasurementVectorLength > RealArrayType;
  
  static MeasurementVectorType SetSize( unsigned int s )
    {
    if( s != MeasurementVectorLength )
      {
      itkGenericExceptionMacro( << "Cannot set the size of a Point of length " 
          << MeasurementVectorLength << " to " << s );
      }
    MeasurementVectorType m;
    for( unsigned int i=0; i< MeasurementVectorLength; i++ )
      {
      m[i] = NumericTraits< ValueType >::Zero;
      }
    return m;
    }

  static unsigned int GetSize( const MeasurementVectorType *m )
    {
    return MeasurementVectorLength;
    }

  static unsigned int GetSize()
    {
    return 0;
    }

  static RealMatrixType RealMatrix( unsigned int r, unsigned int c )
    {
    RealMatrixType m;
    return m;
    }
};

template<class TValueType >
class MeasurementVectorTraits< RGBPixel<TValueType > >
{
public:
  typedef TValueType ValueType;
  typedef double     RealValueType;
  itkStaticConstMacro( MeasurementVectorLength, unsigned int, 3 );
  typedef RGBPixel< ValueType > MeasurementVectorType;
  typedef RGBPixel< RealValueType > RealMeasurementVectorType;
  typedef Matrix< RealValueType, MeasurementVectorLength, 
                                 MeasurementVectorLength > RealMatrixType;
  typedef Vector< RealValueType, MeasurementVectorLength > MeanType;
  typedef Vector< RealValueType, MeasurementVectorLength > OriginType;
  typedef FixedArray< bool,      MeasurementVectorLength > BooleanArrayType;
  typedef FixedArray< RealValueType, MeasurementVectorLength > RealArrayType;
  
  static MeasurementVectorType SetSize( unsigned int s )
    {
    if( s != MeasurementVectorLength )
      {
      itkGenericExceptionMacro( << "Cannot set the size of an RGBPixel of length " 
          << MeasurementVectorLength << " to " << s );
      }
    MeasurementVectorType m;
    m.Fill( NumericTraits< ValueType >::Zero );
    return m;
    }

  static unsigned int GetSize( const MeasurementVectorType *m )
    {
    return MeasurementVectorLength;
    }

  static unsigned int GetSize()
    {
    return MeasurementVectorLength;
    }

  static RealMatrixType RealMatrix( unsigned int r, unsigned int c )
    {
    RealMatrixType m;
    return m;
    }
};


} // __itkMeasurementVectorTraits_h

#endif
