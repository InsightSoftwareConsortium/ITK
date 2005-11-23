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
#include "itkVariableLengthVector.h"
#include "itkVector.h"
#include "itkFixedArray.h"
#include "vnl/vnl_vector_fixed.h"
#include "itkPoint.h"
#include "itkRGBPixel.h"
#include "itkMatrix.h"
#include "itkVariableSizeMatrix.h"
#include "itkNumericTraits.h"
#include "itkSize.h"
#include <vector>


namespace itk
{

/** \class MeasurementVectorTraits
 * \brief   
 * \ingroup Statistics 
 */


class MeasurementVectorTraits {
public:

  typedef unsigned int MeasurementVectorLength;
  
  template<class TValueType, unsigned int TLength>
  static void SetLength( FixedArray< TValueType, TLength > &m, const unsigned int s )
    {
    if( s != TLength )
      {
      itkGenericExceptionMacro( << "Cannot set the size of a FixedArray of length " 
          << TLength << " to " << s );
      }
    m.Fill( NumericTraits< TValueType >::Zero );
    }
  
  template<class TValueType, unsigned int TLength>
  static void SetLength( FixedArray< TValueType, TLength > *m, const unsigned int s )
    {
    if( s != TLength )
      {
      itkGenericExceptionMacro( << "Cannot set the size of a FixedArray of length " 
          << TLength << " to " << s );
      }
    m->Fill( NumericTraits< TValueType >::Zero );
    }
  
  template< class TValueType >
  static void SetLength( Array< TValueType > & m, const unsigned int s )
    {
    m.SetSize( s );
    m.Fill( NumericTraits< TValueType >::Zero );
    }
  
  template< class TValueType >
  static void SetLength( Array< TValueType > * m, const unsigned int s )
    {
    m->SetSize( s );
    m->Fill( NumericTraits< TValueType >::Zero );
    }

  template< class TValueType >
  static void SetLength( VariableLengthVector< TValueType > & m, const unsigned int s )
    {
    m.SetSize( s );
    m.Fill( NumericTraits< TValueType >::Zero );
    }
  
  template< class TValueType >
  static void SetLength( VariableLengthVector< TValueType > * m, const unsigned int s )
    {
    m->SetSize( s );
    m->Fill( NumericTraits< TValueType >::Zero );
    }

  template< class TValueType >
  static void SetLength( std::vector< TValueType > & m, const unsigned int s )
    {
    m.resize( s );
    }
  
  template< class TValueType >
  static void SetLength( std::vector< TValueType > * m, const unsigned int s )
    {
    m->resize( s );
    }


  template< class TValueType, unsigned int TLength > 
  static MeasurementVectorLength 
               GetLength( const FixedArray< TValueType, TLength > &)
    { return TLength; }
  
  template< class TValueType, unsigned int TLength > 
  static MeasurementVectorLength 
               GetLength( const FixedArray< TValueType, TLength > *)
    { return TLength; }

  template< class TValueType >
  static MeasurementVectorLength
               GetLength( const Array< TValueType > &m )
    {return m.GetSize(); }
  
  template< class TValueType >
  static MeasurementVectorLength
               GetLength( const Array< TValueType > *m )
    {return m->GetSize(); }

  template< class TValueType >
  static MeasurementVectorLength
               GetLength( const VariableLengthVector< TValueType > &m )
    {return m.GetSize(); }
  
  template< class TValueType >
  static MeasurementVectorLength
               GetLength( const VariableLengthVector< TValueType > *m )
    {return m->GetSize(); }

  template< class TValueType >
  static MeasurementVectorLength
               GetLength( const std::vector< TValueType > &m )
    {return m.size(); }
  
  template< class TValueType >
  static MeasurementVectorLength
               GetLength( const std::vector< TValueType > *m )
    {return m->size(); }


  template< class TValueType1, unsigned int TLength, class TValueType2 >
  static MeasurementVectorLength Assert( const FixedArray< TValueType1, TLength > &, 
                      const Array< TValueType2 > &b, const char *errMsg="Length Mismatch")
    {
    if( b.Size() == 0 )
      {
      return TLength;
      }
    if( b.Size() != 0 )
      {
      if (b.Size() != TLength)
        {
        itkGenericExceptionMacro( << errMsg );
        return 0;
        }
      }
    return 0;
    }

  template< class TValueType1, unsigned int TLength, class TValueType2 >
  static MeasurementVectorLength Assert( const FixedArray< TValueType1, TLength > *, 
                      const Array< TValueType2 > *b, const char *errMsg="Length Mismatch")
    {
    if( b->Size() == 0 )
      {
      return TLength;
      }
    else if (b->Size() != TLength)
      {
      itkGenericExceptionMacro( << errMsg );
      return 0;
      }
    return 0;
    }

  template< class TValueType1, unsigned int TLength, class TValueType2 >
  static MeasurementVectorLength Assert( const FixedArray< TValueType1, TLength > &, 
                      const VariableLengthVector< TValueType2 > &b, const char *errMsg="Length Mismatch")
    {
    if( b.Size() == 0 )
      {
      return TLength;
      }
    if( b.Size() != 0 )
      {
      if (b.Size() != TLength)
        {
        itkGenericExceptionMacro( << errMsg );
        return 0;
        }
      }
    return 0;
    }

  template< class TValueType1, unsigned int TLength, class TValueType2 >
  static MeasurementVectorLength Assert( const FixedArray< TValueType1, TLength > *, 
                      const VariableLengthVector< TValueType2 > *b, const char *errMsg="Length Mismatch")
    {
    if( b->Size() == 0 )
      {
      return TLength;
      }
    else if (b->Size() != TLength)
      {
      itkGenericExceptionMacro( << errMsg );
      return 0;
      }
    return 0;
    }

  template< class TValueType1, unsigned int TLength>
  static MeasurementVectorLength Assert( const FixedArray< TValueType1, TLength > &, 
                const MeasurementVectorLength l, const char *errMsg="Length Mismatch")
    {
    if( l == 0 )
      {
      return TLength;
      }
    else if( l != TLength )
      {
      itkGenericExceptionMacro( << errMsg );
      return 0;
      }
    return 0;
    }

  template< class TValueType1, unsigned int TLength>
  static MeasurementVectorLength Assert( const FixedArray< TValueType1, TLength > *, 
               const MeasurementVectorLength l, const char *errMsg="Length Mismatch")
    {
    if( l == 0 )
      {
      return TLength;
      }
    else if( l != TLength )
      {
      itkGenericExceptionMacro( << errMsg );
      return 0;
      }
    return 0;
    }

  template< class TValueType >
  static MeasurementVectorLength Assert( const Array< TValueType > &a, 
              const MeasurementVectorLength l, const char *errMsg="Length Mismatch")
    {
    if( ((l!=0) && (a.Size()!=l)) || (a.Size()==0) )
      {
      itkGenericExceptionMacro( << errMsg );
      }
    else if( l == 0 )
      {
      return a.Size();
      }
    return 0;
    }
  
  template< class TValueType >
  static MeasurementVectorLength Assert( const Array< TValueType > *a, 
              const MeasurementVectorLength l, const char *errMsg="Length Mismatch")
    {
    if( ((l!=0) && (a->Size()!=l)) || (a->Size()==0) )
      {
      itkGenericExceptionMacro( << errMsg );
      }
    else if( l == 0 )
      {
      return a->Size();
      }
    return 0;
    }
   
  template< class TValueType >
  static MeasurementVectorLength Assert( const VariableLengthVector< TValueType > &a, 
              const MeasurementVectorLength l, const char *errMsg="Length Mismatch")
    {
    if( ((l!=0) && (a.Size()!=l)) || (a.Size()==0) )
      {
      itkGenericExceptionMacro( << errMsg );
      }
    else if( l == 0 )
      {
      return a.Size();
      }
    return 0;
    }
  
  template< class TValueType >
  static MeasurementVectorLength Assert( const VariableLengthVector< TValueType > *a, 
              const MeasurementVectorLength l, const char *errMsg="Length Mismatch")
    {
    if( ((l!=0) && (a->Size()!=l)) || (a->Size()==0) )
      {
      itkGenericExceptionMacro( << errMsg );
      }
    else if( l == 0 )
      {
      return a->Size();
      }
    return 0;
    }
   template< class TValueType >
  static MeasurementVectorLength Assert( const std::vector< TValueType > &a, 
              const MeasurementVectorLength l, const char *errMsg="Length Mismatch")
    {
    if( ((l!=0) && (a.size()!=l)) || (a.size()==0) )
      {
      itkGenericExceptionMacro( << errMsg );
      }
    else if( l == 0 )
      {
      return a.size();
      }
    return 0;
    }
  
  template< class TValueType >
  static MeasurementVectorLength Assert( const std::vector< TValueType > *a, 
              const MeasurementVectorLength l, const char *errMsg="Length Mismatch")
    {
    if( ((l!=0) && (a->size()!=l)) || (a->size()==0) )
      {
      itkGenericExceptionMacro( << errMsg );
      }
    else if( l == 0 )
      {
      return a->size();
      }
    return 0;
    }
 };

} // namespace itk

#endif  // __itkMeasurementVectorTraits_h

