/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkHilbertPath_hxx
#define itkHilbertPath_hxx

#include "itkHilbertPath.h"

namespace itk
{

/** Constructor */
template<typename TIndexValue, unsigned int VDimension>
HilbertPath<TIndexValue, VDimension>
::HilbertPath() :
  m_HilbertOrder( 1 )
{
}

template<typename TIndexValue, unsigned int VDimension>
void
HilbertPath<TIndexValue, VDimension>
::ConstructHilbertPath()
{
  const SizeValueType numberOfPathVertices (
    static_cast< SizeValueType >( 1 ) << static_cast< SizeValueType >( Dimension * this->m_HilbertOrder ) );
  this->m_HilbertPath.resize( numberOfPathVertices );

  SizeValueType counter = 0;

  typename HilbertPathType::iterator it;
  for( it = this->m_HilbertPath.begin(); it != this->m_HilbertPath.end(); ++it )
    {
    *it = this->TransformPathIndexToMultiDimensionalIndex( counter++ );
    }
}

template<typename TIndexValue, unsigned int VDimension>
typename HilbertPath<TIndexValue, VDimension>::IndexType
HilbertPath<TIndexValue, VDimension>
::TransformPathIndexToMultiDimensionalIndex( const PathIndexType id )
{
  PathIndexType d = 0;
  PathIndexType e = 0;

  IndexType index;
  index.Fill( 0 );

  for( PathIndexType i = 0; i < this->m_HilbertOrder; ++i )
    {
    PathIndexType w = this->GetBitRange( id, Dimension * this->m_HilbertOrder, i * Dimension, ( i + 1 ) * Dimension );
    PathIndexType l = this->GetGrayCode( w );
    l = this->GetInverseTransform( e, d, Dimension, l );
    for( PathIndexType j = 0; j < Dimension; ++j )
      {
      PathIndexType b = this->GetBitRange( l, Dimension, j, j + 1 );
      index[j] = this->SetBit( index[j], this->m_HilbertOrder, i, b );
      }
    e ^= this->GetLeftBitRotation( this->GetEntry( w ), d + 1, Dimension );
    d = ( d + this->GetDirection( w, Dimension ) + 1 ) % Dimension;
    }
  return index;
}

template<typename TIndexValue, unsigned int VDimension>
typename HilbertPath<TIndexValue, VDimension>::PathIndexType
HilbertPath<TIndexValue, VDimension>
::TransformMultiDimensionalIndexToPathIndex( const IndexType & index )
{
  PathIndexType id = 0;

  PathIndexType d = 0;
  PathIndexType e = 0;

  for( PathIndexType i = 0; i < this->m_HilbertOrder; ++i )
    {
    PathIndexType l = 0;
    for( PathIndexType j = 0; j < Dimension; ++j )
      {
      PathIndexType b = this->GetBitRange( index[Dimension - j - 1], this->m_HilbertOrder, i, i + 1 );
      l |= b << j;
      }
    l = this->GetTransform( e, d, Dimension, l );
    PathIndexType w = this->GetInverseGrayCode( l );
    e ^= this->GetLeftBitRotation( this->GetEntry( w ), d + 1, Dimension );
    d = ( d + this->GetDirection( w, Dimension ) + 1 ) % Dimension;
    id = ( id << Dimension ) | w;
    }
  return id;
}

template<typename TIndexValue, unsigned int VDimension>
typename HilbertPath<TIndexValue, VDimension>::PathIndexType
HilbertPath<TIndexValue, VDimension>
::GetTransform( const PathIndexType entry, const PathIndexType direction, const PathIndexType width, const PathIndexType x )
{
  return ( this->GetRightBitRotation( x ^ entry, direction + 1, width ) );
}

template<typename TIndexValue, unsigned int VDimension>
typename HilbertPath<TIndexValue, VDimension>::PathIndexType
HilbertPath<TIndexValue, VDimension>
::GetInverseTransform( const PathIndexType entry, const PathIndexType direction, const PathIndexType width, const PathIndexType x )
{
  return ( this->GetLeftBitRotation( x, direction + 1, width ) ^ entry );
}

template<typename TIndexValue, unsigned int VDimension>
typename HilbertPath<TIndexValue, VDimension>::PathIndexType
HilbertPath<TIndexValue, VDimension>
::GetBitRange( const PathIndexType x, const PathIndexType width, const PathIndexType start, const PathIndexType end )
{
  return ( x >> ( width - end ) & ( ( 1 << ( end - start ) ) - 1 ) );
}

template<typename TIndexValue, unsigned int VDimension>
typename HilbertPath<TIndexValue, VDimension>::PathIndexType
HilbertPath<TIndexValue, VDimension>
::GetLeftBitRotation( PathIndexType x, PathIndexType i, const PathIndexType width )
{
  return ( ( ( x << ( i % width ) ) | ( x >> ( width - ( i % width ) ) ) ) & ( ( 1 << width ) - 1 ) );
}

template<typename TIndexValue, unsigned int VDimension>
typename HilbertPath<TIndexValue, VDimension>::PathIndexType
HilbertPath<TIndexValue, VDimension>
::GetRightBitRotation( PathIndexType x, PathIndexType i, const PathIndexType width )
{
  return ( ( ( x >> ( i % width ) ) | ( x << ( width - ( i % width ) ) ) ) & ( ( 1 << width ) - 1 ) );
}

template<typename TIndexValue, unsigned int VDimension>
typename HilbertPath<TIndexValue, VDimension>::PathIndexType
HilbertPath<TIndexValue, VDimension>
::SetBit( const PathIndexType x, const PathIndexType width, const PathIndexType i, const PathIndexType b )
{
  if( b != 0 )
    {
    return ( x | ( 1 << ( width - i - 1 ) ) );
    }
  else
    {
    return ( x & ~( 1 << ( width - i - 1 ) ) );
    }
}

template<typename TIndexValue, unsigned int VDimension>
typename HilbertPath<TIndexValue, VDimension>::PathIndexType
HilbertPath<TIndexValue, VDimension>
::GetGrayCode( const PathIndexType x )
{
  return ( x ^ ( x >> 1 ) );
}

template<typename TIndexValue, unsigned int VDimension>
typename HilbertPath<TIndexValue, VDimension>::PathIndexType
HilbertPath<TIndexValue, VDimension>
::GetInverseGrayCode( const PathIndexType x )
{
  if( x == 0 )
    {
    return x;
    }

  PathIndexType m = static_cast<PathIndexType>( std::ceil( std::log( static_cast<double>(x) ) / std::log( 2.0 ) ) ) + 1;

  PathIndexType i = x;
  PathIndexType j = 1;

  while( j < m )
    {
    i ^= ( x >> j );
    ++j;
    }

  return i;
}

template<typename TIndexValue, unsigned int VDimension>
typename HilbertPath<TIndexValue, VDimension>::PathIndexType
HilbertPath<TIndexValue, VDimension>
::GetTrailingSetBits( const PathIndexType x, const PathIndexType width )
{
  PathIndexType i = 0;
  PathIndexType y = x;

  while( y & 1 && i <= width )
    {
    y >>= 1;
    ++i;
    }
  return i;
}

template<typename TIndexValue, unsigned int VDimension>
typename HilbertPath<TIndexValue, VDimension>::PathIndexType
HilbertPath<TIndexValue, VDimension>
::GetDirection( const PathIndexType x, const PathIndexType n )
{
  if( x == 0 )
    {
    return 0;
    }
  else if( x % 2 == 0 )
    {
    return ( this->GetTrailingSetBits( x - 1, n ) % n );
    }
  else
    {
    return ( this->GetTrailingSetBits( x, n ) % n );
    }
}

template<typename TIndexValue, unsigned int VDimension>
typename HilbertPath<TIndexValue, VDimension>::PathIndexType
HilbertPath<TIndexValue, VDimension>
::GetEntry( const PathIndexType x )
{
  if( x == 0 )
    {
    return 0;
    }
  else
    {
    return ( this->GetGrayCode( 2 * static_cast<PathIndexType>( ( x - 1 ) / 2 ) ) );
    }
}

/** Standard "PrintSelf" method */
template<typename TIndexValue, unsigned int VDimension>
void
HilbertPath<TIndexValue, VDimension>
::PrintSelf( std::ostream & os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );

  std::cout << "Hilbert order: " << this->m_HilbertOrder << std::endl;
}
} // end namespace itk

#endif
