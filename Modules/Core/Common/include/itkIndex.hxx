#include "itkIndex.h"

namespace itk
{

template <unsigned int VIndexDimension>
std::ostream & operator<<(std::ostream & os, const Index<VIndexDimension> & ind)
{
  os << "[";
  for( unsigned int i = 0; i + 1 < VIndexDimension; ++i )
    {
    os << ind[i] << ", ";
    }
  if( VIndexDimension >= 1 )
    {
    os << ind[VIndexDimension - 1];
    }
  os << "]";
  return os;
}

template <unsigned int VIndexDimension>
unsigned int
Index<VIndexDimension>::GetIndexDimension()
{
  return VIndexDimension;
}

template <unsigned int VIndexDimension>
const typename Index<VIndexDimension>::Self
Index<VIndexDimension>::operator+(const SizeType & size) const
{
  Self result;

  for( unsigned int i = 0; i < VIndexDimension; i++ )
    {
    result[i] = m_Index[i] + static_cast<IndexValueType>( size[i] );
    }
  return result;
}

template <unsigned int VIndexDimension>
const typename Index<VIndexDimension>::Self &
Index<VIndexDimension>::operator+=(const SizeType & size)
{
  for( unsigned int i = 0; i < VIndexDimension; i++ )
    {
    m_Index[i] += static_cast<IndexValueType>( size[i] );
    }
  return *this;
}

template <unsigned int VIndexDimension>
const typename Index<VIndexDimension>::Self
Index<VIndexDimension>::operator-(const SizeType & size) const
{
  Self result;

  for( unsigned int i = 0; i < VIndexDimension; i++ )
    {
    result[i] = m_Index[i] - static_cast<IndexValueType>( size[i] );
    }
  return result;
}

template <unsigned int VIndexDimension>
const typename Index<VIndexDimension>::Self &
Index<VIndexDimension>::operator-=(const SizeType & size)
{
  for( unsigned int i = 0; i < VIndexDimension; i++ )
    {
    m_Index[i] -= static_cast<IndexValueType>( size[i] );
    }
  return *this;
}

template <unsigned int VIndexDimension>
const typename Index<VIndexDimension>::Self
Index<VIndexDimension>::operator+(const OffsetType & offset) const
{
  Self result;

  for( unsigned int i = 0; i < VIndexDimension; i++ )
    {
    result[i] = m_Index[i] + offset[i];
    }
  return result;
}

template <unsigned int VIndexDimension>
const typename Index<VIndexDimension>::Self &
Index<VIndexDimension>::operator+=(const OffsetType & offset)
{
  for( unsigned int i = 0; i < VIndexDimension; i++ )
    {
    m_Index[i] += offset[i];
    }
  return *this;
}

template <unsigned int VIndexDimension>
const typename Index<VIndexDimension>::Self &
Index<VIndexDimension>::operator-=(const OffsetType & offset)
{
  for( unsigned int i = 0; i < VIndexDimension; i++ )
    {
    m_Index[i] -= offset[i];
    }
  return *this;
}

template <unsigned int VIndexDimension>
const typename Index<VIndexDimension>::Self
Index<VIndexDimension>::operator-(const OffsetType & off) const
{
  Self result;

  for( unsigned int i = 0; i < VIndexDimension; i++ )
    {
    result[i] = m_Index[i] - off.m_Offset[i];
    }
  return result;
}

template <unsigned int VIndexDimension>
const typename Index<VIndexDimension>::OffsetType
Index<VIndexDimension>::operator-(const Self & vec) const
{
  OffsetType result;

  for( unsigned int i = 0; i < VIndexDimension; i++ )
    {
    result[i] = m_Index[i] - vec.m_Index[i];
    }
  return result;
}

template <unsigned int VIndexDimension>
const typename Index<VIndexDimension>::Self
Index<VIndexDimension>::operator*(const SizeType & vec) const
{
  Self result;

  for( unsigned int i = 0; i < VIndexDimension; i++ )
    {
    result[i] = m_Index[i] * static_cast<IndexValueType>( vec.m_Size[i] );
    }
  return result;
}

template <unsigned int VIndexDimension>
bool
Index<VIndexDimension>::operator==(const Self & vec) const
{
  bool same = true;

  for( unsigned int i = 0; i < VIndexDimension && same; i++ )
    {
    same = ( m_Index[i] == vec.m_Index[i] );
    }
  return same;
}

template <unsigned int VIndexDimension>
bool
Index<VIndexDimension>::operator!=(const Self & vec) const
{
  bool same = true;

  for( unsigned int i = 0; i < VIndexDimension && same; i++ )
    {
    same = ( m_Index[i] == vec.m_Index[i] );
    }
  return !same;
}

template <unsigned int VIndexDimension>
typename Index<VIndexDimension>::IndexValueType &
Index<VIndexDimension>::operator[](const unsigned int dim)
{
  return m_Index[dim];
}

template <unsigned int VIndexDimension>
typename Index<VIndexDimension>::IndexValueType
Index<VIndexDimension>::operator[](const unsigned int dim) const
{
  return m_Index[dim];
}

template <unsigned int VIndexDimension>
const typename Index<VIndexDimension>::IndexValueType
* Index<VIndexDimension>::GetIndex() const
  {
  return m_Index;
  }

template <unsigned int VIndexDimension>
void
Index<VIndexDimension>::SetIndex(const IndexValueType val[VIndexDimension])
{
  memcpy(m_Index, val, sizeof( IndexValueType ) * VIndexDimension);
}

template <unsigned int VIndexDimension>
void
Index<VIndexDimension>::SetElement(const unsigned long element, const IndexValueType val)
{
  m_Index[element] = val;
}

template <unsigned int VIndexDimension>
typename Index<VIndexDimension>::IndexValueType
Index<VIndexDimension>::GetElement(const unsigned long element) const
{
  return m_Index[element];
}

template <unsigned int VIndexDimension>
typename Index<VIndexDimension>::Self
Index<VIndexDimension>::GetBasisIndex(const unsigned int dim)
{
  Self ind;

  memset(ind.m_Index, 0, sizeof( IndexValueType ) * VIndexDimension);
  ind.m_Index[dim] = 1;
  return ind;
}

template <unsigned int VIndexDimension>
void
Index<VIndexDimension>::Fill(const IndexValueType value)
{
  for( unsigned int i = 0; i < VIndexDimension; ++i )
    {
    m_Index[i] = value;
    }
}

namespace Functor
{
template <unsigned int VIndexDimension>
bool
IndexLexicographicCompare<VIndexDimension>
::operator()(Index<VIndexDimension> const & l,
             Index<VIndexDimension> const & r) const
{
  for( unsigned int i = 0; i < VIndexDimension; ++i )
    {
    if( l.m_Index[i] < r.m_Index[i] )
      {
      return true;
      }
    else if( l.m_Index[i] > r.m_Index[i] )
      {
      return false;
      }
    }
  return false;
}

} // End namepace Functor
} // End namespace itk
