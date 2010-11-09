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
// histogram from the moving histogram operations
#ifndef __itkRankHistogram_h
#define __itkRankHistogram_h
#include "itkNumericTraits.h"

namespace itk
{
// a simple histogram class hierarchy. One subclass will be maps, the
// other vectors.
// This version is intended for keeping track of arbitary ranks. It is
// based on the code from consolidatedMorphology.
//
// Support for different TCompare hasn't been tested, and shouldn't be
// necessary for the rank filters.
//
// This is a modified version for use with masks. Need to allow for
// the situation in which the map is empty

template< class TInputPixel >
class RankHistogram
{
public:

  typedef std::less< TInputPixel > TCompare;

  RankHistogram()
  {
    m_Rank = 0.5;
    m_Below = m_Entries = 0;
    // can't set m_RankIt until something has been put in the histogram
    m_Initialized = false;
    if ( m_Compare( NumericTraits< TInputPixel >::max(),
                    NumericTraits< TInputPixel >::NonpositiveMin() ) )
      {
      m_InitVal = NumericTraits< TInputPixel >::max();
      }
    else
      {
      m_InitVal = NumericTraits< TInputPixel >::NonpositiveMin();
      }
    m_RankValue = m_InitVal;
    m_RankIt = m_Map.begin();  // equivalent to setting to the intial value
  }

  ~RankHistogram()
  {}

  RankHistogram & operator=( const RankHistogram & hist )
  {
    m_Map = hist.m_Map;
    m_Rank = hist.m_Rank;
    m_Below = hist.m_Below;
    m_Entries = hist.m_Entries;
    m_InitVal = hist.m_InitVal;
    m_RankValue = hist.m_RankValue;
    m_Initialized = hist.m_Initialized;
    if ( m_Initialized )
      {
      m_RankIt = m_Map.find(m_RankValue);
      }
    return *this;
  }

  void AddPixel(const TInputPixel & p)
  {
    m_Map[p]++;
    if ( !m_Initialized )
      {
      m_Initialized = true;
      m_RankIt = m_Map.begin();
      m_Entries = m_Below = 0;
      m_RankValue = p;
      }
    if ( m_Compare(p, m_RankValue) || p == m_RankValue )
      {
      ++m_Below;
      }
    ++m_Entries;
  }

  void RemovePixel(const TInputPixel & p)
  {
    m_Map[p]--;
    if ( m_Compare(p, m_RankValue) || p == m_RankValue )
      {
      --m_Below;
      }
    --m_Entries;
    // this is the change that makes this version less efficient. The
    // simplest approach I can think of with maps, though
    if ( m_Entries <= 0 )
      {
      m_Initialized = false;
      m_Below = 0;
      m_Map.clear();
      }
  }

  virtual bool IsValid()
  {
    return m_Initialized;
  }

  TInputPixel GetValueBruteForce()
  {
    unsigned long count = 0;
    unsigned long target = (int)( m_Rank * ( m_Entries - 1 ) ) + 1;
    for( typename MapType::iterator it=m_Map.begin(); it != m_Map.end(); it++ )
      {
      count += it->second;
      if( count >= target )
        {
        return it->first;
        }
      }
    return NumericTraits< TInputPixel >::max();
  }

  TInputPixel GetValue(const TInputPixel &)
  {
    unsigned long target = (unsigned long)( m_Rank * ( m_Entries - 1 ) ) + 1;
    unsigned long total = m_Below;
    unsigned long ThisBin;
    bool          eraseFlag = false;

    if ( total < target )
      {
      typename MapType::iterator searchIt = m_RankIt;
      typename MapType::iterator eraseIt;

      while ( searchIt != m_Map.end() )
        {
        // cleaning up the map - probably a better way of organising
        // the loop. Currently makes sure that the search iterator is
        // incremented before deleting
        ++searchIt;
        ThisBin = searchIt->second;
        total += ThisBin;
        if ( eraseFlag )
          {
          m_Map.erase(eraseIt);
          eraseFlag = false;
          }
        if ( ThisBin <= 0 )
          {
          eraseFlag = true;
          eraseIt = searchIt;
          }
        if ( total >= target )
          {
          break;
          }
        }
      m_RankValue = searchIt->first;
      m_RankIt = searchIt;
      }
    else
      {
      typename MapType::iterator searchIt = m_RankIt;
      typename MapType::iterator eraseIt;

      while ( searchIt != m_Map.begin() )
        {
        ThisBin = searchIt->second;
        unsigned int tbelow = total - ThisBin;
        if ( tbelow < target ) // we've overshot
          {
          break;
          }
        if ( eraseFlag )
          {
          m_Map.erase(eraseIt);
          eraseFlag = false;
          }
        if ( ThisBin <= 0 )
          {
          eraseIt = searchIt;
          eraseFlag = true;
          }
        total = tbelow;

        --searchIt;
        }
      m_RankValue = searchIt->first;
      m_RankIt = searchIt;
      }

    m_Below = total;
    assert( m_RankValue == GetValueBruteForce() );
    return ( m_RankValue );
  }

  void SetRank(float rank)
  {
    m_Rank = rank;
  }

  void AddBoundary(){}

  void RemoveBoundary(){}

protected:
  float m_Rank;

private:
  typedef typename std::map< TInputPixel, unsigned long, TCompare > MapType;

  MapType       m_Map;
  unsigned long m_Below;
  unsigned long m_Entries;
  TInputPixel   m_RankValue;
  TInputPixel   m_InitVal;
  TCompare      m_Compare;
  bool          m_Initialized;
  // This iterator will point at the desired rank value
  typename MapType::iterator m_RankIt;
};


template< class TInputPixel >
class VectorRankHistogram
{
public:

  typedef std::less< TInputPixel > TCompare;

  VectorRankHistogram()
  {
    m_Size = (long)NumericTraits< TInputPixel >::max() - (long)NumericTraits< TInputPixel >::NonpositiveMin() + 1;
    m_Vec.resize(m_Size, 0);
    if ( m_Compare( NumericTraits< TInputPixel >::max(),
                    NumericTraits< TInputPixel >::NonpositiveMin() ) )
      {
      m_InitVal = NumericTraits< TInputPixel >::NonpositiveMin();
      }
    else
      {
      m_InitVal = NumericTraits< TInputPixel >::max();
      }
    m_Entries = m_Below = 0;
    m_RankValue = m_InitVal  - NumericTraits< TInputPixel >::NonpositiveMin();
    m_Rank = 0.5;
  }

  ~VectorRankHistogram() {}

  virtual bool IsValid()
  {
    return m_Entries > 0;
  }

  TInputPixel GetValueBruteForce()
  {
    unsigned long count = 0;
    unsigned long target = (unsigned long)( m_Rank * ( m_Entries - 1 ) ) + 1;
    for( unsigned long i=0; i<m_Size; i++ )
      {
      count += m_Vec[i];
      if( count >= target )
        {
        return i + NumericTraits< TInputPixel >::NonpositiveMin();
        }
      }
    return NumericTraits< TInputPixel >::max();
  }

  TInputPixel GetValue(const TInputPixel &)
  {
    return GetValueBruteForce();
    unsigned long     target = (unsigned long)( this->m_Rank * ( m_Entries - 1 ) ) + 1;
    unsigned long     total = m_Below;
    unsigned long     pos = (long)m_RankValue - NumericTraits< TInputPixel >::NonpositiveMin();

    if ( total < target )
      {
      while ( pos < m_Size - 1 )
        {
        ++pos;
        total += m_Vec[pos];
        if ( total >= target )
          {
          break;
          }
        }
      }
    else
      {
      while ( pos > 0 )
        {
        unsigned long tbelow = total - m_Vec[pos];
        if ( tbelow < target ) // we've overshot
          {
          break;
          }
        total = tbelow;
        --pos;
        }
      }

    m_RankValue = (TInputPixel)( pos + NumericTraits< TInputPixel >::NonpositiveMin() );
    m_Below = total;
    // std::cout << m_RankValue+0.0 << "  " << GetValueBruteForce(0)+0.0 << std::endl;
    assert( m_RankValue == GetValueBruteForce() );
    return ( m_RankValue );
  }

  void AddPixel(const TInputPixel & p)
  {
    long q = (long)p - NumericTraits< TInputPixel >::NonpositiveMin();

    m_Vec[q]++;
    if ( m_Compare(p, m_RankValue) || p == m_RankValue )
      {
      ++m_Below;
      }
    ++m_Entries;
  }

  void RemovePixel(const TInputPixel & p)
  {
    const long q = (long)p - NumericTraits< TInputPixel >::NonpositiveMin();

    assert( q >= 0 );
    assert( q < (int)m_Vec.size() );
    assert( m_Entries >= 1 );
    assert( m_Vec[q] > 0 );

    m_Vec[q]--;
    --m_Entries;

    if ( m_Compare(p, m_RankValue) || p == m_RankValue )
      {
      --m_Below;
      }
  }

  void SetRank(float rank)
  {
    m_Rank = rank;
  }

  void AddBoundary(){}

  void RemoveBoundary(){}

protected:
  float m_Rank;

private:
  typedef typename std::vector< unsigned long > VecType;

  VecType      m_Vec;
  unsigned int m_Size;
  TCompare     m_Compare;
  TInputPixel  m_RankValue;
  TInputPixel  m_InitVal;
  int          m_Below;
  int          m_Entries;
};

// now create MorphologicalGradientHistogram specilizations using the VectorMorphologicalGradientHistogram
// as base class

template<>
class RankHistogram<unsigned char>:
  public VectorRankHistogram<unsigned char>
{
};

template<>
class RankHistogram<signed char>:
  public VectorRankHistogram<signed char>
{
};

template<>
class RankHistogram<bool>:
  public VectorRankHistogram<bool>
{
};

} // end namespace itk
#endif
