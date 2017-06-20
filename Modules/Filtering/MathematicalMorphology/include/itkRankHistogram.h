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
#ifndef itkRankHistogram_h
#define itkRankHistogram_h

#include "itkIntTypes.h"
#include "itkNumericTraits.h"

#include <map>
#include <vector>

namespace itk
{
namespace Function
{

/* \class RankHistogram
 * \brief A simple histogram class hierarchy. One specialization will
 * be maps, the other vectors.
 *
 * This version is intended for keeping track of arbitrary ranks. It
 * is based on the code from consolidatedMorphology.
 *
 * This is a modified version for use with masks. Need to allow for
 * the situation in which the map is empty.
 *
 * This code was contributed in the Insight Journal paper:
 * "Efficient implementation of kernel filtering"
 * by Beare R., Lehmann G
 * https://hdl.handle.net/1926/555
 * http://www.insight-journal.org/browse/publication/160
 *
 * /sa VectorRankHistogram
 */
template< typename TInputPixel >
class RankHistogram
{
public:

  typedef std::less< TInputPixel > Compare;

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
    m_RankIt = m_Map.begin();  // equivalent to setting to the initial value
  }

  ~RankHistogram()
  {}

  RankHistogram & operator=( const RankHistogram & hist )
  {
    if(this != &hist)
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

  bool IsValid()
  {
    return m_Initialized;
  }

  TInputPixel GetValueBruteForce()
  {
    SizeValueType count = 0;
    SizeValueType target = (int)( m_Rank * ( m_Entries - 1 ) ) + 1;
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
    SizeValueType target = (SizeValueType)( m_Rank * ( m_Entries - 1 ) ) + 1;
    SizeValueType total = m_Below;
    SizeValueType ThisBin;
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
      if (searchIt == m_Map.end())
        {
        --searchIt;
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
    itkAssertInDebugAndIgnoreInReleaseMacro( m_RankValue == GetValueBruteForce() );
    return ( m_RankValue );
  }

  void SetRank(float rank)
  {
    m_Rank = rank;
  }

  void AddBoundary(){}

  void RemoveBoundary(){}

  static bool UseVectorBasedAlgorithm()
  {
    return false;
  }

protected:
  float m_Rank;

private:
  typedef typename std::map< TInputPixel, SizeValueType, Compare > MapType;

  MapType       m_Map;
  SizeValueType m_Below;
  SizeValueType m_Entries;
  TInputPixel   m_RankValue;
  TInputPixel   m_InitVal;
  Compare       m_Compare;
  bool          m_Initialized;

  // This iterator will point at the desired rank value
  typename MapType::iterator m_RankIt;
};


template< typename TInputPixel >
class VectorRankHistogram
{
public:
  typedef std::less< TInputPixel > Compare;

  VectorRankHistogram()
  {
    m_Size = (OffsetValueType)NumericTraits< TInputPixel >::max() - (OffsetValueType)NumericTraits< TInputPixel >::NonpositiveMin() + 1;
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

  bool IsValid()
  {
    return m_Entries > 0;
  }

  TInputPixel GetValueBruteForce()
  {
    SizeValueType count = 0;
    SizeValueType target = (SizeValueType)( m_Rank * ( m_Entries - 1 ) ) + 1;
    for( SizeValueType i=0; i<m_Size; i++ )
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
  }

  void AddPixel(const TInputPixel & p)
  {
    OffsetValueType q = (OffsetValueType)p - NumericTraits< TInputPixel >::NonpositiveMin();

    m_Vec[q]++;
    if ( m_Compare(p, m_RankValue) || p == m_RankValue )
      {
      ++m_Below;
      }
    ++m_Entries;
  }

  void RemovePixel(const TInputPixel & p)
  {
    const OffsetValueType q = (OffsetValueType)p - NumericTraits< TInputPixel >::NonpositiveMin();

    itkAssertInDebugAndIgnoreInReleaseMacro( q >= 0 );
    itkAssertInDebugAndIgnoreInReleaseMacro( q < (int)m_Vec.size() );
    itkAssertInDebugAndIgnoreInReleaseMacro( m_Entries >= 1 );
    itkAssertInDebugAndIgnoreInReleaseMacro( m_Vec[q] > 0 );

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

  static bool UseVectorBasedAlgorithm()
  {
    return true;
  }

protected:
  float m_Rank;

private:
  typedef typename std::vector< SizeValueType > VecType;

  VecType       m_Vec;
  SizeValueType m_Size;
  Compare       m_Compare;
  TInputPixel   m_RankValue;
  TInputPixel   m_InitVal;
  int           m_Below;
  int           m_Entries;
};

// now create MorphologicalGradientHistogram specilizations using the VectorMorphologicalGradientHistogram
// as base class

/// \cond HIDE_SPECIALIZATION_DOCUMENTATION

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

/// \endcond

} // end namespace Function
} // end namespace itk
#endif
