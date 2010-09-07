/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMaskedRankHistogram.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

    This software is distributed WITHOUT ANY WARRANTY; without even
    the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
    PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

// histogram from the moving histogram operations
#ifndef __itkMaskedRankHistogram_h
#define __itkMaskedRankHistogram_h
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
class MaskedRankHistogram
{
public:
  MaskedRankHistogram()
  {
    m_Rank = 0.5;
  }

  virtual ~MaskedRankHistogram(){}

  virtual MaskedRankHistogram * Clone() const { return 0; }

  virtual void Reset(){}

  virtual void AddPixel( const TInputPixel & itkNotUsed(p) ){}

  virtual void RemovePixel( const TInputPixel & itkNotUsed(p) ){}

  // For the map based version - to be called after there is some data
  // included. Meant to be an optimization so that the rank value
  // iterator is properly set.
  // virtual void Initialize(){};

  virtual bool IsValid()
  {
    return false;
  }

  virtual TInputPixel GetValue(const TInputPixel &)
  {
    return NumericTraits< TInputPixel >::Zero;
  }

  void SetRank(float rank)
  {
    m_Rank = rank;
  }

  void AddBoundary(){}

  void RemoveBoundary(){}
protected:
  float m_Rank;
};

template< class TInputPixel, class TCompare >
class MaskedRankHistogramMap:public MaskedRankHistogram< TInputPixel >
{
public:

  typedef MaskedRankHistogram< TInputPixel > Superclass;
public:
  MaskedRankHistogramMap()
  {
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

  ~MaskedRankHistogramMap()
  {}

  Superclass * Clone() const
  {
    MaskedRankHistogramMap *result = new MaskedRankHistogramMap();

    result->m_Map = this->m_Map;
    result->m_Rank = this->m_Rank;
    result->m_Below = this->m_Below;
    result->m_Entries = this->m_Entries;
    result->m_InitVal = this->m_InitVal;
    result->m_RankValue = this->m_RankValue;
    result->m_Initialized = this->m_Initialized;
    if ( result->m_Initialized )
      {
      result->m_RankIt = result->m_Map.find(this->m_RankValue);
      }
    return ( result );
  }

  void Reset()
  {
    m_Map.clear();
    m_RankValue = m_InitVal;
    m_Entries = m_Below = 0;
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

//   void Initialize()
//   {
//     m_RankIt = m_Map.begin();
//   }

  virtual bool IsValid()
  {
    return m_Initialized;
  }

  TInputPixel GetValue(const TInputPixel &)
  {
    unsigned long target = (int)( this->m_Rank * ( m_Entries - 1 ) ) + 1;
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
    return ( m_RankValue );
  }

#if 0
  TInputPixel GetValue(const TInputPixel &)
  {      // clean the map
    typename MapType::iterator mapIt = m_Map.begin();
    while ( mapIt != m_Map.end() )
      {
      if ( mapIt->second <= 0 )
        {
        // this value must be removed from the histogram
        // The value must be stored and the iterator updated before removing the
        // value
        // or the iterator is invalidated.
        TInputPixel toErase = mapIt->first;
        mapIt++;
        m_Map.erase(toErase);
        }
      else
        {
        mapIt++;
        // don't remove all the zero value found, just remove the one before the
        // current maximum value
        // the histogram may become quite big on real type image, but it's an
        // important increase of performances
        break;
        }
      }

    // and return the value
    return m_Map.begin()->first;
  }

#endif
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

template< class TInputPixel, class TCompare >
class MaskedRankHistogramVec:public MaskedRankHistogram< TInputPixel >
{
public:

  typedef MaskedRankHistogram< TInputPixel > Superclass;
public:

  MaskedRankHistogramVec()
  {
    m_Size = static_cast< unsigned int >( NumericTraits< TInputPixel >::max()
                                          - NumericTraits< TInputPixel >::NonpositiveMin() + 1 );
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
  }

  MaskedRankHistogramVec(bool NoInit)
  {
    m_Size = static_cast< unsigned int >( NumericTraits< TInputPixel >::max()
                                          - NumericTraits< TInputPixel >::NonpositiveMin() + 1 );
    if ( !NoInit ) { m_Vec.resize(m_Size, 0); }
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
  }

  ~MaskedRankHistogramVec()
  {}

  Superclass * Clone() const
  {
    MaskedRankHistogramVec *result = new MaskedRankHistogramVec(true);

    result->m_Vec = this->m_Vec;
    result->m_Size = this->m_Size;
    //result->m_CurrentValue = this->m_CurrentValue;
    result->m_InitVal = this->m_InitVal;
    result->m_Entries = this->m_Entries;
    result->m_Below = this->m_Below;
    result->m_Rank = this->m_Rank;
    result->m_RankValue = this->m_RankValue;
    return ( result );
  }

  virtual bool IsValid()
  {
    return m_Entries > 0;
  }

  TInputPixel GetValue(const TInputPixel &)
  {
    unsigned long     target = (int)( this->m_Rank * ( m_Entries - 1 ) ) + 1;
    unsigned long     total = m_Below;
    long unsigned int pos = (long unsigned int)( m_RankValue - NumericTraits< TInputPixel >::NonpositiveMin() );

    if ( total < target )
      {
      while ( pos < m_Size )
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
        unsigned int tbelow = total - m_Vec[pos];
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
    return ( m_RankValue );
  }

  void Reset()
  {
    std::fill(&( m_Vec[0] ), &( m_Vec[m_Size - 1] ), 0);
    m_RankValue = m_InitVal;
    m_Entries = m_Below = 0;
  }

  void AddPixel(const TInputPixel & p)
  {
    long unsigned int idx = (long unsigned int)( p - NumericTraits< TInputPixel >::NonpositiveMin() );

    m_Vec[idx]++;
    if ( m_Compare(p, m_RankValue) || p == m_RankValue )
      {
      ++m_Below;
      }
    ++m_Entries;
  }

  void RemovePixel(const TInputPixel & p)
  {
    const long int q = p - NumericTraits< TInputPixel >::NonpositiveMin();

    itkAssertOrThrowMacro( ( q >= 0 ), "Input pixel value is out of range" );
    itkAssertOrThrowMacro( ( q < (int)m_Vec.size() ), "Input pixel value is out of range" );
    itkAssertOrThrowMacro( ( m_Entries >= 1 ), "Insufficient entries" );

    m_Vec[static_cast< long unsigned int >( q )]--;
    --m_Entries;

    if ( m_Compare(p, m_RankValue) || p == m_RankValue )
      {
      --m_Below;
      }
  }

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
} // end namespace itk
#endif
