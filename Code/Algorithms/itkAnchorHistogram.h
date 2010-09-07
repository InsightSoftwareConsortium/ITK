/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAnchorHistogram.h
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
#ifndef __itkAnchorHistogram_h
#define __itkAnchorHistogram_h
#include "itkNumericTraits.h"
#include <vector>
#include <map>
#include "itkIndent.h"
namespace itk
{
// a simple histogram class hierarchy. One subclass will be maps, the
// other vectors
template< class TInputPixel >
class MorphologyHistogram
{
public:
  MorphologyHistogram() {}
  virtual ~MorphologyHistogram(){}

  virtual void Reset(){}

  virtual void AddBoundary(){}

  virtual void RemoveBoundary(){}

  virtual void AddPixel(const TInputPixel &){}

  virtual void RemovePixel(const TInputPixel &){}

  virtual TInputPixel GetValue()
  {
    return ( m_Boundary );  // a dummy return
  }

  void SetBoundary(const TInputPixel & val)
  {
    m_Boundary = val;
  }

protected:
  TInputPixel m_Boundary;
};

template< class TInputPixel, class TCompare >
class MorphologyHistogramMap:public MorphologyHistogram< TInputPixel >
{
private:
  typedef typename std::map< TInputPixel, unsigned long, TCompare > MapType;

  MapType m_Map;
public:
  MorphologyHistogramMap()
  {}

  ~MorphologyHistogramMap(){}

  void Reset()
  {
    m_Map.clear();
  }

  void AddBoundary()
  {
    m_Map[this->m_Boundary]++;
  }

  void RemoveBoundary()
  {
    m_Map[this->m_Boundary]--;
  }

  void AddPixel(const TInputPixel & p)
  {
    m_Map[p]++;
  }

  void RemovePixel(const TInputPixel & p)
  {
    m_Map[p]--;
  }

  TInputPixel GetValue()
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
};

template< class TInputPixel, class TCompare >
class MorphologyHistogramVec:public MorphologyHistogram< TInputPixel >
{
public:
  MorphologyHistogramVec()
  {
    m_Size = static_cast< unsigned int >( NumericTraits< TInputPixel >::max()
                                          - NumericTraits< TInputPixel >::NonpositiveMin() + 1 );
    m_Vec.resize(m_Size, 0);
    std::fill(m_Vec.begin(), m_Vec.end(), 0);
    if ( m_Compare( NumericTraits< TInputPixel >::max(),
                    NumericTraits< TInputPixel >::NonpositiveMin() ) )
      {
      m_CurrentValue = m_InitVal = NumericTraits< TInputPixel >::NonpositiveMin();
      m_Direction = -1;
      }
    else
      {
      m_CurrentValue = m_InitVal = NumericTraits< TInputPixel >::max();
      m_Direction = 1;
      }
    m_Entries = 0;
  }

  ~MorphologyHistogramVec(){}

  void Reset()
  {
    m_CurrentValue = m_InitVal;
    if ( m_Entries != 0 )
      {
      std::fill(m_Vec.begin(), m_Vec.end(), 0);
      m_Entries = 0;
      }
  }

  void AddBoundary()
  {
    AddPixel(this->m_Boundary);
    ++m_Entries;
  }

  void RemoveBoundary()
  {
    RemovePixel(this->m_Boundary);
    --m_Entries;
  }

  void AddPixel(const TInputPixel & p)
  {
    m_Vec[(long unsigned int)( p - NumericTraits < TInputPixel > ::NonpositiveMin() )]++;
    if ( m_Compare(p, m_CurrentValue) )
      {
      m_CurrentValue = p;
      }
    ++m_Entries;
  }

  void RemovePixel(const TInputPixel & p)
  {
    assert( (int)p - (int)NumericTraits< TInputPixel >::NonpositiveMin() >= 0 );
    assert( ( (int)p - (int)NumericTraits< TInputPixel >::NonpositiveMin() ) < (int)m_Vec.size() );
    assert(m_Entries >= 1);
    m_Vec[(long unsigned int)( p - NumericTraits < TInputPixel > ::NonpositiveMin() )]--;
    --m_Entries;
    assert(static_cast< int >( (int)m_CurrentValue
                               - (int)NumericTraits< TInputPixel >::NonpositiveMin() ) >= 0);
    assert( static_cast< int >( (int)m_CurrentValue
                                - (int)NumericTraits< TInputPixel >::NonpositiveMin() ) < (int)m_Vec.size() );
    if ( m_Entries > 0 )
      {
      while ( m_Vec[static_cast< int >( m_CurrentValue
                                        - NumericTraits < TInputPixel > ::NonpositiveMin() )] == 0 )
        {
        m_CurrentValue += m_Direction;
        assert(static_cast< int >( m_CurrentValue
                                   - NumericTraits< TInputPixel >::NonpositiveMin() ) >= 0);
        assert( static_cast< int >( m_CurrentValue
                                    - NumericTraits< TInputPixel >::NonpositiveMin() ) < (int)m_Vec.size() );
        }
      }
  }

  TInputPixel GetValue()
  {
    assert(m_Entries > 0);
    return ( m_CurrentValue );
  }

private:
  typedef typename std::vector< unsigned long > VecType;

  VecType      m_Vec;
  unsigned int m_Size;
  TCompare     m_Compare;
  TInputPixel  m_CurrentValue;
  TInputPixel  m_InitVal;
  int          m_Direction;
  int          m_Entries;
};
} // end namespace itk
#endif
