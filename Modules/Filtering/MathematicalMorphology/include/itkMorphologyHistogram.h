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
#ifndef itkMorphologyHistogram_h
#define itkMorphologyHistogram_h

#include <map>
#include <vector>
#include "itkIntTypes.h"
#include "itkNumericTraits.h"

namespace itk
{
namespace Function
{
template< typename TInputPixel, typename TCompare >
class MorphologyHistogram
{
public:

  typedef typename std::map< TInputPixel, IdentifierType, TCompare > MapType;

  MorphologyHistogram(){}

  inline void AddBoundary()
  {
    m_Map[m_Boundary]++;
  }

  inline void RemoveBoundary()
  {
    m_Map[m_Boundary]--;
  }

  inline void AddPixel(const TInputPixel & p)
  {
    m_Map[p]++;
  }

  inline void RemovePixel(const TInputPixel & p)
  {
    m_Map[p]--;
  }

  inline TInputPixel GetValue()
  {
    itkAssertInDebugAndIgnoreInReleaseMacro(!m_Map.empty());
    // clean the map
    typename MapType::iterator mapIt = m_Map.begin();
    while ( mapIt != m_Map.end() )
      {
      if ( mapIt->second == 0 )
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
    itkAssertInDebugAndIgnoreInReleaseMacro(!m_Map.empty());
    return m_Map.begin()->first;
  }

  inline TInputPixel GetValue(const TInputPixel &)
  {
    return GetValue();
  }

  void SetBoundary(const TInputPixel & val)
  {
    m_Boundary = val;
  }

  static bool UseVectorBasedAlgorithm()
  {
    return false;
  }

  MapType     m_Map;
  TInputPixel m_Boundary;

};

template< typename TInputPixel, typename TCompare >
class VectorMorphologyHistogram
{
public:

  VectorMorphologyHistogram()
  {
    // initialize members need for the vector based algorithm
    m_Vector.resize(NumericTraits< TInputPixel >::max() - NumericTraits< TInputPixel >::NonpositiveMin() + 1 , 0);
    if ( m_Compare( NumericTraits< TInputPixel >::max(), NumericTraits< TInputPixel >::NonpositiveMin() ) )
      {
      m_InitValue = NumericTraits< TInputPixel >::NonpositiveMin();
      m_CurrentValue = m_InitValue;
      m_Direction = -1;
      }
    else
      {
      m_InitValue = NumericTraits< TInputPixel >::max();
      m_CurrentValue = m_InitValue;
      m_Direction = 1;
      }
    m_Boundary = 0;
  }

  inline void AddBoundary()
  {
    AddPixel(m_Boundary);
  }

  inline void RemoveBoundary()
  {
    RemovePixel(m_Boundary);
  }

  inline void AddPixel(const TInputPixel & p)
  {
    m_Vector[p - NumericTraits < TInputPixel > ::NonpositiveMin()]++;
    if ( m_Compare(p, m_CurrentValue) )
      {
      m_CurrentValue = p;
      }
  }

  inline void RemovePixel(const TInputPixel & p)
  {
    m_Vector[p - NumericTraits < TInputPixel > ::NonpositiveMin()]--;
    while ( m_Vector[m_CurrentValue - NumericTraits < TInputPixel > ::NonpositiveMin()] == 0
            && m_CurrentValue != m_InitValue )
      {
      m_CurrentValue += m_Direction;
      }
  }

  inline TInputPixel GetValue()
  {
    return m_CurrentValue;
  }

  inline TInputPixel GetValue(const TInputPixel &)
  {
    return GetValue();
  }

  void SetBoundary(const TInputPixel & val)
  {
    m_Boundary = val;
  }

  static bool UseVectorBasedAlgorithm()
  {
    return true;
  }

  std::vector< IdentifierType >   m_Vector;
  TInputPixel                     m_InitValue;
  TInputPixel                     m_CurrentValue;
  TCompare                        m_Compare;
  signed int                      m_Direction;
  TInputPixel                     m_Boundary;
};

/// \cond HIDE_SPECIALIZATION_DOCUMENTATION

// now create MorphologyHistogram partial specilizations using the VectorMorphologyHistogram
// as base class

template< typename TCompare >
class MorphologyHistogram<unsigned char, TCompare>:
  public VectorMorphologyHistogram<unsigned char, TCompare>
{
};

template< typename TCompare >
class MorphologyHistogram<signed char, TCompare>:
  public VectorMorphologyHistogram<signed char, TCompare>
{
};

template< typename TCompare >
class MorphologyHistogram<bool, TCompare>:
  public VectorMorphologyHistogram<bool, TCompare>
{
};

/// \endcond

} // end namespace Function
} // end namespace itk

#endif
