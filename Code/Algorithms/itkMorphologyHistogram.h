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
#ifndef __itkMorphologyHistogram_h
#define __itkMorphologyHistogram_h

#include <map>

namespace itk
{
namespace Function
{
template< class TInputPixel, class TCompare >
class MorphologyHistogram
{
public:
  MorphologyHistogram()
  {
    m_UseVectorBasedAlgorithm = UseVectorBasedAlgorithm();
    if ( m_UseVectorBasedAlgorithm )
            { initVector(); }
  }

  ~MorphologyHistogram(){}

  MorphologyHistogram * Clone() const
  {
    MorphologyHistogram *result = new MorphologyHistogram();

    result->m_Map = this->m_Map;
    result->m_Vector = this->m_Vector;
    result->m_CurrentValue = this->m_CurrentValue;
    result->m_Compare = this->m_Compare;
    result->m_Direction = this->m_Direction;
    result->m_Boundary = this->m_Boundary;
    return result;
  }

  // define the method required by the functor and dispatch to the specialized
  // methods

  inline void AddBoundary()
  {
    if ( m_UseVectorBasedAlgorithm )
            { AddBoundaryVector(); }
    else
            { AddBoundaryMap(); }
  }

  inline void RemoveBoundary()
  {
    if ( m_UseVectorBasedAlgorithm )
            { RemoveBoundaryVector(); }
    else
            { RemoveBoundaryMap(); }
  }

  inline void AddPixel(const TInputPixel & p)
  {
    if ( m_UseVectorBasedAlgorithm )
            { AddPixelVector(p); }
    else
            { AddPixelMap(p); }
  }

  inline void RemovePixel(const TInputPixel & p)
  {
    if ( m_UseVectorBasedAlgorithm )
            { RemovePixelVector(p); }
    else
            { RemovePixelMap(p); }
  }

  inline TInputPixel GetValue()
  {
    if ( m_UseVectorBasedAlgorithm )
            { return GetValueVector(); }
    else
            { return GetValueMap(); }
  }

  inline TInputPixel GetValue(const TInputPixel &)
  {
    return GetValue();
  }

  inline static bool UseVectorBasedAlgorithm()
  {
    // bool, short and char are acceptable for vector based algorithm: they do
    // not require
    // too much memory. Other types are not usable with that algorithm
    return typeid( TInputPixel ) == typeid( unsigned char )
           || typeid( TInputPixel ) == typeid( signed char )
           || typeid( TInputPixel ) == typeid( unsigned short )
           || typeid( TInputPixel ) == typeid( signed short )
           || typeid( TInputPixel ) == typeid( bool );
  }

  bool m_UseVectorBasedAlgorithm;

  //
  // the map based algorithm
  //

  typedef typename std::map< TInputPixel, unsigned long, TCompare > MapType;

  inline void AddBoundaryMap()
  { m_Map[m_Boundary]++; }

  inline void RemoveBoundaryMap()
  { m_Map[m_Boundary]--; }

  inline void AddPixelMap(const TInputPixel & p)
  { m_Map[p]++; }

  inline void RemovePixelMap(const TInputPixel & p)
  { m_Map[p]--; }

  inline TInputPixel GetValueMap()
  {
    assert(!m_Map.empty());
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
    assert(!m_Map.empty());
    return m_Map.begin()->first;
  }

  MapType m_Map;

  //
  // the vector based algorithm
  //

  inline void initVector()
  {
    // initialize members need for the vector based algorithm
    m_Vector.resize(static_cast< int >( NumericTraits< TInputPixel >::max()
                                        - NumericTraits< TInputPixel >::NonpositiveMin() + 1 ), 0);
    if ( m_Compare( NumericTraits< TInputPixel >::max(), NumericTraits< TInputPixel >::NonpositiveMin() ) )
      {
      m_CurrentValue = NumericTraits< TInputPixel >::NonpositiveMin();
      m_Direction = -1;
      }
    else
      {
      m_CurrentValue = NumericTraits< TInputPixel >::max();
      m_Direction = 1;
      }
  }

  inline void AddBoundaryVector()
  { AddPixelVector(m_Boundary); }

  inline void RemoveBoundaryVector()
  { RemovePixelVector(m_Boundary); }

  inline void AddPixelVector(const TInputPixel & p)
  {
    m_Vector[static_cast< int >( p - NumericTraits < TInputPixel > ::NonpositiveMin() )]++;
    if ( m_Compare(p, m_CurrentValue) )
            { m_CurrentValue = p; }
  }

  inline void RemovePixelVector(const TInputPixel & p)
  {
    m_Vector[static_cast< int >( p - NumericTraits < TInputPixel > ::NonpositiveMin() )]--;
    while ( m_Vector[static_cast< int >( m_CurrentValue - NumericTraits < TInputPixel > ::NonpositiveMin() )] == 0 )
            { m_CurrentValue += m_Direction; }
  }

  inline TInputPixel GetValueVector()
  { return m_CurrentValue; }

  std::vector< unsigned long > m_Vector;
  TInputPixel                  m_CurrentValue;
  TCompare                     m_Compare;
  signed int                   m_Direction;

  // accessor for boundary value

  void SetBoundary(const TInputPixel & val)
  { m_Boundary = val; }

  TInputPixel m_Boundary;
};
} // end namespace Function
} // end namespace itk

#endif
