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
#ifndef itkMovingHistogramMorphologicalGradientImageFilter_h
#define itkMovingHistogramMorphologicalGradientImageFilter_h

#include "itkMovingHistogramImageFilter.h"
#include <map>

namespace itk
{
namespace Function
{
template< typename TInputPixel >
class MorphologicalGradientHistogram
{
public:
  MorphologicalGradientHistogram()
  {
  }

  ~MorphologicalGradientHistogram(){}

  inline void AddBoundary() {}

  inline void RemoveBoundary() {}

  typedef std::map< TInputPixel, SizeValueType > MapType;

  inline void AddPixel(const TInputPixel & p)
  {
    m_Map[p]++;
  }

  inline void RemovePixel(const TInputPixel & p)
  {
    m_Map[p]--;
  }

  inline TInputPixel GetValue(const TInputPixel &)
  {
    return GetValue();
  }

  inline TInputPixel GetValue()
  {
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
        }
      }

    // and return the value
    if( !m_Map.empty() )
      {
      return m_Map.rbegin()->first - m_Map.begin()->first;
      }
    return 0;
  }

  static bool UseVectorBasedAlgorithm()
  {
    return false;
  }

  MapType m_Map;
};


template< typename TInputPixel >
class VectorMorphologicalGradientHistogram
{
public:
  VectorMorphologicalGradientHistogram()
  {
    // initialize members need for the vector based algorithm
    m_Vector.resize(NumericTraits< TInputPixel >::max() - NumericTraits< TInputPixel >::NonpositiveMin() + 1, 0);
    m_Max = NumericTraits< TInputPixel >::NonpositiveMin();
    m_Min = NumericTraits< TInputPixel >::max();
    m_Count = 0;
  }

  ~VectorMorphologicalGradientHistogram(){}

  inline void AddBoundary() {}

  inline void RemoveBoundary() {}


  inline void AddPixel(const TInputPixel & p)
  {
    m_Vector[p - NumericTraits < TInputPixel > ::NonpositiveMin()]++;
    if ( p > m_Max )
      {
      m_Max = p;
      }
    if ( p < m_Min )
      {
      m_Min = p;
      }
    m_Count++;
  }

  inline void RemovePixel(const TInputPixel & p)
  {
    m_Vector[p - NumericTraits < TInputPixel > ::NonpositiveMin()]--;
    m_Count--;
    if ( m_Count > 0 )
      {
      while ( m_Vector[m_Max - NumericTraits < TInputPixel > ::NonpositiveMin()] == 0 )
        {
        m_Max--;
        }
      while ( m_Vector[m_Min - NumericTraits < TInputPixel > ::NonpositiveMin()] == 0 )
        {
        m_Min++;
        }
      }
    else
      {
      m_Max = NumericTraits< TInputPixel >::NonpositiveMin();
      m_Min = NumericTraits< TInputPixel >::max();
      }
  }

  inline TInputPixel GetValue(const TInputPixel &)
  {
    return GetValue();
  }

  inline TInputPixel GetValue()
  {
    if ( m_Count > 0 )
      {
      return m_Max - m_Min;
      }
    else
      {
      return NumericTraits< TInputPixel >::ZeroValue();
      }
  }

  static bool UseVectorBasedAlgorithm()
  {
    return true;
  }

  std::vector< SizeValueType > m_Vector;
  TInputPixel                  m_Min;
  TInputPixel                  m_Max;
  SizeValueType                m_Count;
};

/// \cond HIDE_SPECIALIZATION_DOCUMENTATION

// now create MorphologicalGradientHistogram specilizations using the VectorMorphologicalGradientHistogram
// as base class

template<>
class MorphologicalGradientHistogram<unsigned char>:
  public VectorMorphologicalGradientHistogram<unsigned char>
{
};

template<>
class MorphologicalGradientHistogram<signed char>:
  public VectorMorphologicalGradientHistogram<signed char>
{
};

template<>
class MorphologicalGradientHistogram<bool>:
  public VectorMorphologicalGradientHistogram<bool>
{
};

/// \endcond

} // end namespace Function

/**
 * \class MovingHistogramMorphologicalGradientImageFilter
 * \brief Morphological gradients enhance the variation of pixel
 * intensity in a given neighborhood.
 *
 * Morphological gradient is described in Chapter 3.8.1 of Pierre
 * Soille's book  "Morphological Image Analysis: Principles and
 * Applications", Second Edition, Springer, 2003.
 *
 * \author Gaetan Lehmann. Biologie du Developpement et de la Reproduction, INRA de Jouy-en-Josas, France.
 *
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKMathematicalMorphology
 */

template< typename TInputImage, typename TOutputImage, typename TKernel >
class MovingHistogramMorphologicalGradientImageFilter:
  public MovingHistogramImageFilter< TInputImage, TOutputImage, TKernel,
                                     typename  Function::MorphologicalGradientHistogram< typename TInputImage::
                                                                                         PixelType > >
{
public:
  /** Standard class typedefs. */
  typedef MovingHistogramMorphologicalGradientImageFilter Self;
  typedef MovingHistogramImageFilter< TInputImage, TOutputImage, TKernel,
                                      typename  Function::MorphologicalGradientHistogram< typename TInputImage::
                                                                                          PixelType > >  Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(MovingHistogramMorphologicalGradientImageFilter,
               ImageToImageFilter);

  /** Image related typedefs. */
  typedef TInputImage                                InputImageType;
  typedef TOutputImage                               OutputImageType;
  typedef typename TInputImage::RegionType           RegionType;
  typedef typename TInputImage::SizeType             SizeType;
  typedef typename TInputImage::IndexType            IndexType;
  typedef typename TInputImage::PixelType            PixelType;
  typedef typename TInputImage::OffsetType           OffsetType;
  typedef typename Superclass::OutputImageRegionType OutputImageRegionType;
  typedef typename TOutputImage::PixelType           OutputPixelType;

  typedef Function::MorphologicalGradientHistogram< PixelType > HistogramType;

  /** Image related typedefs. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Return true if the vector based algorithm is used, and
   * false if the map based algorithm is used */
  static bool GetUseVectorBasedAlgorithm()
  { return Function::MorphologicalGradientHistogram< typename TInputImage::PixelType >::UseVectorBasedAlgorithm(); }

protected:
  MovingHistogramMorphologicalGradientImageFilter() {}
  ~MovingHistogramMorphologicalGradientImageFilter() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MovingHistogramMorphologicalGradientImageFilter);
};                                                               // end of class
} // end namespace itk

#endif
