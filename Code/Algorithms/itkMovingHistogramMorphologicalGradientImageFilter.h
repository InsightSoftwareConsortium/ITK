/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMovingHistogramMorphologicalGradientImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMovingHistogramMorphologicalGradientImageFilter_h
#define __itkMovingHistogramMorphologicalGradientImageFilter_h

#include "itkMovingHistogramImageFilter.h"
#include <map>

namespace itk
{
namespace Function
{
template< class TInputPixel >
class MorphologicalGradientHistogram
{
public:
  MorphologicalGradientHistogram()
  {
    m_UseVectorBasedAlgorithm = UseVectorBasedAlgorithm();
    if ( m_UseVectorBasedAlgorithm )
            { initVector(); }
  }

  ~MorphologicalGradientHistogram(){}

  MorphologicalGradientHistogram * Clone() const
  {
    MorphologicalGradientHistogram *result = new MorphologicalGradientHistogram();

    result->m_Map = this->m_Map;
    result->m_Vector = this->m_Vector;
    result->m_Min = this->m_Min;
    result->m_Max = this->m_Max;
    result->m_Count = this->m_Count;
    return result;
  }

  inline void AddBoundary() {}

  inline void RemoveBoundary() {}

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

  inline TInputPixel GetValue(const TInputPixel &)
  {
    if ( m_UseVectorBasedAlgorithm )
            { return GetValueVector(); }
    else
            { return GetValueMap(); }
  }

  static inline bool UseVectorBasedAlgorithm()
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

  typedef std::map< TInputPixel, unsigned long > MapType;

  inline void AddPixelMap(const TInputPixel & p)
  { m_Map[p]++; }

  inline void RemovePixelMap(const TInputPixel & p)
  { m_Map[p]--; }

  inline TInputPixel GetValueMap()
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
    if ( !m_Map.empty() )
            { return m_Map.rbegin()->first - m_Map.begin()->first; }
    return 0;
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
    m_Max = NumericTraits< TInputPixel >::NonpositiveMin();
    m_Min = NumericTraits< TInputPixel >::max();
    m_Count = 0;
  }

  inline void AddPixelVector(const TInputPixel & p)
  {
    m_Vector[static_cast< int >( p - NumericTraits < TInputPixel > ::NonpositiveMin() )]++;
    if ( p > m_Max )
            { m_Max = p; }
    if ( p < m_Min )
            { m_Min = p; }
    m_Count++;
  }

  inline void RemovePixelVector(const TInputPixel & p)
  {
    m_Vector[static_cast< int >( p - NumericTraits < TInputPixel > ::NonpositiveMin() )]--;
    m_Count--;
    if ( m_Count > 0 )
      {
      while ( m_Vector[static_cast< int >( m_Max - NumericTraits < TInputPixel > ::NonpositiveMin() )] == 0 )
              { m_Max--; }
      while ( m_Vector[static_cast< int >( m_Min - NumericTraits < TInputPixel > ::NonpositiveMin() )] == 0 )
              { m_Min++; }
      }
    else
      {
      m_Max = NumericTraits< TInputPixel >::NonpositiveMin();
      m_Min = NumericTraits< TInputPixel >::max();
      }
  }

  inline TInputPixel GetValueVector()
  {
    if ( m_Count > 0 )
            { return m_Max - m_Min; }
    else
            { return NumericTraits< TInputPixel >::Zero; }
  }

  std::vector< unsigned long > m_Vector;
  TInputPixel                  m_Min;
  TInputPixel                  m_Max;
  unsigned long                m_Count;
};
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
 */

template< class TInputImage, class TOutputImage, class TKernel >
class ITK_EXPORT MovingHistogramMorphologicalGradientImageFilter:
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
  { return Function::MorphologicalGradientHistogram< ITK_TYPENAME TInputImage::PixelType >::UseVectorBasedAlgorithm(); }
protected:
  MovingHistogramMorphologicalGradientImageFilter() {}
  ~MovingHistogramMorphologicalGradientImageFilter() {}
private:
  MovingHistogramMorphologicalGradientImageFilter(const Self &); //purposely not
                                                                 // implemented
  void operator=(const Self &);                                  //purposely not
                                                                 // implemented
};                                                               // end of class
} // end namespace itk

#endif
