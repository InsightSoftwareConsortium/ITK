/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMovingHistogramMorphologyImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMovingHistogramMorphologyImageFilter_h
#define __itkMovingHistogramMorphologyImageFilter_h

#include "itkMovingHistogramImageFilter.h"
#include <list>
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

  inline TInputPixel GetValue(const TInputPixel &)
  {
    if ( m_UseVectorBasedAlgorithm )
            { return GetValueVector(); }
    else
            { return GetValueMap(); }
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

/**
 * \class MovingHistogramMorphologyImageFilter
 * \brief base class for MovingHistogramDilateImageFilter and MovingHistogramErodeImageFilter
 *
 * This class is similar to MovingHistogramImageFilter but add support
 * for boundaries and don't fully update the histogram to enhance performances.
 *
 * \sa MovingHistogramImageFilter, MovingHistogramDilateImageFilter, MovingHistogramErodeImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 */

template< class TInputImage, class TOutputImage, class TKernel, class THistogram >
class ITK_EXPORT MovingHistogramMorphologyImageFilter:
  public MovingHistogramImageFilter< TInputImage, TOutputImage, TKernel, THistogram >
{
public:
  /** Standard class typedefs. */
  typedef MovingHistogramMorphologyImageFilter Self;
  typedef MovingHistogramImageFilter< TInputImage, TOutputImage, TKernel, THistogram >
  Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(MovingHistogramMorphologyImageFilter,
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

  /** Image related typedefs. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Kernel typedef. */
  typedef TKernel KernelType;

  /** Kernel (structuring element) iterator. */
  typedef typename KernelType::ConstIterator KernelIteratorType;

  /** n-dimensional Kernel radius. */
  typedef typename KernelType::SizeType RadiusType;

  typedef typename std::list< OffsetType > OffsetListType;

  typedef typename std::map< OffsetType, OffsetListType, typename OffsetType::LexicographicCompare > OffsetMapType;

  /** Set/Get the boundary value. */
  itkSetMacro(Boundary, PixelType);
  itkGetConstMacro(Boundary, PixelType);

  /** Return true if the vector based algorithm is used, and
   * false if the map based algorithm is used */
  static bool GetUseVectorBasedAlgorithm()
  { return THistogram::UseVectorBasedAlgorithm(); }
protected:
  MovingHistogramMorphologyImageFilter();
  ~MovingHistogramMorphologyImageFilter() {}
  void PrintSelf(std::ostream & os, Indent indent) const;

  /** Multi-thread version GenerateData. */
//   void  ThreadedGenerateData (const OutputImageRegionType&
//                               outputRegionForThread,
//                               int threadId);

  /** needed to pass the boundary value to the histogram object */
  virtual THistogram * NewHistogram();

  PixelType m_Boundary;
private:
  MovingHistogramMorphologyImageFilter(const Self &); //purposely not
                                                      // implemented
  void operator=(const Self &);                       //purposely not
                                                      // implemented
};                                                    // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMovingHistogramMorphologyImageFilter.txx"
#endif

#endif
