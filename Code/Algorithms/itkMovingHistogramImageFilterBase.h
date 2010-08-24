/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMovingHistogramImageFilterBase.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMovingHistogramImageFilterBase_h
#define __itkMovingHistogramImageFilterBase_h

#include "itkKernelImageFilter.h"
#include <list>
#include <map>
#include <set>

namespace itk
{
/**
 * \class MovingHistogramImageFilterBase
 * \brief Implements a generic moving histogram algorithm
 *
 * This filter is a base class to implement efficiently many neighborhood
 * filters. Instead of visiting all the neighbors of a pixel, the set
 * of pixels in the neighborhood is updated when the filter is moving
 * to a new pixel. The number of pixels read for each pixel can be very
 * smaller than the number of pixels read by a basic algorithm.
 *
 * This filter moves the neighborhood over all the pixels of the output requested region,
 * and pass the pixel added and removed of the neighborhood to the an
 * histogram class. This filter doesn't implement the histogram class - it
 * must be implement and passed as template parameter. The histogram class
 * is not necessary a real histogram. It can be implemented in many ways,
 * and only has to provide the methods described below.
 *
 * This filter takes 4 template parameters: the input and output image type,
 * the structuring element (or kernel) type, and the histogram type.
 * The input and output image must have the same number of dimension.
 *
 * The histogram type is a class which has to implements seven methods:
 * + a default constructor which takes no parameter.
 * + HistogramType * Clone() must produce a new identical histogram. It is
 * used internally to optimize the filter, by avoiding reverse iteration
 * over the image.
 * + void AddPixel( const InputPixelType &p ) is called when a new pixel
 * is added to the histogram.
 * + void RemovePixel( const InputPixelType &p ) is called when a pixel
 * is removed of the histogram.
 * + void AddBoundary() is called when a pixel outside the image is added.
 * No value is provided: it's the responsability to the histogram class to
 * get it if needed. This method can be kept empty to ignore the boundary
 * pixels.
 * + void RemoveBoundary() is called to when a pixel outside the image is removed.
 * No value is provided: it's the responsability to the histogram class to
 * get it if needed. This method can be kept empty to ignore the boundary
 * pixels.
 * + AType GetValue() is called to set the value of the output image. AType
 * must be the output pixel type, or a type castable to the output pixel type.
 *
 * MovingHistogramImageFilterBase add the new pixels before removing the old ones,
 * so, if AddBoundary() is implemented and/or the kernel is symetric, it is safe
 * to consider that the histogram will never be empty.
 *
 * One histogram is created for each thread by the method NewHistogram().
 * The NewHistogram() method can be overiden to pass some parameters to the
 * histogram.
 *
 * The neighborhood is defined by a structuring element, and must a
 * itk::Neighborhood object or a subclass.
 * The structuring element is assumed to be composed of binary
 * values (zero or one). Only elements of the structuring element
 * having values > 0 are candidates for affecting the center pixel.
 *
 * \sa MovingWindowMeanImageFilter, RankImageFilter, MaskedMovingHistogramImageFitler,
 * \sa MovingHistogramMorphologicalGradientImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 *
 * \author Gaetan Lehmann
 * \author Richard Beare
 */

template< class TInputImage, class TOutputImage, class TKernel >
class ITK_EXPORT MovingHistogramImageFilterBase:
  public KernelImageFilter< TInputImage, TOutputImage, TKernel >
{
public:
  /** Standard class typedefs. */
  typedef MovingHistogramImageFilterBase                          Self;
  typedef KernelImageFilter< TInputImage, TOutputImage, TKernel > Superclass;
  typedef SmartPointer< Self >                                    Pointer;
  typedef SmartPointer< const Self >                              ConstPointer;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(MovingHistogramImageFilterBase,
               KernelImageFilter);

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

  /** Set kernel (structuring element). */
  void SetKernel(const KernelType & kernel);

  itkGetConstMacro(PixelsPerTranslation, unsigned long);
protected:
  MovingHistogramImageFilterBase();
  ~MovingHistogramImageFilterBase() {}

  void PrintSelf(std::ostream & os, Indent indent) const;

  void GetDirAndOffset(const IndexType LineStart,
                       const IndexType PrevLineStart,
                       OffsetType & LineOffset,
                       OffsetType & Changes,
                       int & LineDirection);

  // store the added and removed pixel offset in a list
  OffsetMapType m_AddedOffsets;
  OffsetMapType m_RemovedOffsets;

  // store the offset of the kernel to initialize the histogram
  OffsetListType m_KernelOffsets;

  FixedArray< int, itkGetStaticConstMacro(ImageDimension) > m_Axes;

  unsigned long m_PixelsPerTranslation;
private:
  MovingHistogramImageFilterBase(const Self &); //purposely not implemented
  void operator=(const Self &);                 //purposely not implemented

  class DirectionCost
  {
public:
    DirectionCost(int dimension, int count)
    {
      m_Dimension = dimension;
      m_Count = count;
    }

    /**
     * return true if the object is a worth choice for the best axis
     * than the object in parameter
     */
    inline bool operator<(const DirectionCost & dc) const
    {
      if ( m_Count > dc.m_Count )
            { return true; }
      else if ( m_Count < dc.m_Count )
            { return false; }
      else //if (m_Count == dc.m_Count)
            { return m_Dimension > dc.m_Dimension; }
    }

    int m_Dimension;
    int m_Count;
  };
}; // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMovingHistogramImageFilterBase.txx"
#endif

#endif
