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
#ifndef itkMovingHistogramMorphologyImageFilter_h
#define itkMovingHistogramMorphologyImageFilter_h

#include "itkMorphologyHistogram.h"
#include "itkMovingHistogramImageFilter.h"
#include <list>
#include <map>

namespace itk
{

/**
 * \class MovingHistogramMorphologyImageFilter
 * \brief Base class for MovingHistogramDilateImageFilter and MovingHistogramErodeImageFilter.
 *
 * This class is similar to MovingHistogramImageFilter, but adds support for
 * boundaries and does not fully update the histogram in order to enhance
 * performance.
 *
 * \sa MovingHistogramImageFilter, MovingHistogramDilateImageFilter, MovingHistogramErodeImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKMathematicalMorphology
 */

template< typename TInputImage, typename TOutputImage, typename TKernel, typename THistogram >
class ITK_TEMPLATE_EXPORT MovingHistogramMorphologyImageFilter:
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
               MovingHistogramImageFilter);

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

  /** Set/Get the boundary value.
   *  Subclasses should set their own values. */
  itkSetMacro(Boundary, PixelType);
  itkGetConstMacro(Boundary, PixelType);

  /** Return true if the vector based algorithm is used, and
   * false if the map based algorithm is used */
  static bool GetUseVectorBasedAlgorithm()
  { return THistogram::UseVectorBasedAlgorithm(); }

protected:
  MovingHistogramMorphologyImageFilter();
  ~MovingHistogramMorphologyImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Multi-thread version GenerateData. */
//   void  ThreadedGenerateData (const OutputImageRegionType&
//                               outputRegionForThread,
//                               ThreadIdType threadId);

  /** Configure the histogram.
   *  Used by this class to pass the boundary value to the histogram object. */
  virtual void ConfigureHistogram(THistogram & histogram) ITK_OVERRIDE;

  PixelType m_Boundary;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MovingHistogramMorphologyImageFilter);
};                                                    // end of class
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMovingHistogramMorphologyImageFilter.hxx"
#endif

#endif
