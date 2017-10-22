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
#ifndef itkMovingHistogramErodeImageFilter_h
#define itkMovingHistogramErodeImageFilter_h

#include "itkMovingHistogramMorphologyImageFilter.h"

namespace itk
{
/**
 * \class MovingHistogramErodeImageFilter
 * \brief gray scale erosion of an image
 *
 * Erode an image using grayscale morphology. Erode takes the
 * minimum of all the pixels identified by the structuring element.
 *
 * The structuring element is assumed to be composed of binary
 * values (zero or one). Only elements of the structuring element
 * having values > 0 are candidates for affecting the center pixel.
 *
 * \sa MorphologyImageFilter, GrayscaleFunctionMorphologicalGradientImageFilter, BinaryMorphologicalGradientImageFilter
 * \ingroup ImageEnhancement  MathematicalMorphologyImageFilters
 * \ingroup ITKMathematicalMorphology
 */

template< typename TInputImage, typename TOutputImage, typename TKernel >
class MovingHistogramErodeImageFilter:
  public MovingHistogramMorphologyImageFilter< TInputImage, TOutputImage, TKernel,
                                               typename Function::MorphologyHistogram< typename TInputImage::PixelType,
                                                                                       typename std::less< typename
                                                                                                           TInputImage
                                                                                                           ::PixelType > > >
{
public:
  /** Standard class typedefs. */
  typedef MovingHistogramErodeImageFilter Self;
  typedef MovingHistogramMorphologyImageFilter< TInputImage, TOutputImage, TKernel,
                                                typename Function::MorphologyHistogram< typename TInputImage::PixelType,
                                                                                        typename std::less< typename
                                                                                                            TInputImage
                                                                                                            ::PixelType > > >  Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Standard New method. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(MovingHistogramErodeImageFilter,
               MovingHistogramMorphologyImageFilter);

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

protected:
  MovingHistogramErodeImageFilter()
  {
    this->m_Boundary = NumericTraits< PixelType >::max();
  }

  ~MovingHistogramErodeImageFilter() ITK_OVERRIDE {}

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MovingHistogramErodeImageFilter);
};                                               // end of class
} // end namespace itk

#endif
