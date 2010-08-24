/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMovingHistogramErodeImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMovingHistogramErodeImageFilter_h
#define __itkMovingHistogramErodeImageFilter_h

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
 */

template< class TInputImage, class TOutputImage, class TKernel >
class ITK_EXPORT MovingHistogramErodeImageFilter:
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

  ~MovingHistogramErodeImageFilter() {}
private:
  MovingHistogramErodeImageFilter(const Self &); //purposely not implemented
  void operator=(const Self &);                  //purposely not implemented
};                                               // end of class
} // end namespace itk

#endif
