/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientToMagnitudeImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGradientToMagnitudeImageFilter_h
#define __itkGradientToMagnitudeImageFilter_h

#include "itkImageFunction.h"
#include "itkImageRegionIterator.h"
#include "itkImageToImageFilter.h"
#include "itkSize.h"

namespace itk
{

/** \class GradientToMagnitudeImageFilter
 * \brief Converts a gradient image to a magnitude of gradient image
 * 
 * \ingroup GradientFilters
 */
template<class TInputImage, class TOutputImage, class TComputation=double>
class ITK_EXPORT GradientToMagnitudeImageFilter :
   public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef GradientToMagnitudeImageFilter Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage>  Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Run-time type information (and related methods). */
  itkTypeMacro( GradientToMagnitudeImageFilter, ImageToImageFilter );

  /** Number of dimensions, */
  enum {NDimensions = TInputImage::ImageDimension};

  /** Image size typedef. */
  typedef Size<TOutputImage::ImageDimension> SizeType;

  /** Image index typedef. */
  typedef typename TOutputImage::IndexType IndexType;

  /** Image pixel value typedef. */
  typedef typename TOutputImage::PixelType OutputImagePixelType;

  /** Typedef to describe the output image region type. */
  typedef typename TOutputImage::RegionType OutputImageRegionType;

protected:
  /** Method for evaluating the implicit function over the image. */
  void GenerateData();

  GradientToMagnitudeImageFilter();
  virtual ~GradientToMagnitudeImageFilter() {};
  void PrintSelf(std::ostream& os, Indent indent) const;

private:
  GradientToMagnitudeImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGradientToMagnitudeImageFilter.txx"
#endif

#endif
