/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSpatialFunctionImageEvaluatorFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSpatialFunctionImageEvaluatorFilter_h
#define __itkSpatialFunctionImageEvaluatorFilter_h

#include "itkImageFunction.h"
#include "itkImageRegionIterator.h"
#include "itkImageToImageFilter.h"
#include "itkSize.h"
#include "itkSpatialFunction.h"

namespace itk
{

/** \class SpatialFunctionImageEvaluatorFilter
 * \brief Evaluates a SpatialFunction onto a source image
 *
 * SpatialFunctionImageEvaluatorFilter walks an input image and evaluates
 * the function at every pixel location. The output of the spatial function
 * and the pixel type of the output image must be compatible.
 *
 * Like its parent ImageToImageFilter, this class functions in the filtering
 * pipeline and produces a unique output image.
 * 
 * \ingroup ImageFilters
 */
template<class TSpatialFunction, class TInputImage, class TOutputImage>
class ITK_EXPORT SpatialFunctionImageEvaluatorFilter :
   public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef SpatialFunctionImageEvaluatorFilter Self;
  typedef ImageToImageFilter<TInputImage,TOutputImage>  Superclass;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);  

  /** Run-time type information (and related methods). */
  itkTypeMacro( SpatialFunctionImageEvaluatorFilter, ImageToImageFilter );

  /** Number of dimensions. */
  enum {NDimensions = TInputImage::ImageDimension};

  /** Image size typedef. */
  typedef Size<TOutputImage::ImageDimension> SizeType;

  /** Image index typedef. */
  typedef typename TOutputImage::IndexType IndexType;

  /** Image pixel value typedef. */
  typedef typename TOutputImage::PixelType PixelType;

  /** Typedef to describe the output image region type. */
  typedef typename TOutputImage::RegionType OutputImageRegionType;

  /** Type of function. */
  typedef TSpatialFunction TFunctionType;

  /** Return type of function. */
  typedef typename TFunctionType::OutputType TFunctionValueType;

  /** Typedef describing vector info. */
  typedef typename TFunctionType::InputType TPositionType;

  /** Set the internal spatial function. */
  void SetFunction( TFunctionType* PixelFunction )
    {m_PixelFunction = PixelFunction;};

protected:
  SpatialFunctionImageEvaluatorFilter();
  virtual ~SpatialFunctionImageEvaluatorFilter() {};

  /** Method for evaluating the implicit function over the image. */
  void GenerateData();

private:
  SpatialFunctionImageEvaluatorFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** The function that will be evaluated over the image */
  TFunctionType* m_PixelFunction;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSpatialFunctionImageEvaluatorFilter.txx"
#endif

#endif
