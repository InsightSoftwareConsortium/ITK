/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGradientImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkGradientImageFilter_h
#define __itkGradientImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImage.h"
#include "itkCovariantVector.h"

namespace itk
{
/** \class GradientImageFilter
 * \brief Computes the gradient of an image using directional derivatives.
 *
 * Computes the gradient of an image using directional derivatives.
 * The directional derivative at each pixel location is computed by
 * convolution with a first-order derivative operator.
 *
 * The second template parameter defines the value type used in the
 * derivative operator (defaults to float).  The third template
 * parameter defines the value type used for output image (defaults to
 * float).  The output image is defined as a covariant vector image
 * whose value type is specified as this third template parameter.
 *
 *
 * \sa Image
 * \sa Neighborhood
 * \sa NeighborhoodOperator
 * \sa NeighborhoodIterator
 * 
 * \ingroup GradientFilters 
 */
template <class TInputImage, class TOperatorValueType=float, class TOutputValueType=float>
class ITK_EXPORT GradientImageFilter :
    public ImageToImageFilter< TInputImage,
                               Image<CovariantVector<TOutputValueType, ::itk::GetImageDimension<TInputImage>::ImageDimension>,  ::itk::GetImageDimension<TInputImage>::ImageDimension> >
{
public:
  /** Extract dimension from input image. */
  itkStaticConstMacro(InputImageDimension, unsigned int,
                      TInputImage::ImageDimension);
  itkStaticConstMacro(OutputImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Standard class typedefs. */
  typedef GradientImageFilter Self;

  /** Convenient typedefs for simplifying declarations. */
  typedef TInputImage InputImageType;
  typedef typename InputImageType::Pointer InputImagePointer;
  typedef Image<CovariantVector<TOutputValueType, itkGetStaticConstMacro(OutputImageDimension)>,  itkGetStaticConstMacro(OutputImageDimension)> OutputImageType;
  typedef typename OutputImageType::Pointer OutputImagePointer;

  /** Standard class typedefs. */
  typedef ImageToImageFilter< InputImageType, OutputImageType> Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GradientImageFilter, ImageToImageFilter);
  
  /** Image typedef support. */
  typedef typename InputImageType::PixelType InputPixelType;
  typedef TOperatorValueType OperatorValueType;
  typedef TOutputValueType OutputValueType;
  typedef CovariantVector<OutputValueType, itkGetStaticConstMacro(OutputImageDimension)> OutputPixelType;
  typedef typename OutputImageType::RegionType OutputImageRegionType;
  
  /** GradientImageFilter needs a larger input requested region than
   * the output requested region.  As such, GradientImageFilter needs
   * to provide an implementation for GenerateInputRequestedRegion()
   * in order to inform the pipeline execution model.
   *
   * \sa ImageToImageFilter::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion() throw(InvalidRequestedRegionError);

protected:
  GradientImageFilter() {}
  virtual ~GradientImageFilter() {}

  /** GradientImageFilter can be implemented as a multithreaded filter.
   * Therefore, this implementation provides a ThreadedGenerateData()
   * routine which is called for each processing thread. The output
   * image data is allocated automatically by the superclass prior to
   * calling ThreadedGenerateData().  ThreadedGenerateData can only
   * write to the portion of the output image specified by the
   * parameter "outputRegionForThread"
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData() */
  void ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                            int threadId );

private:
  GradientImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGradientImageFilter.txx"
#endif

#endif
