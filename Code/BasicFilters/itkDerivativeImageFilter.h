/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDerivativeImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDerivativeImageFilter_h
#define __itkDerivativeImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImage.h"

namespace itk
{
/** \class DerivativeImageFilter
 * \brief Computes the directional derivative of an image.
 * The directional derivative at each pixel location is computed by convolution
 * with a first-order derivative operator.
 *
 * \sa Image
 * \sa Neighborhood
 * \sa NeighborhoodOperator
 * \sa NeighborhoodIterator
 * 
 * \ingroup ImageFeatureExtraction 
 */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT DerivativeImageFilter :
    public ImageToImageFilter< TInputImage, TOutputImage > 
{
public:
  /** Standard class typedefs. */
  typedef DerivativeImageFilter Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same. */
  typedef typename TOutputImage::PixelType OutputPixelType;
  typedef typename TOutputImage::InternalPixelType OutputInternalPixelType;
  typedef typename TInputImage::PixelType InputPixelType;
  typedef typename TInputImage::InternalPixelType InputInternalPixelType;

  /** Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same. */
  enum { ImageDimension = TOutputImage::ImageDimension };
  
  /** Image typedef support. */
  typedef TInputImage  InputImageType;
  typedef TOutputImage OutputImageType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(DerivativeImageFilter, ImageToImageFilter);
  
  /** Standard get/set macros for filter parameters. */
  itkSetMacro(Order, unsigned int);
  itkGetMacro(Order, unsigned int);
  itkSetMacro(Direction, unsigned int);
  itkGetMacro(Direction, unsigned int);
  
  /** DerivativeImageFilter needs a larger input requested region than
   * the output requested region (larger in the direction of the
   * derivative).  As such, DerivativeImageFilter needs to provide an
   * implementation for GenerateInputRequestedRegion() in order to
   * inform the pipeline execution model.
   *
   * \sa ImageToImageFilter::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion() throw(InvalidRequestedRegionError);

protected:
  DerivativeImageFilter() {m_Order = 0; m_Direction = 0;}
  virtual ~DerivativeImageFilter() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Standard pipeline method. While this class does not implement a
   * ThreadedGenerateData(), its GenerateData() delegates all
   * calculations to an NeighborhoodOperatorImageFilter.  Since the
   * NeighborhoodOperatorImageFilter is multithreaded, this filter is
   * multithreaded by default. */
  void GenerateData();

private:
  DerivativeImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** The order of the derivative. */
  unsigned int m_Order;

  /** The direction of the derivative. */
  unsigned int m_Direction;

};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDerivativeImageFilter.txx"
#endif

#endif
