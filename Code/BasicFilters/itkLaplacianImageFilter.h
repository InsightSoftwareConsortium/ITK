/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLaplacianImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkLaplacianImageFilter_h
#define __itkLaplacianImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImage.h"

namespace itk
{
/**
 * \class LaplacianImageFilter
 *
 * This filter computes the Laplacian of a scalar-valued image. The Laplacian
 * is an isotropic measure of the 2nd spatial derivative of an image. The
 * Laplacian of an image highlights regions of rapid intensity change and is
 * therefore often used for edge detection.  Often, the Laplacian is applied to
 * an image that has first been smoothed with a Gaussian filter in order to
 * reduce its sensitivity to noise.
 *
 * \par
 * The Laplacian at each pixel location is computed by convolution with the
 * itk::LaplacianOperator.
 *
 * \par Inputs and Outputs
 * The input to this filter is a scalar-valued itk::Image of arbitrary
 * dimension. The output is a scalar-valued itk::Image.
 * \sa Image
 * \sa Neighborhood
 * \sa NeighborhoodOperator
 * \sa NeighborhoodIterator
 * \sa LaplacianOperator
 *
 * \ingroup ImageFeatureExtraction */
template <class TInputImage, class TOutputImage>
class ITK_EXPORT LaplacianImageFilter : 
           public ImageToImageFilter< TInputImage, TOutputImage > 
{
public:
  /** Standard "Self" & Superclass typedef.   */
  typedef LaplacianImageFilter Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;

  /** Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same. */
  typedef typename TOutputImage::PixelType OutputPixelType;
  typedef typename TOutputImage::InternalPixelType OutputInternalPixelType;
  typedef typename TInputImage::PixelType InputPixelType;
  typedef typename TInputImage::InternalPixelType InputInternalPixelType;
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TOutputImage::ImageDimension);
  
  /** Image typedef support. */
  typedef TInputImage  InputImageType;
  typedef TOutputImage OutputImageType;
  typedef typename InputImageType::Pointer InputImagePointer;

  /** Smart pointer typedef support.   */
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Run-time type information (and related methods)  */
  itkTypeMacro(LaplacianImageFilter, ImageToImageFilter);
  
  /** Method for creation through the object factory.  */
  itkNewMacro(Self);

  /** LaplacianImageFilter needs a larger input requested region than
   * the output requested region (larger in the direction of the
   * derivative).  As such, LaplacianImageFilter needs to provide an
   * implementation for GenerateInputRequestedRegion() in order to
   * inform the pipeline execution model.
   *
   * \sa ImageToImageFilter::GenerateInputRequestedRegion()  */
  virtual void GenerateInputRequestedRegion() throw(InvalidRequestedRegionError);

protected:
  LaplacianImageFilter() {}
  virtual ~LaplacianImageFilter() {}

  /** Standard pipeline method. While this class does not implement a
   * ThreadedGenerateData(), its GenerateData() delegates all
   * calculations to an NeighborhoodOperatorImageFilter.  Since the
   * NeighborhoodOperatorImageFilter is multithreaded, this filter is
   * multithreaded by default.   */
  void GenerateData();
  void PrintSelf(std::ostream& os, Indent indent) const
    { Superclass::PrintSelf(os,indent);   }

private:
  LaplacianImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented
  
};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLaplacianImageFilter.txx"
#endif

#endif
