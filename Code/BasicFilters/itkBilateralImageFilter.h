/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBilateralImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBilateralImageFilter_h
#define __itkBilateralImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImage.h"
#include "itkNeighborhoodIterator.h"
#include "itkConstSmartNeighborhoodIterator.h"
#include "itkNeighborhood.h"

namespace itk
{
/**
 * \class BilateralImageFilter
 * \brief Blurs an image while preserving edges
 *
 * This filter uses bilateral filtering to blur an image using both
 * domain and range "neighorhoods". Pixels that are close to a pixel
 * in the image domain and similar to a pixel in the image range are
 * used to calculate the filtered value. Two gaussian kernels (one in
 * the image domain and one in the image range) are used to smooth
 * the image. The result is an image that is smoothed in homogeneous
 * regions yet has edges preserved. The result is similar to
 * anisotropic diffusion but the implementation in non-iterative.
 * Another benefit to bilateral filtering is that any distance metric
 * can be used for kernel smoothing the image range.  Hence, color
 * images can be smoothed as vector images, using the CIE distances
 * between intensity values as the similarity metric (the Gaussian
 * kernel for the image domain is evaluated using CIE distances).
 * A separate version of this filter will be designed for color
 * and vector images.
 *
 * The bilateral operator used here was described by Tomasi and
 * Manduchi (Bilateral Filtering for Gray and ColorImages. IEEE
 * ICCV. 1998.)
 *
 * \sa GaussianOperator
 * \sa AnisotropicDiffusionImageFilter
 * \sa Image
 * \sa Neighborhood
 * \sa NeighborhoodOperator
 * 
 * \ingroup ImageEnhancement 
 * \ingroup ImageFeatureExtraction 
 * \todo Support color images
 * \todo Support vector images
 */

template <class TInputImage, class TOutputImage >
class ITK_EXPORT BilateralImageFilter :
    public ImageToImageFilter< TInputImage, TOutputImage > 
{
public:
  /** Standard class typedefs. */
  typedef BilateralImageFilter Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(BilateralImageFilter, ImageToImageFilter);
  
  /** Image type information. */
  typedef TInputImage  InputImageType;
  typedef TOutputImage OutputImageType;

  /** Superclass typedefs. */
  typedef typename Superclass::OutputImageRegionType OutputImageRegionType;

  /** Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same. */
  typedef typename TOutputImage::PixelType OutputPixelType;
  typedef typename TOutputImage::InternalPixelType OutputInternalPixelType;
  typedef typename NumericTraits<OutputPixelType>::RealType OutputPixelRealType;
  typedef typename TInputImage::PixelType InputPixelType;
  typedef typename TInputImage::InternalPixelType InputInternalPixelType;

  /** Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same. */
  enum { ImageDimension = TOutputImage::ImageDimension };
  
  /** Neighborhood iterator types. */
  typedef ConstNeighborhoodIterator<TInputImage> 
    NeighborhoodIteratorType ;
  typedef ConstSmartNeighborhoodIterator<TInputImage> 
    SmartNeighborhoodIteratorType ;
  
  /** Kernel typedef. */
  typedef Neighborhood<double, ImageDimension> KernelType;
  
  /** Kernel iterator. */
  typedef typename KernelType::Iterator KernelIteratorType ;
  typedef typename KernelType::ConstIterator KernelConstIteratorType ;

  /** Gaussian image type */
  typedef Image<double, ImageDimension> GaussianImageType;
  
  /** Standard get/set macros for filter parameters. */
  itkSetVectorMacro(DomainVariance, double, ImageDimension);
  itkSetVectorMacro(DomainVariance, float, ImageDimension);
  itkGetVectorMacro(DomainVariance, const double, ImageDimension);
  itkSetMacro(RangeVariance, double);
  itkGetMacro(RangeVariance, const double);
  itkGetMacro(FilterDimensionality, unsigned int);
  itkSetMacro(FilterDimensionality, unsigned int);
  
  /** Convenience get/set methods for setting all dimensional parameters to the
   * same values.  */
  void SetDomainVariance(const double v)
    {
    double vArray[ImageDimension];
    for (unsigned int i = 0; i<ImageDimension; ++i) { vArray[i] = v; }
    this->SetDomainVariance(vArray);
    }
  
  /** Convenience get/set methods for setting all dimensional parameters to the
   * same values.  */
  void SetDomainVariance(const float v)
    {
    double vArray[ImageDimension];
    for (unsigned int i = 0; i<ImageDimension; ++i) { vArray[i] = static_cast<double>(v); }
    this->SetDomainVariance(vArray);
    }
  
  /** BilateralImageFilter needs a larger input requested region than
   * the output requested region (larger by the size of the domain
   * Gaussian kernel).  As such, BilateralImageFilter needs to provide
   * an implementation for GenerateInputRequestedRegion() in order to
   * inform the pipeline execution model.  
   * \sa ImageToImageFilter::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion() throw(InvalidRequestedRegionError);

protected:
  BilateralImageFilter()
    {
    int i;
    for (i = 0; i < ImageDimension; i++)
      {
      m_DomainVariance[i] = 1.0f;
      }
    m_RangeVariance = 1.0f;
    m_FilterDimensionality = ImageDimension;
    }
  virtual ~BilateralImageFilter() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Standard pipeline method. This filter is implemented as a multi-threaded
   * filter. */
  void ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                            int threadId); 

  
private:
  BilateralImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** The variance of the gaussian blurring kernel in the image range */
  double m_RangeVariance;
  
  /** The variance of the gaussian blurring kernel in each dimensional direction. */
  double m_DomainVariance[ImageDimension];

  /** Number of dimensions to process. Default is all dimensions */
  unsigned int m_FilterDimensionality;

  
};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBilateralImageFilter.txx"
#endif

#endif
