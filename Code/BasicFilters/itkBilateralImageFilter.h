/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBilateralImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBilateralImageFilter_h
#define __itkBilateralImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImage.h"
#include "itkFixedArray.h"
#include "itkNeighborhoodIterator.h"
#include "itkConstNeighborhoodIterator.h"
#include "itkNeighborhood.h"

namespace itk
{
/**
 * \class BilateralImageFilter
 * \brief Blurs an image while preserving edges
 *
 * This filter uses bilateral filtering to blur an image using both
 * domain and range "neighborhoods". Pixels that are close to a pixel
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
 * Bilateral filtering is capable of reducing the noise in an image
 * by an order of magnitude while maintaining edges.
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
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TOutputImage::ImageDimension);
  
  /** Typedef of double containers */
  typedef FixedArray<double, itkGetStaticConstMacro(ImageDimension)> ArrayType;

  /** Neighborhood iterator types. */
  typedef ConstNeighborhoodIterator<TInputImage> 
  NeighborhoodIteratorType ;
  
  /** Kernel typedef. */
  typedef
  Neighborhood<double, itkGetStaticConstMacro(ImageDimension)> KernelType;
  typedef typename KernelType::SizeType SizeType;
  
  /** Kernel iterator. */
  typedef typename KernelType::Iterator KernelIteratorType ;
  typedef typename KernelType::ConstIterator KernelConstIteratorType ;

  /** Gaussian image type */
  typedef
  Image<double, itkGetStaticConstMacro(ImageDimension)> GaussianImageType;
  
  /** Standard get/set macros for filter parameters.
   * DomainSigma is specified in the same units as the Image spacing.
   * RangeSigma is specified in the units of intensity. */
  itkSetMacro(DomainSigma, ArrayType);
  itkGetMacro(DomainSigma, const ArrayType);
  itkSetMacro(RangeSigma, double);
  itkGetMacro(RangeSigma, double);
  itkGetMacro(FilterDimensionality, unsigned int);
  itkSetMacro(FilterDimensionality, unsigned int);
  
  /** Convenience get/set methods for setting all domain parameters to the
   * same values.  */
  void SetDomainSigma(const double v)
  {
    m_DomainSigma.Fill(v);
  }

  /** Control automatic kernel size determination. When
   * automatic is "on", the kernel size is a function of the domain
   * sigma. When automatic is "off", the kernel size is whatever is
   * specified by the user.
   * \sa SetRadius() */
  itkBooleanMacro(AutomaticKernelSize);
  itkGetMacro(AutomaticKernelSize, bool);
  itkSetMacro(AutomaticKernelSize, bool);

  /** Set/Get the kernel radius, specified in pixels.  This parameter
   * is used only when AutomaticNeighborhoodSize is "off". */
  void SetRadius(const unsigned long);
  itkSetMacro(Radius, SizeType);
  itkGetConstReferenceMacro(Radius, SizeType);
  
  
  /** Set/Get the number of samples in the approximation to the Gaussian
   * used for the range smoothing. Samples are only generated in the
   * range of [0, 4*m_RangeSigma]. Default is 100. */
  itkSetMacro(NumberOfRangeGaussianSamples, unsigned long);
  itkGetMacro(NumberOfRangeGaussianSamples, unsigned long);

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(OutputHasNumericTraitsCheck,
                  (Concept::HasNumericTraits<OutputPixelType>));
  /** End concept checking */
#endif

protected:
  /** Constructor.  Default value for DomainSigma is 4. Default value
   * RangeSigma is 50. */
  BilateralImageFilter()
  {
    m_Radius.Fill(1);
    m_AutomaticKernelSize = true;
    m_DomainSigma.Fill(4.0);
    m_RangeSigma = 50.0;
    m_FilterDimensionality = ImageDimension;
    m_NumberOfRangeGaussianSamples = 100;
    m_DynamicRange = 0.0;
    m_DynamicRangeUsed = 0.0;
    m_DomainMu = 2.5;  // keep small to keep kernels small
    m_RangeMu = 4.0;   // can be bigger then DomainMu since we only
                       // index into a single table

  }
  virtual ~BilateralImageFilter() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Do some setup before the ThreadedGenerateData */
  void BeforeThreadedGenerateData();
  
  /** Standard pipeline method. This filter is implemented as a multi-threaded
   * filter. */
  void ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                            int threadId); 

  /** BilateralImageFilter needs a larger input requested region than
   * the output requested region (larger by the size of the domain
   * Gaussian kernel).  As such, BilateralImageFilter needs to provide
   * an implementation for GenerateInputRequestedRegion() in order to
   * inform the pipeline execution model.  
   * \sa ImageToImageFilter::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion() throw(InvalidRequestedRegionError);

  
private:
  BilateralImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** The standard deviation of the gaussian blurring kernel in the image
      range. Units are intensity. */
  double m_RangeSigma;
  
  /** The standard deviation of the gaussian blurring kernel in each
      dimensional direction. Units match image spacing units. */
  ArrayType m_DomainSigma;

  /** Multiplier used to define statistical thresholds.  Gaussians are
   * only evaluated to m_DomainMu*m_DomainSigma or m_RangeMu*m_RangeSigma. */
  double m_DomainMu;
  double m_RangeMu;

  /** Number of dimensions to process. Default is all dimensions */
  unsigned int m_FilterDimensionality;

  /** Gaussian kernel used for smoothing in the spatial domain */
  KernelType m_GaussianKernel;
  SizeType   m_Radius;
  bool       m_AutomaticKernelSize;

  /** Variables for the lookup table of range gaussian values */
  unsigned long m_NumberOfRangeGaussianSamples;
  double m_DynamicRange;
  double m_DynamicRangeUsed;
  std::vector<double> m_RangeGaussianTable;
};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBilateralImageFilter.txx"
#endif

#endif
