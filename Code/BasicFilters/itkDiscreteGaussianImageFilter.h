/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDiscreteGaussianImageFilter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDiscreteGaussianImageFilter_h
#define __itkDiscreteGaussianImageFilter_h

#include "itkImageToImageFilter.h"
#include "itkImage.h"

namespace itk
{
/**
 * \class DiscreteGaussianImageFilter
 * \brief Blurs an image by separable convolution with discrete gaussian kernels.
 * This filter performs Gaussian blurring by separable convolution of an image
 * and a discrete Gaussian operator (kernel).
 *
 * The Gaussian operator used here was described by Tony Lindeberg (Discrete
 * Scale-Space Theory and the Scale-Space Primal Sketch.  Dissertation. Royal
 * Institute of Technology, Stockholm, Sweden. May 1991.)
 *
 * \sa GaussianOperator
 * \sa Image
 * \sa Neighborhood
 * \sa NeighborhoodOperator
 * 
 * \ingroup ImageEnhancement 
 * \ingroup ImageFeatureExtraction 
 */

template <class TInputImage, class TOutputImage >
class ITK_EXPORT DiscreteGaussianImageFilter :
    public ImageToImageFilter< TInputImage, TOutputImage > 
{
public:
  /** Standard class typedefs. */
  typedef DiscreteGaussianImageFilter Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(DiscreteGaussianImageFilter, ImageToImageFilter);
  
  /** Image type information. */
  typedef TInputImage  InputImageType;
  typedef TOutputImage OutputImageType;

  /** Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same. */
  typedef typename TOutputImage::PixelType OutputPixelType;
  typedef typename TOutputImage::InternalPixelType OutputInternalPixelType;
  typedef typename TInputImage::PixelType InputPixelType;
  typedef typename TInputImage::InternalPixelType InputInternalPixelType;

  /** Extract some information from the image types.  Dimensionality
   * of the two images is assumed to be the same. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TOutputImage::ImageDimension);
  
  /** Standard get/set macros for filter parameters. */
  itkSetVectorMacro(Variance, double, ImageDimension);
  itkSetVectorMacro(Variance, float, ImageDimension);
  itkGetVectorMacro(Variance, const double, ImageDimension);
  itkSetVectorMacro(MaximumError, double, ImageDimension);
  itkSetVectorMacro(MaximumError, float, ImageDimension);
  itkGetVectorMacro(MaximumError, const double, ImageDimension);
  itkGetMacro(MaximumKernelWidth, int);
  itkSetMacro(MaximumKernelWidth, int);
  itkGetMacro(FilterDimensionality, unsigned int);
  itkSetMacro(FilterDimensionality, unsigned int);
  
  /** Convenience get/set methods for setting all dimensional parameters to the
   * same values.  */
  void SetVariance(const double v)
    {
    double vArray[ImageDimension];
    for (unsigned int i = 0; i<ImageDimension; ++i) { vArray[i] = v; }
    this->SetVariance(vArray);
    }
  void SetMaximumError(const double v)
    {
    double vArray[ImageDimension];
    for (unsigned int i = 0; i<ImageDimension; ++i) { vArray[i] = v; }
    this->SetMaximumError(vArray);
    }

  /** Convenience get/set methods for setting all dimensional parameters to the
   * same values.  */
  void SetVariance(const float v)
    {
    double vArray[ImageDimension];
    for (unsigned int i = 0; i<ImageDimension; ++i) { vArray[i] = static_cast<double>(v); }
    this->SetVariance(vArray);
    }
  void SetMaximumError(const float v)
    {
    double vArray[ImageDimension];
    for (unsigned int i = 0; i<ImageDimension; ++i) { vArray[i] = static_cast<double>(v); }
    this->SetMaximumError(vArray);
    }
  
  /** DiscreteGaussianImageFilter needs a larger input requested region
   * than the output requested region (larger by the size of the
   * Gaussian kernel).  As such, DiscreteGaussianImageFilter needs to
   * provide an implementation for GenerateInputRequestedRegion() in
   * order to inform the pipeline execution model.
   * \sa ImageToImageFilter::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion() throw(InvalidRequestedRegionError);

protected:
  DiscreteGaussianImageFilter()
    {
    unsigned int i;
    for (i = 0; i < ImageDimension; i++)
      {
      m_Variance[i] = 0.0f;
      m_MaximumError[i] = 0.01f;
      m_MaximumKernelWidth = 32;
      }
    m_FilterDimensionality = ImageDimension;
    }
  virtual ~DiscreteGaussianImageFilter() {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  /** Standard pipeline method. While this class does not implement a
   * ThreadedGenerateData(), its GenerateData() delegates all
   * calculations to an NeighborhoodOperatorImageFilter.  Since the
   * NeighborhoodOperatorImageFilter is multithreaded, this filter is
   * multithreaded by default. */
  void GenerateData();

  
private:
  DiscreteGaussianImageFilter(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** The variance of the gaussian blurring kernel in each dimensional direction. */
  double m_Variance[ImageDimension];

  /** The maximum error of the gaussian blurring kernel in each dimensional
   * direction. For definition of maximum error, see GaussianOperator.
   * \sa GaussianOperator */
  double m_MaximumError[ImageDimension];

  /** Maximum allowed kernel width for any dimension of the discrete Gaussian
      approximation */
  int m_MaximumKernelWidth;

  /** Number of dimensions to process. Default is all dimensions */
  unsigned int m_FilterDimensionality;
};
  
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDiscreteGaussianImageFilter.txx"
#endif

#endif
