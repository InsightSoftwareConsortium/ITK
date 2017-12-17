/*=========================================================================
 *
 * Copyright Insight Software Consortium
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 *=========================================================================*/
#pragma once

#ifndef _ITK_SMOOTHING_RECURSIVE_YVV_GAUSSIAN_IMAGE_FILTER_H_
#  define _ITK_SMOOTHING_RECURSIVE_YVV_GAUSSIAN_IMAGE_FILTER_H_

#  include "itkRecursiveLineYvvGaussianImageFilter.h"
#  include "itkCastImageFilter.h"
#  include "itkImage.h"
#  include "itkPixelTraits.h"
#  include "itkCommand.h"
#  include "itkFixedArray.h"

namespace itk
{
/**
 * \class SmoothingRecursiveYvvGaussianImageFilter
 * \brief Recursive Gaussian blurring filter based on Young-Van Vliet's
 *  algorithm, implemented for CPU.
 *
 *  This CPU implementation is more efficient than the GPU implamentation for
 *  smaller images (e.g. 512x512 and smaller for quadcores at over 3GHz); use
 *  the benchmark tests to establish the size for which this implementation
 *  performs better for your particular hardware configuration.
 *
 *  More information in the Insight Journal publication:
 *  http://hdl.handle.net/10380/3425
 *
 * \ingroup SmoothingRecursiveYvvGaussianFilter
 */
template <typename TInputImage, typename TOutputImage = TInputImage>
class ITK_EXPORT SmoothingRecursiveYvvGaussianImageFilter : public InPlaceImageFilter<TInputImage, TOutputImage>
{
public:
  /** Standard class typedefs. */
  typedef SmoothingRecursiveYvvGaussianImageFilter      Self;
  typedef InPlaceImageFilter<TInputImage, TOutputImage> Superclass;
  typedef SmartPointer<Self>                            Pointer;
  typedef SmartPointer<const Self>                      ConstPointer;

  /** Pixel Type of the input image */
  typedef TInputImage                     InputImageType;
  typedef TOutputImage                    OutputImageType;
  typedef typename TInputImage::PixelType PixelType;
#  ifdef WITH_DOUBLE
  typedef typename NumericTraits<PixelType>::RealType       RealType;
  typedef typename NumericTraits<PixelType>::ScalarRealType ScalarRealType;
#  else
  typedef typename NumericTraits<PixelType>::FloatType RealType;
  typedef typename NumericTraits<PixelType>::FloatType ScalarRealType;
#  endif

  /** Runtime information support. */
  itkTypeMacro(SmoothingRecursiveYvvGaussianImageFilter, InPlaceImageFilter);

  /** Image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int, TInputImage::ImageDimension);

  /** Define the type for the sigma array */
  typedef FixedArray<ScalarRealType, itkGetStaticConstMacro(ImageDimension)> SigmaArrayType;

  /** Define the image type for internal computations
   RealType is usually 'double' in NumericTraits.
   Here we prefer float in order to save memory.  */

  typedef typename NumericTraits<PixelType>::FloatType                     InternalRealType;
  typedef typename InputImageType::template Rebind<InternalRealType>::Type RealImageType;

  /**  The first in the pipeline  */
  typedef RecursiveLineYvvGaussianImageFilter<InputImageType, RealImageType> FirstGaussianFilterType;

  /**  Smoothing filter type */
  typedef RecursiveLineYvvGaussianImageFilter<RealImageType, RealImageType> InternalGaussianFilterType;

  /**  The last in the pipeline  */
  typedef CastImageFilter<RealImageType, OutputImageType> CastingFilterType;

  /**  Pointer to a gaussian filter.  */
  typedef typename InternalGaussianFilterType::Pointer InternalGaussianFilterPointer;

  /**  Pointer to the first gaussian filter.  */
  typedef typename FirstGaussianFilterType::Pointer FirstGaussianFilterPointer;

  /**  Pointer to the last filter, casting  */
  typedef typename CastingFilterType::Pointer CastingFilterPointer;

  /**  Pointer to the Output Image */
  typedef typename OutputImageType::Pointer OutputImagePointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Set Sigma value. Sigma is measured in the units of image spacing. You
   may use the method SetSigma to set the same value across each axis or
   use the method SetSigmaArray if you need different values along each
   axis. */
  void
  SetSigmaArray(const SigmaArrayType & sigmas);

  void
  SetSigma(ScalarRealType sigma);

  SigmaArrayType
  GetSigmaArray() const;

  ScalarRealType
  GetSigma() const;

  /** Define which normalization factor will be used for the Gaussian */
  void
  SetNormalizeAcrossScale(bool normalizeInScaleSpace);

  itkGetConstMacro(NormalizeAcrossScale, bool);

  void
  SetNumberOfThreads(ThreadIdType nb) ITK_OVERRIDE;

  // See super class for doxygen documentation
  //
  bool
  CanRunInPlace(void) const ITK_OVERRIDE;

#  ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(InputHasNumericTraitsCheck, (Concept::HasNumericTraits<PixelType>));
  /** End concept checking */
#  endif
protected:
  SmoothingRecursiveYvvGaussianImageFilter();
  ~SmoothingRecursiveYvvGaussianImageFilter() ITK_OVERRIDE {}
  void
  PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Generate Data */
  void
  GenerateData(void) ITK_OVERRIDE;

  /** SmoothingRecursiveYvvGaussianImageFilter needs all of the input to produce an
   * output. Therefore, SmoothingRecursiveYvvGaussianImageFilter needs to provide
   * an implementation for GenerateInputRequestedRegion in order to inform
   * the pipeline execution model.
   * \sa ImageToImageFilter::GenerateInputRequestedRegion() */
  void
  GenerateInputRequestedRegion() throw (InvalidRequestedRegionError)ITK_OVERRIDE;

  // Override since the filter produces the entire dataset
  void
  EnlargeOutputRequestedRegion(DataObject * output) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(SmoothingRecursiveYvvGaussianImageFilter);

  InternalGaussianFilterPointer m_SmoothingFilters[ImageDimension - 1];
  FirstGaussianFilterPointer    m_FirstSmoothingFilter;
  CastingFilterPointer          m_CastingFilter;

  /** Normalize the image across scale space */
  bool m_NormalizeAcrossScale;

  /** Standard deviation of the gaussian used for smoothing */
  SigmaArrayType m_Sigma;
  int            telltale; // TODO: REMOVE
};
} // end namespace itk

#  ifndef ITK_MANUAL_INSTANTIATION
#    include "itkSmoothingRecursiveYvvGaussianImageFilter.hxx"
#  endif

#endif
