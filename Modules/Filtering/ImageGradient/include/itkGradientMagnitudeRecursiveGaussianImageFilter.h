/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkGradientMagnitudeRecursiveGaussianImageFilter_h
#define itkGradientMagnitudeRecursiveGaussianImageFilter_h

#include "itkNthElementImageAdaptor.h"
#include "itkImage.h"
#include "itkPixelTraits.h"
#include "itkRecursiveGaussianImageFilter.h"
#include "itkSqrtImageFilter.h"
#include "itkBinaryFunctorImageFilter.h"

namespace itk
{
/** \class GradientMagnitudeRecursiveGaussianImageFilter
 * \brief Computes the Magnitude of the Gradient of an image by convolution
 *        with the first derivative of a Gaussian.
 *
 * This filter is implemented using the recursive gaussian
 * filters
 *
 * \ingroup GradientFilters
 * \ingroup SingleThreaded
 * \ingroup ITKImageGradient
 *
 * \wiki
 * \wikiexample{EdgesAndGradients/GradientMagnitudeRecursiveGaussianImageFilter,Find the gradient magnitude of the image first smoothed with a Gaussian kernel}
 * \endwiki
 */
// NOTE that the typename macro has to be used here in lieu
// of "typename" because VC++ doesn't like the typename keyword
// on the defaults of template parameters
template< typename TInputImage,
          typename TOutputImage = TInputImage >
class ITK_TEMPLATE_EXPORT GradientMagnitudeRecursiveGaussianImageFilter:
  public InPlaceImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef GradientMagnitudeRecursiveGaussianImageFilter   Self;
  typedef InPlaceImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Pixel Type of the input image */
  typedef TInputImage                        InputImageType;
  typedef typename InputImageType::PixelType PixelType;

  /** Image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  typedef typename NumericTraits< PixelType >::RealType RealType;

  /** Define the image type for internal computations
      RealType is usually 'double' in NumericTraits.
      Here we prefer float in order to save memory.  */

  typedef float InternalRealType;
  typedef Image< InternalRealType,
                 itkGetStaticConstMacro(ImageDimension) >   RealImageType;

  /**  Smoothing filter type */
  typedef RecursiveGaussianImageFilter<
    RealImageType,
    RealImageType
    >    GaussianFilterType;

  /**  Derivative filter type, it will be the first in the pipeline  */
  typedef RecursiveGaussianImageFilter<
    InputImageType,
    RealImageType
    >    DerivativeFilterType;

  /**  Smoothing filter type */
  typedef SqrtImageFilter<
    RealImageType,
    TOutputImage
    >    SqrtFilterType;

  /**  Pointer to a gaussian filter.  */
  typedef typename GaussianFilterType::Pointer GaussianFilterPointer;

  /**  Pointer to a derivative filter.  */
  typedef typename DerivativeFilterType::Pointer DerivativeFilterPointer;

  typedef typename SqrtFilterType::Pointer SqrtFilterPointer;

  /**  Pointer to the Output Image */
  typedef typename TOutputImage::Pointer OutputImagePointer;

  /** Type of the output Image */
  typedef TOutputImage                        OutputImageType;
  typedef typename OutputImageType::PixelType OutputPixelType;

  /**  Auxiliary image for holding the values of the squared gradient components
    */
  typedef Image< InternalRealType,
                 itkGetStaticConstMacro(ImageDimension) >      CumulativeImageType;
  typedef typename CumulativeImageType::Pointer CumulativeImagePointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(GradientMagnitudeRecursiveGaussianImageFilter,
               InPlaceImageFilter);

  /** Set Sigma value. Sigma is measured in the units of image spacing.  */
  void SetSigma(RealType sigma);
  RealType GetSigma();

  /** Define which normalization factor will be used for the Gaussian
   *  \sa  RecursiveGaussianImageFilter::SetNormalizeAcrossScale
   */
  void SetNormalizeAcrossScale(bool normalizeInScaleSpace);
  itkGetConstMacro(NormalizeAcrossScale, bool);

  void SetNumberOfThreads(ThreadIdType nb) ITK_OVERRIDE;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< PixelType > ) );
  // End concept checking
#endif

protected:
  GradientMagnitudeRecursiveGaussianImageFilter();
  virtual ~GradientMagnitudeRecursiveGaussianImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Generate Data */
  void GenerateData(void) ITK_OVERRIDE;

  /** GradientMagnitudeRecursiveGaussianImageFilter needs all of the
   * input to produce an output. Therefore,
   * GradientMagnitudeRecursiveGaussianImageFilter needs to provide an
   * implementation for GenerateInputRequestedRegion in order to
   * inform the pipeline execution model.  \sa
   * ImageToImageFilter::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion() ITK_OVERRIDE;

  /** GradientMagnitudeRecursiveGaussianImageFilter produces all of
   * the output.  Therefore, it needs to provide an implementation of
   * EnlargeOutputRequestedRegion(). */
  void EnlargeOutputRequestedRegion(DataObject *output) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(GradientMagnitudeRecursiveGaussianImageFilter);

  class SqrSpacing
  {
  public:
    SqrSpacing():m_Spacing(0) {}
    ~SqrSpacing() {}
    bool operator!=(const SqrSpacing & other) const
    {
      return !( *this == other );
    }

    bool operator==(const SqrSpacing & other) const
    {
      return other.m_Spacing == m_Spacing;
    }

    inline InternalRealType operator()(const InternalRealType & a, const InternalRealType & b)
    {
      return a + itk::Math::sqr(b / m_Spacing);
    }

    double m_Spacing;
  };

  typedef BinaryFunctorImageFilter< RealImageType, RealImageType, RealImageType, SqrSpacing > SqrSpacingFilterType;
  typedef typename SqrSpacingFilterType::Pointer                                              SqrSpacingFilterPointer;

  GaussianFilterPointer   m_SmoothingFilters[ImageDimension - 1];
  DerivativeFilterPointer m_DerivativeFilter;
  SqrSpacingFilterPointer m_SqrSpacingFilter;
  SqrtFilterPointer       m_SqrtFilter;

  /** Normalize the image across scale space */
  bool m_NormalizeAcrossScale;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGradientMagnitudeRecursiveGaussianImageFilter.hxx"
#endif

#endif
