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
#ifndef itkHessianRecursiveGaussianImageFilter_h
#define itkHessianRecursiveGaussianImageFilter_h

#include "itkRecursiveGaussianImageFilter.h"
#include "itkNthElementImageAdaptor.h"
#include "itkImage.h"
#include "itkSymmetricSecondRankTensor.h"
#include "itkPixelTraits.h"

namespace itk
{
/** \class HessianRecursiveGaussianImageFilter
 * \brief Computes the Hessian matrix of an image by convolution
 *        with the Second and Cross derivatives of a Gaussian.
 *
 * This filter is implemented using the recursive gaussian
 * filters
 *
 *
 * \ingroup GradientFilters
 * \ingroup SingleThreaded
 * \ingroup ITKImageFeature
 */
// NOTE that the typename macro has to be used here in lieu
// of "typename" because VC++ doesn't like the typename keyword
// on the defaults of template parameters
template< typename TInputImage,
          typename TOutputImage = Image< SymmetricSecondRankTensor<
                                           typename NumericTraits< typename TInputImage::PixelType >::RealType,
                                           TInputImage::ImageDimension >,
                                         TInputImage::ImageDimension > >
class ITK_TEMPLATE_EXPORT HessianRecursiveGaussianImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef HessianRecursiveGaussianImageFilter             Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Pixel Type of the input image */
  typedef TInputImage                                   InputImageType;
  typedef typename TInputImage::PixelType               PixelType;
  typedef typename NumericTraits< PixelType >::RealType RealType;

  /** Image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Number of smoothing filters. */
  itkStaticConstMacro(NumberOfSmoothingFilters, unsigned int,
                      TInputImage::ImageDimension > 2 ? TInputImage::ImageDimension - 2 : 0);

  /** Define the image type for internal computations
      RealType is usually 'double' in NumericTraits.
      Here we prefer float in order to save memory.  */

  typedef float InternalRealType;
  typedef Image<InternalRealType, TInputImage::ImageDimension >
                RealImageType;

  /**  Output Image Nth Element Adaptor
   *  This adaptor allows to use conventional scalar
   *  smoothing filters to compute each one of the
   *  components of the gradient image pixels. */
  typedef NthElementImageAdaptor< TOutputImage,
                                  InternalRealType >  OutputImageAdaptorType;

  typedef typename OutputImageAdaptorType::Pointer OutputImageAdaptorPointer;

  /**  Smoothing filter type */
  typedef RecursiveGaussianImageFilter<
    RealImageType,
    RealImageType
    >    GaussianFilterType;

  /**  Derivative filter type, it will be the first in the pipeline  */
  typedef RecursiveGaussianImageFilter<
    InputImageType,
    RealImageType
    >    DerivativeFilterAType;

  typedef RecursiveGaussianImageFilter<
    RealImageType,
    RealImageType
    >    DerivativeFilterBType;

  /**  Pointer to a gaussian filter.  */
  typedef typename GaussianFilterType::Pointer GaussianFilterPointer;
  typedef std::vector< GaussianFilterPointer > GaussianFiltersArray;

  /**  Pointer to a derivative filter.  */
  typedef typename DerivativeFilterAType::Pointer DerivativeFilterAPointer;
  typedef typename DerivativeFilterBType::Pointer DerivativeFilterBPointer;

  /**  Pointer to the Output Image */
  typedef typename TOutputImage::Pointer OutputImagePointer;

  /** Type of the output Image */
  typedef TOutputImage                                       OutputImageType;
  typedef typename          OutputImageType::PixelType       OutputPixelType;
  typedef typename PixelTraits< OutputPixelType >::ValueType OutputComponentType;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(HessianRecursiveGaussianImageFilter, ImageToImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Set Sigma value. Sigma is measured in the units of image spacing.  */
  void SetSigma(RealType sigma);
  RealType GetSigma() const;

  /** Define which normalization factor will be used for the Gaussian
   *  \sa  RecursiveGaussianImageFilter::SetNormalizeAcrossScale
   */
  void SetNormalizeAcrossScale(bool normalizeInScaleSpace);
  itkGetConstMacro(NormalizeAcrossScale, bool);

  /** HessianRecursiveGaussianImageFilter needs all of the input to produce an
   * output. Therefore, HessianRecursiveGaussianImageFilter needs to provide
   * an implementation for GenerateInputRequestedRegion in order to inform
   * the pipeline execution model.
   * \sa ImageToImageFilter::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion() ITK_OVERRIDE;

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  itkConceptMacro( InputHasNumericTraitsCheck,
                   ( Concept::HasNumericTraits< PixelType > ) );
  itkConceptMacro( OutputHasPixelTraitsCheck,
                   ( Concept::HasPixelTraits< OutputPixelType > ) );
  // End concept checking
#endif

protected:

  HessianRecursiveGaussianImageFilter();
  virtual ~HessianRecursiveGaussianImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Generate Data */
  void GenerateData(void) ITK_OVERRIDE;

  // Override since the filter produces the entire dataset
  void EnlargeOutputRequestedRegion(DataObject *output) ITK_OVERRIDE;

private:

  ITK_DISALLOW_COPY_AND_ASSIGN(HessianRecursiveGaussianImageFilter);

  GaussianFiltersArray      m_SmoothingFilters;
  DerivativeFilterAPointer  m_DerivativeFilterA;
  DerivativeFilterBPointer  m_DerivativeFilterB;
  OutputImageAdaptorPointer m_ImageAdaptor;

  /** Normalize the image across scale space */
  bool m_NormalizeAcrossScale;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkHessianRecursiveGaussianImageFilter.hxx"
#endif

#endif
