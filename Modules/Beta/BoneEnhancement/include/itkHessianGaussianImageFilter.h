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

#ifndef itkHessianGaussianImageFilter_h
#define itkHessianGaussianImageFilter_h

#include "itkDiscreteGaussianDerivativeImageFilter.h"
#include "itkNthElementImageAdaptor.h"
#include "itkImage.h"
#include "itkSymmetricSecondRankTensor.h"
#include "itkPixelTraits.h"

namespace itk {
/** \class HessianGaussianImageFilter
 * \brief Computes the Hessian matrix of an image by convolution
 *        with the Second and Cross derivatives of a Gaussian
 *        with streaming.
 * 
 * This filter is implemented using the discrete gaussian
 * filters to enable streaming. Although IIR filters are faster
 * than FIR filters, IIR filters cannot be streamed. FIR filters
 * are slower but can be streamed for small memory computers.
 * 
 * This class is an exact copy of HessianRecursiveGaussianImageFilter
 * but with streaming.
 * 
 * \sa HessianRecursiveGaussianImageFilter.
 * 
 * \author: Bryce Besler
 * \ingroup BoneEnhancement
 */

template< typename TInputImage,
          typename TOutputImage = Image<  SymmetricSecondRankTensor<
                                          typename NumericTraits< typename TInputImage::PixelType >::RealType,
                                          TInputImage::ImageDimension >,
                                          TInputImage::ImageDimension > >
class ITK_TEMPLATE_EXPORT HessianGaussianImageFilter:
public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard Self typedef */
  typedef HessianGaussianImageFilter                      Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Pixel Type of the input image */
  typedef TInputImage                                   InputImageType;
  typedef typename TInputImage::PixelType               PixelType;
  typedef typename NumericTraits< PixelType >::RealType RealType;

  /** Image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int, TInputImage::ImageDimension);

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

  /**  Derivative filter type */
  typedef DiscreteGaussianDerivativeImageFilter<
    InputImageType,
    RealImageType
    >    DerivativeFilterType;

  /**  Pointer to a gaussian filter.  */
  typedef typename DerivativeFilterType::Pointer DerivativeFilterPointer;

  /**  Pointer to the Output Image */
  typedef typename TOutputImage::Pointer OutputImagePointer;

  /** Type of the output Image */
  typedef TOutputImage                                       OutputImageType;
  typedef typename          OutputImageType::PixelType       OutputPixelType;
  typedef typename PixelTraits< OutputPixelType >::ValueType OutputComponentType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(HessianGaussianImageFilter, ImageToImageFilter);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Set Sigma value. Sigma is measured in the units of image spacing.  */
  void SetSigma(RealType sigma);
  RealType GetSigma() const;

  /** Define which normalization factor will be used for the Gaussian
   *  \sa  DiscreteGaussianDerivativeImageFilter::SetNormalizeAcrossScale
   */
  void SetNormalizeAcrossScale(bool normalizeInScaleSpace);
  bool GetNormalizeAcrossScale() const;
  itkBooleanMacro(NormalizeAcrossScale);

  /** As opposed to HessianRecursiveGaussianImageFilter, HessianGaussianImageFilter
   * doe not need all of the input to produce an output. However, it does need to
   * expand the InputRequestedRegion region to account for the support of the
   * Gaussian filter.
   * \sa DiscreteGaussianDerivativeImageFilter::GenerateInputRequestedRegion() */
  // TODO
  virtual void GenerateInputRequestedRegion()
  throw( InvalidRequestedRegionError ) ITK_OVERRIDE;

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro( InputHasNumericTraitsCheck, ( Concept::HasNumericTraits< PixelType > ) );
  itkConceptMacro( OutputHasPixelTraitsCheck,  ( Concept::HasPixelTraits< OutputPixelType > ) );
  /** End concept checking */
#endif

protected:
  HessianGaussianImageFilter();
  virtual ~HessianGaussianImageFilter() {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Generate Data */
  void GenerateData(void) ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(HessianGaussianImageFilter);

  /** Internal filters **/
  DerivativeFilterPointer   m_DerivativeFilter;
  OutputImageAdaptorPointer m_ImageAdaptor;
}; //end class
} // end namespace 

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkHessianGaussianImageFilter.hxx"
#endif

#endif // itkHessianGaussianImageFilter_h
