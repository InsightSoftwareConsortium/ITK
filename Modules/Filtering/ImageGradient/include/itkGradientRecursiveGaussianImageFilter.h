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
#ifndef itkGradientRecursiveGaussianImageFilter_h
#define itkGradientRecursiveGaussianImageFilter_h

#include "itkRecursiveGaussianImageFilter.h"
#include "itkNthElementImageAdaptor.h"
#include "itkImage.h"
#include "itkCovariantVector.h"
#include "itkDefaultConvertPixelTraits.h"
#include "itkProgressAccumulator.h"
#include "itkImageRegionIterator.h"
#include "itkVectorImage.h"
#include <vector>

namespace itk
{
/** \class GradientRecursiveGaussianImageFilter
 * \brief Computes the gradient of an image by convolution
 *        with the first derivative of a Gaussian.
 *
 * This filter is implemented using the recursive gaussian
 * filters.
 *
 * This filter supports both scalar and vector pixel types
 * within the input image, including VectorImage type.
 *
 * \ingroup GradientFilters
 * \ingroup SingleThreaded
 * \ingroup ITKImageGradient
 *
 * \wiki
 * \wikiexample{EdgesAndGradients/GradientRecursiveGaussianImageFilter,Compute the gradient of an image by convolution with the first derivative of a Gaussian}
 * \endwiki
 */
template< typename TInputImage,
          typename TOutputImage = Image< CovariantVector<
                                           typename NumericTraits< typename TInputImage::PixelType >::RealType,
                                           TInputImage::ImageDimension >,
                                         TInputImage::ImageDimension > >
class ITK_TEMPLATE_EXPORT GradientRecursiveGaussianImageFilter:
  public ImageToImageFilter< TInputImage, TOutputImage >
{
public:
  /** Standard class typedefs. */
  typedef GradientRecursiveGaussianImageFilter            Self;
  typedef ImageToImageFilter< TInputImage, TOutputImage > Superclass;
  typedef SmartPointer< Self >                            Pointer;
  typedef SmartPointer< const Self >                      ConstPointer;

  /** Pixel Type of the input image. May be scalar or vector. */
  typedef TInputImage                                           InputImageType;
  typedef typename TInputImage::PixelType                       PixelType;
  typedef typename NumericTraits< PixelType >::RealType         RealType;
  typedef typename NumericTraits< PixelType >::ScalarRealType   ScalarRealType;

  /** Define the image type for internal computations
      RealType is usually 'double' in NumericTraits.
      Here we prefer float in order to save memory.  */
  typedef typename NumericTraits< RealType >::FloatType         InternalRealType;
  typedef typename NumericTraits< InternalRealType >::ValueType InternalScalarRealType;

  /** Image dimension. */
  itkStaticConstMacro(ImageDimension, unsigned int,
                      TInputImage::ImageDimension);

  /** Gradient vector typedef */
  typedef CovariantVector<ScalarRealType, ImageDimension > GradientVectorType;

  /** Define the image type for internal computations
      RealType is usually 'double' in NumericTraits.
      Here we prefer float in order to save memory.  */
  typedef Image< InternalRealType,
                 itkGetStaticConstMacro(ImageDimension) >   RealImageType;


  /**  Output Image Nth Element Adaptor
   *  This adaptor allows to use conventional scalar
   *  smoothing filters to compute each one of the
   *  components of the gradient image pixels. */
  typedef NthElementImageAdaptor< TOutputImage,
                                  InternalScalarRealType >  OutputImageAdaptorType;

  typedef typename OutputImageAdaptorType::Pointer OutputImageAdaptorPointer;

  /** Define the type for the sigma array **/
  typedef FixedArray< ScalarRealType,
                      itkGetStaticConstMacro(ImageDimension) > SigmaArrayType;

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

  /**  Pointer to a gaussian filter.  */
  typedef typename GaussianFilterType::Pointer GaussianFilterPointer;

  /**  Pointer to a derivative filter.  */
  typedef typename DerivativeFilterType::Pointer DerivativeFilterPointer;

  /**  Pointer to the Output Image */
  typedef typename TOutputImage::Pointer OutputImagePointer;

  /** Type of the output Image */
  typedef TOutputImage                                         OutputImageType;
  typedef typename OutputImageType::PixelType                  OutputPixelType;
  typedef typename NumericTraits< OutputPixelType >::ValueType OutputComponentType;
  typedef CovariantVector< OutputComponentType, ImageDimension >
    CovariantVectorType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(GradientRecursiveGaussianImageFilter,
               ImageToImageFilter);

  /** Set Sigma value. Sigma is measured in the units of image spacing. */
  void SetSigmaArray(const SigmaArrayType & sigmas);
  void SetSigma(ScalarRealType sigma);

  SigmaArrayType GetSigmaArray() const;
  ScalarRealType GetSigma() const;

  /** Define which normalization factor will be used for the Gaussian
   *  \sa  RecursiveGaussianImageFilter::SetNormalizeAcrossScale
   */
  void SetNormalizeAcrossScale(bool normalizeInScaleSpace);
  itkGetConstMacro(NormalizeAcrossScale, bool);

  /** GradientRecursiveGaussianImageFilter needs all of the input to produce an
   * output. Therefore, GradientRecursiveGaussianImageFilter needs to provide
   * an implementation for GenerateInputRequestedRegion in order to inform
   * the pipeline execution model.
   * \sa ImageToImageFilter::GenerateInputRequestedRegion() */
  virtual void GenerateInputRequestedRegion() ITK_OVERRIDE;

  /** The UseImageDirection flag determines whether the gradients are
   * computed with respect to the image grid or with respect to the physical
   * space. When this flag is ON the gradients are computed with respect to
   * the coordinate system of physical space. The difference is whether we take
   * into account the image Direction or not. The flag ON will take into
   * account the image direction and will result in an extra matrix
   * multiplication compared to the amount of computation performed when the
   * flag is OFF.
   * The default value of this flag is On.
   */
  itkSetMacro(UseImageDirection, bool);
  itkGetConstMacro(UseImageDirection, bool);
  itkBooleanMacro(UseImageDirection);

#ifdef ITK_USE_CONCEPT_CHECKING
  // Begin concept checking
  // Does not seem to work with wrappings, disabled
  // itkConceptMacro( InputHasNumericTraitsCheck,
  //                 ( Concept::HasNumericTraits< PixelType > ) );
  // End concept checking
#endif

protected:
  GradientRecursiveGaussianImageFilter();
  virtual ~GradientRecursiveGaussianImageFilter() ITK_OVERRIDE {}
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Generate Data */
  void GenerateData(void) ITK_OVERRIDE;

  // Override since the filter produces the entire dataset
  void EnlargeOutputRequestedRegion(DataObject *output) ITK_OVERRIDE;

  void GenerateOutputInformation() ITK_OVERRIDE;

private:

  template <typename TValue>
  void TransformOutputPixel( ImageRegionIterator< VectorImage<TValue, ImageDimension> > &it )
  {
    // To transform Variable length vector we need to convert to and
    // fro the CovariantVectorType
    const CovariantVectorType gradient( it.Get().GetDataPointer() );
    CovariantVectorType physicalGradient;
    it.GetImage()->TransformLocalVectorToPhysicalVector(gradient, physicalGradient );
    it.Set( OutputPixelType( physicalGradient.GetDataPointer(), ImageDimension, false ) );
  }

  template <typename T >
  void TransformOutputPixel( ImageRegionIterator< T > &it )
  {
    OutputPixelType correctedGradient;
    const OutputPixelType & gradient = it.Get();

    const unsigned int nComponents = NumericTraits<OutputPixelType>::GetLength( gradient )/ImageDimension;

    for (unsigned int nc = 0; nc < nComponents; nc++ )
      {
      GradientVectorType componentGradient;
      GradientVectorType correctedComponentGradient;
      for (unsigned int dim = 0; dim < ImageDimension; dim++ )
        {
        componentGradient[dim] = DefaultConvertPixelTraits<OutputPixelType>::GetNthComponent( nc*ImageDimension+dim, gradient );
        }
      it.GetImage()->TransformLocalVectorToPhysicalVector(componentGradient, correctedComponentGradient );
      for (unsigned int dim = 0; dim < ImageDimension; dim++ )
        {
        DefaultConvertPixelTraits<OutputPixelType>::SetNthComponent( nc*ImageDimension+dim, correctedGradient,
                                                                     correctedComponentGradient[dim] );
        }
      }
    it.Set(correctedGradient);
  }

  template <template<typename, unsigned int> class P, class T, unsigned int N>
    void TransformOutputPixel( ImageRegionIterator< Image< P<T,N>, N > > &it )
  {
    const OutputPixelType gradient = it.Get();
    // This uses the more efficient set by reference method
    it.GetImage()->TransformLocalVectorToPhysicalVector(gradient, it.Value() );
  }


  ITK_DISALLOW_COPY_AND_ASSIGN(GradientRecursiveGaussianImageFilter);

  std::vector< GaussianFilterPointer > m_SmoothingFilters;
  DerivativeFilterPointer              m_DerivativeFilter;
  OutputImageAdaptorPointer            m_ImageAdaptor;

  /** Normalize the image across scale space */
  bool m_NormalizeAcrossScale;

  /** Take into account image orientation when computing the Gradient */
  bool m_UseImageDirection;

  /** Standard deviation of the gaussian */
  SigmaArrayType m_Sigma;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkGradientRecursiveGaussianImageFilter.hxx"
#endif

#endif
