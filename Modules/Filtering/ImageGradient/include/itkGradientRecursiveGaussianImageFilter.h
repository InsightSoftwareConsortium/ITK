/*=========================================================================
 *
 *  Copyright NumFOCUS
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
/**
 *\class GradientRecursiveGaussianImageFilter
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
 * \sphinx
 * \sphinxexample{Filtering/ImageGradient/ApplyGradientRecursiveGaussianWithVectorInput,Apply
 * GradientRecursiveGaussianImageFilter on Image with Vector type}
 * \sphinxexample{Filtering/ImageGradient/ImplementationOfSnakes,Implementation Of Snakes}
 * \endsphinx
 */
template <
  typename TInputImage,
  typename TOutputImage = Image<
    CovariantVector<typename NumericTraits<typename TInputImage::PixelType>::RealType, TInputImage::ImageDimension>,
    TInputImage::ImageDimension>>
class ITK_TEMPLATE_EXPORT GradientRecursiveGaussianImageFilter : public ImageToImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(GradientRecursiveGaussianImageFilter);

  /** Standard class type aliases. */
  using Self = GradientRecursiveGaussianImageFilter;
  using Superclass = ImageToImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Pixel Type of the input image. May be scalar or vector. */
  using InputImageType = TInputImage;
  using PixelType = typename TInputImage::PixelType;
  using RealType = typename NumericTraits<PixelType>::RealType;
  using ScalarRealType = typename NumericTraits<PixelType>::ScalarRealType;

  /** Define the image type for internal computations
      RealType is usually 'double' in NumericTraits.
      Here we prefer float in order to save memory.  */
  using InternalRealType = typename NumericTraits<RealType>::FloatType;
  using InternalScalarRealType = typename NumericTraits<InternalRealType>::ValueType;

  /** Image dimension. */
  static constexpr unsigned int ImageDimension = TInputImage::ImageDimension;

  /** Gradient vector type alias */
  using GradientVectorType = CovariantVector<ScalarRealType, ImageDimension>;

  /** Define the image type for internal computations
      RealType is usually 'double' in NumericTraits.
      Here we prefer float in order to save memory.  */
  using RealImageType = Image<InternalRealType, Self::ImageDimension>;


  /**  Output Image Nth Element Adaptor
   *  This adaptor allows to use conventional scalar
   *  smoothing filters to compute each one of the
   *  components of the gradient image pixels. */
  using OutputImageAdaptorType = NthElementImageAdaptor<TOutputImage, InternalScalarRealType>;

  using OutputImageAdaptorPointer = typename OutputImageAdaptorType::Pointer;

  /** Define the type for the sigma array **/
  using SigmaArrayType = FixedArray<ScalarRealType, Self::ImageDimension>;

  /**  Smoothing filter type */
  using GaussianFilterType = RecursiveGaussianImageFilter<RealImageType, RealImageType>;

  /**  Derivative filter type, it will be the first in the pipeline  */
  using DerivativeFilterType = RecursiveGaussianImageFilter<InputImageType, RealImageType>;

  /**  Pointer to a gaussian filter.  */
  using GaussianFilterPointer = typename GaussianFilterType::Pointer;

  /**  Pointer to a derivative filter.  */
  using DerivativeFilterPointer = typename DerivativeFilterType::Pointer;

  /**  Pointer to the Output Image */
  using OutputImagePointer = typename TOutputImage::Pointer;

  /** Type of the output Image */
  using OutputImageType = TOutputImage;
  using OutputPixelType = typename OutputImageType::PixelType;
  using OutputComponentType = typename NumericTraits<OutputPixelType>::ValueType;
  using CovariantVectorType = CovariantVector<OutputComponentType, ImageDimension>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Runtime information support. */
  itkTypeMacro(GradientRecursiveGaussianImageFilter, ImageToImageFilter);

  /** Set Sigma value. Sigma is measured in the units of image spacing. */
  void
  SetSigmaArray(const SigmaArrayType & sigma);
  void
  SetSigma(ScalarRealType sigma);

  SigmaArrayType
  GetSigmaArray() const;
  ScalarRealType
  GetSigma() const;

  /** Define which normalization factor will be used for the Gaussian
   *  \sa  RecursiveGaussianImageFilter::SetNormalizeAcrossScale
   */
  void
  SetNormalizeAcrossScale(bool normalize);
  itkGetConstMacro(NormalizeAcrossScale, bool);

  /** GradientRecursiveGaussianImageFilter needs all of the input to produce an
   * output. Therefore, GradientRecursiveGaussianImageFilter needs to provide
   * an implementation for GenerateInputRequestedRegion in order to inform
   * the pipeline execution model.
   * \sa ImageToImageFilter::GenerateInputRequestedRegion() */
  void
  GenerateInputRequestedRegion() override;

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
  ~GradientRecursiveGaussianImageFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Generate Data */
  void
  GenerateData() override;

  // Override since the filter produces the entire dataset
  void
  EnlargeOutputRequestedRegion(DataObject * output) override;

  void
  GenerateOutputInformation() override;

private:
  template <typename TValue>
  void
  TransformOutputPixel(ImageRegionIterator<VectorImage<TValue, ImageDimension>> & it)
  {
    // To transform Variable length vector we need to convert to and
    // fro the CovariantVectorType
    const CovariantVectorType gradient(it.Get().GetDataPointer());
    CovariantVectorType       physicalGradient;
    it.GetImage()->TransformLocalVectorToPhysicalVector(gradient, physicalGradient);
    it.Set(OutputPixelType(physicalGradient.GetDataPointer(), ImageDimension, false));
  }

  template <typename T>
  void
  TransformOutputPixel(ImageRegionIterator<T> & it)
  {
    OutputPixelType         correctedGradient;
    const OutputPixelType & gradient = it.Get();

    const unsigned int nComponents = NumericTraits<OutputPixelType>::GetLength(gradient) / ImageDimension;

    for (unsigned int nc = 0; nc < nComponents; nc++)
    {
      GradientVectorType componentGradient;
      GradientVectorType correctedComponentGradient;
      for (unsigned int dim = 0; dim < ImageDimension; dim++)
      {
        componentGradient[dim] =
          DefaultConvertPixelTraits<OutputPixelType>::GetNthComponent(nc * ImageDimension + dim, gradient);
      }
      it.GetImage()->TransformLocalVectorToPhysicalVector(componentGradient, correctedComponentGradient);
      for (unsigned int dim = 0; dim < ImageDimension; dim++)
      {
        DefaultConvertPixelTraits<OutputPixelType>::SetNthComponent(
          nc * ImageDimension + dim, correctedGradient, correctedComponentGradient[dim]);
      }
    }
    it.Set(correctedGradient);
  }

  template <template <typename, unsigned int> class P, class T, unsigned int N>
  void
  TransformOutputPixel(ImageRegionIterator<Image<P<T, N>, N>> & it)
  {
    const OutputPixelType gradient = it.Get();
    // This uses the more efficient set by reference method
    it.GetImage()->TransformLocalVectorToPhysicalVector(gradient, it.Value());
  }


  std::vector<GaussianFilterPointer> m_SmoothingFilters;
  DerivativeFilterPointer            m_DerivativeFilter;
  OutputImageAdaptorPointer          m_ImageAdaptor;

  /** Normalize the image across scale space */
  bool m_NormalizeAcrossScale;

  /** Take into account image orientation when computing the Gradient */
  bool m_UseImageDirection;

  /** Standard deviation of the gaussian */
  SigmaArrayType m_Sigma;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkGradientRecursiveGaussianImageFilter.hxx"
#endif

#endif
