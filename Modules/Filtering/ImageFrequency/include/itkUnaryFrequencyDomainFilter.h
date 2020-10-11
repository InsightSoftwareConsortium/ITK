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
#ifndef itkUnaryFrequencyDomainFilter_h
#define itkUnaryFrequencyDomainFilter_h

#include <itkInPlaceImageFilter.h>
#include <itkFrequencyFFTLayoutImageRegionIteratorWithIndex.h>

namespace itk
{
/**
 *\class UnaryFrequencyDomainFilter
 * \brief Performs a unary operation on a frequency domain image
 *
 * A frequency filtering functor needs to be supplied via one of SetFunctor() overloads.
 * The functor should take FrequencyIteratorType reference as its only parameter.
 * If functor configurability is required,
 * those parameters should be passed directly to the functor at the place of definition.
 *
 *
 * Filters in the module ITKImageFrequency work with input images in the frequency domain.
 * This filter is templated over a TFrequencyIterator depending on the
 * frequency layout of the input image.
 *
 * Images in the dual space can be acquired experimentally, from scattering experiments or other techniques.
 * In that case use FrequencyImageRegionIteratorWithIndex because
 * the layout of dual space images is the same as spatial domain images.
 *
 * Frequency-domain images can be computed from any spatial-domain applying a Fourier Transform.
 * If ForwardFFTImageFilter was used, template this filter with
 * the FrequencyFFTLayoutImageRegionIteratorWithIndex.
 * Please note that FrequencyFFTLayoutImageRegionIteratorWithIndex requires a full FFT,
 * and is not compatible with the Hermitian optimization.
 *
 * To use this filter with Hermitian (halved-frequency) FFTs, use
 * FrequencyHalfHermitianFFTLayoutImageRegionIteratorWithIndex or its const version.
 *
 * If the output of the FFT is shifted, for example after applying FFTShiftImageFilter,
 * use FrequencyShiftedFFTLayoutImageRegionIteratorWithIndex.
 *
 * \sa UnaryGeneratorImageFilter
 *
 * \ingroup ITKImageFrequency
 */
template <typename TImageType, typename TFrequencyIterator = FrequencyFFTLayoutImageRegionIteratorWithIndex<TImageType>>
class UnaryFrequencyDomainFilter : public InPlaceImageFilter<TImageType, TImageType>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(UnaryFrequencyDomainFilter);

  /** Standard class type alias. */
  using Self = UnaryFrequencyDomainFilter;
  using Superclass = InPlaceImageFilter<TImageType, TImageType>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(UnaryFrequencyDomainFilter, InPlaceImageFilter);

  /** Typedef to images */
  using ImageType = TImageType;
  using ImagePointer = typename ImageType::Pointer;
  using ImageConstPointer = typename ImageType::ConstPointer;
  using IndexType = typename TImageType::IndexType;
  using PixelType = typename TImageType::PixelType;

  /** Typedef to describe the image region type. */
  using ImageRegionType = typename TImageType::RegionType;

  static constexpr unsigned int ImageDimension = TImageType::ImageDimension;

#ifdef ITK_USE_CONCEPT_CHECKING
  itkConceptMacro(ImageTypeHasNumericTraitsCheck, (Concept::HasNumericTraits<typename TImageType::PixelType>));
#endif

  /** Frequency Iterator types */
  using FrequencyIteratorType = TFrequencyIterator;
  using FrequencyValueType = typename FrequencyIteratorType::FrequencyValueType;

  /** Set to true when the you are dealing with images in the frequency
   * domain that have been computed using RealToHalfHermitianFFT, and the
   * original image in the spatial domain was odd.
   * Only needed when using HermitianFrequencyIterator and the original
   * image was odd. **/
  itkSetMacro(ActualXDimensionIsOdd, bool);
  itkGetConstReferenceMacro(ActualXDimensionIsOdd, bool);
  itkBooleanMacro(ActualXDimensionIsOdd);

  /** Returns factor with which the current frequency should be multiplied. */
  using ConstRefFunctionType = double(const FrequencyIteratorType &);

  /** Directly modifies the frequency as needed. */
  using ValueFunctionType = void(FrequencyIteratorType &);

#if !defined(ITK_WRAPPING_PARSER)
  /** The functor returns factor with which the current frequency should be multiplied. */
  void
  SetFunctor(const std::function<ConstRefFunctionType> & f)
  {
    auto inPlaceFunctor = [f](FrequencyIteratorType & freq) { freq.Value() *= f(freq); };

    m_DynamicThreadedGenerateDataFunction = [this, inPlaceFunctor](const ImageRegionType & outputRegionForThread) {
      return this->DynamicThreadedGenerateDataWithFunctor(inPlaceFunctor, outputRegionForThread);
    };

    this->Modified();
  }

  /** The functor directly modifies the frequency as needed. */
  void
  SetFunctor(const std::function<ValueFunctionType> & f)
  {
    m_DynamicThreadedGenerateDataFunction = [this, f](const ImageRegionType & outputRegionForThread) {
      return this->DynamicThreadedGenerateDataWithFunctor(f, outputRegionForThread);
    };

    this->Modified();
  }


  /** The functor returns factor with which the current frequency should be multiplied. */
  void
  SetFunctor(ConstRefFunctionType * f)
  {
    auto inPlaceFunctor = [f](FrequencyIteratorType & freq) { freq.Value() *= f(freq); };

    m_DynamicThreadedGenerateDataFunction = [this, inPlaceFunctor](const ImageRegionType & outputRegionForThread) {
      return this->DynamicThreadedGenerateDataWithFunctor(inPlaceFunctor, outputRegionForThread);
    };

    this->Modified();
  }

  /** The functor directly modifies the frequency as needed. */
  void
  SetFunctor(ValueFunctionType * funcPointer)
  {
    m_DynamicThreadedGenerateDataFunction = [this, funcPointer](const ImageRegionType & outputRegionForThread) {
      return this->DynamicThreadedGenerateDataWithFunctor(funcPointer, outputRegionForThread);
    };

    this->Modified();
  }


  /** Set the frequency functor by a "Functor Object"
   *
   * The functor defines an operation done per scalar frequency. A single copy
   * of the argument is created an used for all threads, so the functor
   * must be concurrent thread-safe. The functor must a have an
   * operator() method which accept arguments of FrequencyIteratorType&.
   */
  template <typename TFunctor>
  void
  SetFunctor(const TFunctor & functor)
  {
    m_DynamicThreadedGenerateDataFunction = [this, functor](const ImageRegionType & outputRegionForThread) {
      return this->DynamicThreadedGenerateDataWithFunctor(functor, outputRegionForThread);
    };

    this->Modified();
  }
#endif // !defined( ITK_WRAPPING_PARSER )

protected:
  UnaryFrequencyDomainFilter();
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** UnaryFrequencyDomainFilter is implemented as a multithreaded filter.
   * Therefore, this implementation provides a DynamicThreadedGenerateData()
   * routine which is called for each processing thread.
   *
   * The template method is instantiated by the SetFunctor method, and
   * the generated code is run during the update.
   *
   * \sa ImageToImageFilter::ThreadedGenerateData(),
   *     ImageToImageFilter::GenerateData()  */
  template <typename TFunctor>
  void
  DynamicThreadedGenerateDataWithFunctor(const TFunctor &, const ImageRegionType & outputRegionForThread);
  void
  DynamicThreadedGenerateData(const ImageRegionType & outputRegionForThread) override;

private:
  std::function<void(const ImageRegionType &)> m_DynamicThreadedGenerateDataFunction;

  bool m_ActualXDimensionIsOdd{ false };
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkUnaryFrequencyDomainFilter.hxx"
#endif

#endif // itkUnaryFrequencyDomainFilter_h
