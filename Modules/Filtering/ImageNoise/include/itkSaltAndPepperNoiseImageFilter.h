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

#ifndef itkSaltAndPepperNoiseImageFilter_h
#define itkSaltAndPepperNoiseImageFilter_h

#include "itkNoiseBaseImageFilter.h"

namespace itk
{

/**
 *\class SaltAndPepperNoiseImageFilter
 *
 * \brief Alter an image with fixed value impulse noise, often called salt and pepper noise.
 *
 * Salt (sensor saturation) and pepper (dead pixels) noise is a
 * special kind of impulse noise where the value of the noise is
 * either the maximum possible value in the image or its
 * minimum. This is not necessarily the maximal/minimal possible intensity
 * value based on the pixel type. For example, the native pixel type
 * for CT is a signed 16 bit integer, but only 12 bits used, so we
 * would like to set the salt and pepper values to match this smaller intensity range
 * and not the range the pixel type represents.
 * It can be modeled as:
 *
 * \par
 * \f$ I =
 * \begin{cases}
 * M,   & \quad \text{if } U < p/2 \\
 * m,   & \quad \text{if } U > 1 - p/2 \\
 * I_0, & \quad \text{if } p/2 \geq U \leq 1 - p/2
 * \end{cases} \f$
 *
 * \par
 * where \f$ p \f$ is the probability of the noise event, \f$ U \f$ is a
 * uniformly distributed random variable in the \f$ [0,1] \f$ range, \f$ M \f$
 * is the greatest possible pixel value, and \f$ m \f$ the smallest possible
 * pixel value.
 *
 * Pixel alteration occurs at a user defined probability.
 * Salt and pepper pixels are equally distributed.
 *
 * \author Gaetan Lehmann
 *
 * This code was contributed in the Insight Journal paper "Noise
 * Simulation". https://hdl.handle.net/10380/3158
 *
 * \ingroup ITKImageNoise
 */
template <class TInputImage, class TOutputImage = TInputImage>
class ITK_TEMPLATE_EXPORT SaltAndPepperNoiseImageFilter : public NoiseBaseImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(SaltAndPepperNoiseImageFilter);

  /** Standard class type aliases. */
  using Self = SaltAndPepperNoiseImageFilter;
  using Superclass = NoiseBaseImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SaltAndPepperNoiseImageFilter, NoiseBaseImageFilter);

  /** Superclass type alias. */
  using OutputImageType = typename Superclass::OutputImageType;
  using OutputImagePointer = typename Superclass::OutputImagePointer;
  using OutputImageRegionType = typename Superclass::OutputImageRegionType;
  using OutputImagePixelType = typename Superclass::OutputImagePixelType;

  /** Some convenient type alias. */
  using InputImageType = TInputImage;
  using InputImagePointer = typename InputImageType::Pointer;
  using InputImageConstPointer = typename InputImageType::ConstPointer;
  using InputImageRegionType = typename InputImageType::RegionType;
  using InputImagePixelType = typename InputImageType::PixelType;

  /** Set/Get the probability of the salt and pepper noise event.
   * Defaults to 0.01. */
  itkGetConstMacro(Probability, double);
  itkSetMacro(Probability, double);

  /** Set/Get the salt/high pixel value.
   * Defaults to NumericTraits<OutputImagePixelType>::max(). */
  itkSetMacro(SaltValue, OutputImagePixelType);
  itkGetConstMacro(SaltValue, OutputImagePixelType);


  /** Set/Get the pepper/low pixel value.
   * Defaults to NumericTraits<OutputImagePixelType>::NonpositiveMin(). */
  itkSetMacro(PepperValue, OutputImagePixelType);
  itkGetConstMacro(PepperValue, OutputImagePixelType);


#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(InputConvertibleToOutputCheck,
                  (Concept::Convertible<typename TInputImage::PixelType, typename TOutputImage::PixelType>));
  /** End concept checking */
#endif

protected:
  SaltAndPepperNoiseImageFilter();
  ~SaltAndPepperNoiseImageFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread, ThreadIdType threadId) override;

private:
  double               m_Probability{ 0.01 };
  OutputImagePixelType m_SaltValue;
  OutputImagePixelType m_PepperValue;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSaltAndPepperNoiseImageFilter.hxx"
#endif

#endif
