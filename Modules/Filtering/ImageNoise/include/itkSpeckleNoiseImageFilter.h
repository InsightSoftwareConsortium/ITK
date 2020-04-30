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

#ifndef itkSpeckleNoiseImageFilter_h
#define itkSpeckleNoiseImageFilter_h

#include "itkNoiseBaseImageFilter.h"

namespace itk
{

/**
 *\class SpeckleNoiseImageFilter
 *
 * \brief Alter an image with speckle (multiplicative) noise.
 *
 * The speckle noise follows a gamma distribution of mean 1 and standard deviation
 * provided by the user. The noise is proportional to the pixel intensity.
 *
 * It can be modeled as:
 *
 * \par
 * \f$ I = I_0 \ast G \f$
 *
 * \par
 * where \f$ G \f$ is a is a gamma distributed random variable of mean 1 and
 * variance proportional to the noise level:
 *
 * \par
 * \f$ G \sim \Gamma(\frac{1}{\sigma^2}, \sigma^2) \f$
 *
 * \author Gaetan Lehmann
 *
 * This code was contributed in the Insight Journal paper "Noise
 * Simulation". https://hdl.handle.net/10380/3158
 *
 * \ingroup ITKImageNoise
 */
template <class TInputImage, class TOutputImage = TInputImage>
class ITK_TEMPLATE_EXPORT SpeckleNoiseImageFilter : public NoiseBaseImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(SpeckleNoiseImageFilter);

  /** Standard class type aliases. */
  using Self = SpeckleNoiseImageFilter;
  using Superclass = NoiseBaseImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SpeckleNoiseImageFilter, NoiseBaseImageFilter);

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

  /** Set/Get the standard deviation of the gamma distribution.
   * Defaults to 1.0. */
  itkGetConstMacro(StandardDeviation, double);
  itkSetMacro(StandardDeviation, double);

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(InputConvertibleToOutputCheck,
                  (Concept::Convertible<typename TInputImage::PixelType, typename TOutputImage::PixelType>));
  /** End concept checking */
#endif

protected:
  SpeckleNoiseImageFilter();
  ~SpeckleNoiseImageFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread, ThreadIdType threadId) override;

private:
  double m_StandardDeviation{ 1.0 };
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSpeckleNoiseImageFilter.hxx"
#endif

#endif
