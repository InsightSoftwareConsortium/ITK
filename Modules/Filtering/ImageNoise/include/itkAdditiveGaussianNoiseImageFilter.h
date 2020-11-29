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
#ifndef itkAdditiveGaussianNoiseImageFilter_h
#define itkAdditiveGaussianNoiseImageFilter_h

#include "itkNoiseBaseImageFilter.h"

namespace itk
{

/**
 *\class AdditiveGaussianNoiseImageFilter
 *
 * \brief Alter an image with additive Gaussian white noise.
 *
 * Additive Gaussian white noise can be modeled as:
 *
 * \par
 * \f$ I = I_0 + N \f$
 *
 * \par
 * where \f$ I \f$ is the observed image, \f$ I_0 \f$ is the noise-free image
 * and \f$ N \f$ is a normally distributed random variable of mean \f$ \mu \f$
 * and variance \f$ \sigma^2 \f$:
 *
 * \par
 * \f$ N \sim \mathcal{N}(\mu, \sigma^2) \f$
 *
 * The noise is independent of the pixel intensities.
 *
 * \author Gaetan Lehmann
 *
 * This code was contributed in the Insight Journal paper "Noise
 * Simulation". https://www.insight-journal.org/browse/publication/721
 *
 * \ingroup ITKImageNoise
 */
template <class TInputImage, class TOutputImage = TInputImage>
class ITK_TEMPLATE_EXPORT AdditiveGaussianNoiseImageFilter : public NoiseBaseImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(AdditiveGaussianNoiseImageFilter);

  /** Standard class type aliases. */
  using Self = AdditiveGaussianNoiseImageFilter;
  using Superclass = NoiseBaseImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(AdditiveGaussianNoiseImageFilter, NoiseBaseImageFilter);

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

  /** Set/Get the mean of the Gaussian distribution.
   * Defaults to 0.0. */
  itkGetConstMacro(Mean, double);
  itkSetMacro(Mean, double);

  /** Set/Get the standard deviation of the Gaussian distribution.
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
  AdditiveGaussianNoiseImageFilter();
  ~AdditiveGaussianNoiseImageFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread, ThreadIdType threadId) override;


private:
  double m_Mean{ 0.0 };
  double m_StandardDeviation{ 1.0 };
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkAdditiveGaussianNoiseImageFilter.hxx"
#endif

#endif
