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
#ifndef itkShotNoiseImageFilter_h
#define itkShotNoiseImageFilter_h

#include "itkNoiseBaseImageFilter.h"

namespace itk
{

/**
 *\class ShotNoiseImageFilter
 *
 * \brief Alter an image with shot noise.
 *
 * The shot noise follows a Poisson distribution:
 *
 * \par
 * \f$ I = N(I_0) \f$
 *
 * \par
 * where \f$ N(I_0) \f$ is a Poisson-distributed random variable of mean
 * \f$ I_0 \f$. The noise is thus dependent on the pixel intensities in the
 * image.
 *
 * The intensities in the image can be scaled by a user provided value to map
 * pixel values to the actual number of particles. The scaling can be seen as
 * the inverse of the gain used during the acquisition. The noisy signal is
 * then scaled back to its input intensity range:
 *
 * \par
 * \f$ I = \frac{N(I_0 \times s)}{s} \f$
 *
 * \par
 * where \f$ s \f$ is the scale factor.
 *
 * The Poisson-distributed variable \f$ \lambda \f$ is computed by using the
 * algorithm:
 *
 * \par
 * \f$ \begin{array}{l}
 * k \leftarrow 0 \\
 * p \leftarrow 1 \\
 * \textbf{repeat} \\
 * \left\{
 * \begin{array}{l}
 * k \leftarrow k+1 \\
 * p \leftarrow p \ast U()
 * \end{array}
 * \right. \\
 * \textbf{until } p > e^{\lambda} \\
 * \textbf{return} (k)
 * \end{array} \f$
 *
 * \par
 * where \f$ U() \f$ provides a uniformly distributed random variable in the
 * interval \f$ [0,1] \f$.
 *
 * This algorithm is very inefficient for large values of \f$ \lambda \f$,
 * though. Fortunately, the Poisson distribution can be accurately approximated
 * by a Gaussian distribution of mean and variance \f$ \lambda \f$ when
 * \f$ \lambda \f$ is large enough. In this implementation, this value is
 * considered to be 50. This leads to the faster algorithm:
 *
 * \par
 * \f$ \lambda + \sqrt{\lambda} \times N()\f$
 *
 * \par
 * where \f$ N() \f$ is a normally distributed random variable of mean 0 and variance 1.
 *
 * \author Gaetan Lehmann
 *
 * This code was contributed in the Insight Journal paper "Noise
 * Simulation". https://hdl.handle.net/10380/3158
 *
 * \ingroup ITKImageNoise
 */
template <class TInputImage, class TOutputImage = TInputImage>
class ITK_TEMPLATE_EXPORT ShotNoiseImageFilter : public NoiseBaseImageFilter<TInputImage, TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(ShotNoiseImageFilter);

  /** Standard class type aliases. */
  using Self = ShotNoiseImageFilter;
  using Superclass = NoiseBaseImageFilter<TInputImage, TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(ShotNoiseImageFilter, NoiseBaseImageFilter);

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

  /** Set/Get the value to map the pixel value to the actual particle counting.
   * The scaling can be seen as the inverse of the gain used during the
   * acquisition. The noisy signal is then scaled back to its input intensity
   * range. Defaults to 1.0. */
  itkGetConstMacro(Scale, double);
  itkSetMacro(Scale, double);

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro(InputConvertibleToOutputCheck,
                  (Concept::Convertible<typename TInputImage::PixelType, typename TOutputImage::PixelType>));
  /** End concept checking */
#endif

protected:
  ShotNoiseImageFilter();
  ~ShotNoiseImageFilter() override = default;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread, ThreadIdType threadId) override;


private:
  double m_Scale{ 1.0 };
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkShotNoiseImageFilter.hxx"
#endif

#endif
