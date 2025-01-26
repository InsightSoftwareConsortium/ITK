/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkSinusoidImageSource_h
#define itkSinusoidImageSource_h

#include "itkParametricImageSource.h"
#include "itkFixedArray.h"

namespace itk
{

/** \class SinusoidImageSource
 * \brief Generate an n-dimensional image of a Sinusoid.
 *
 * \f[
 *   I(\mathbf{x}) = \cos(2\pi\sum_i \omega_i x_i + \phi)
 * \f]
 *
 * where \f$\omega_i\f$ is the frequency, in spatial units, in direction
 * \f$i\f$, and \f$\phi\f$ is a phase shift.
 *
 * \ingroup DataSources
 * \ingroup PhaseSymmetry
 */
template <typename TOutputImage>
class SinusoidImageSource : public ParametricImageSource<TOutputImage>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(SinusoidImageSource);

  /** Standard class type alias. */
  using Self = SinusoidImageSource;
  using Superclass = ParametricImageSource<TOutputImage>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Typedef for the output image type. */
  using OutputImageType = TOutputImage;

  /** Dimensionality of the output image */
  itkStaticConstMacro(ImageDimension, unsigned int, TOutputImage::ImageDimension);

  /** Type used to store Sinusoid parameters. */
  using ArrayType = FixedArray<double, ImageDimension>;

  /** Size type matches that used for images */
  using SizeType = typename TOutputImage::SizeType;
  using SizeValueType = typename TOutputImage::SizeValueType;

  /** Types for parameters. */
  using ParametersValueType = typename Superclass::ParametersValueType;
  using ParametersType = typename Superclass::ParametersType;

  /** Run-time type information (and related methods). */
  itkOverrideGetNameOfClassMacro(SinusoidImageSource);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Set/Get the sinusoid phase shift in radians. */
  itkSetMacro(PhaseOffset, double);
  itkGetConstMacro(PhaseOffset, double);
  /** Set/Get the per-direction frequency in cycles / spatial unit. */
  itkSetMacro(Frequency, ArrayType);
  itkGetConstReferenceMacro(Frequency, ArrayType);

  /** Set/get the parameters for this source. When this source is
   * templated over an N-dimensional output image type, the first N
   * values in the parameter array are the Frequency parameters in each
   * dimension, and the last value is the phaseOffset. */
  void
  SetParameters(const ParametersType & parameters) override;
  ParametersType
  GetParameters() const override;

  /** Get the number of parameters for this image source. When this
   * source is templated over an N-dimensional output image type, the
   * number of parameters is 2*N+1. */
  unsigned int
  GetNumberOfParameters() const override;

protected:
  SinusoidImageSource();
  // ~SinusoidImageSource(); default implementation ok
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  GenerateData() override;

private:
  /** Parameters for the Sinusoid. */

  /** The spatial frequency in each direction. */
  ArrayType m_Frequency;

  /** The phase shift. */
  double m_PhaseOffset{ 0.0 };

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Really only want to use a floating point pixel because the domain of the
   * output will be [-1, 1] */
  itkConceptMacro(FloatPixelType, (Concept::IsFloatingPoint<typename OutputImageType::PixelType>));
#endif
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSinusoidImageSource.hxx"
#endif

#endif
