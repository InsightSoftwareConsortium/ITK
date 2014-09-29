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
#ifndef __itkSinusoidImageSource_h
#define __itkSinusoidImageSource_h

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
  /** Standard class typedefs. */
  typedef SinusoidImageSource                 Self;
  typedef ParametricImageSource<TOutputImage> Superclass;
  typedef SmartPointer<Self>                  Pointer;
  typedef SmartPointer<const Self>            ConstPointer;

  /** Typedef for the output image type. */
  typedef TOutputImage OutputImageType;

  /** Dimensionality of the output image */
  itkStaticConstMacro(ImageDimension, unsigned int, TOutputImage::ImageDimension);

  /** Type used to store Sinusoid parameters. */
  typedef FixedArray<double, ImageDimension> ArrayType;

  /** Size type matches that used for images */
  typedef typename TOutputImage::SizeType      SizeType;
  typedef typename TOutputImage::SizeValueType SizeValueType;

  /** Types for parameters. */
  typedef typename Superclass::ParametersValueType ParametersValueType;
  typedef typename Superclass::ParametersType      ParametersType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(SinusoidImageSource, ParametricImageSource);

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Set/Get the sinusoid phase shift in radians. */
  itkSetMacro(PhaseShift, double);
  itkGetConstMacro(PhaseShift, double);
  /** Set/Get the per-direction frequency in cycles / spatial unit. */
  itkSetMacro(Frequency, ArrayType);
  itkGetConstReferenceMacro(Frequency, ArrayType);

  /** Set/get the parameters for this source. When this source is
   * templated over an N-dimensional output image type, the first N
   * values in the parameter array are the Frequency parameters in each
   * dimension, and the last value is the PhaseShift. */
  virtual void
  SetParameters(const ParametersType & parameters);
  virtual ParametersType
  GetParameters() const;

  /** Get the number of parameters for this image source. When this
   * source is templated over an N-dimensional output image type, the
   * number of parameters is 2*N+1. */
  virtual unsigned int
  GetNumberOfParameters() const;

protected:
  SinusoidImageSource();
  // ~SinusoidImageSource(); default implementation ok
  void
  PrintSelf(std::ostream & os, Indent indent) const;

  virtual void
  GenerateData() ITK_OVERRIDE;

private:
  SinusoidImageSource(const SinusoidImageSource &); // purposely not implemented
  void
  operator=(const SinusoidImageSource &); // purposely not implemented

  /** Parameters for the Sinusoid. */

  /** The spatial frequency in each direction. */
  ArrayType m_Frequency;

  /** The phase shift. */
  double m_PhaseShift;

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
