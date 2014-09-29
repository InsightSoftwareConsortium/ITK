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
#ifndef __itkSinusoidSpatialFunction_h
#define __itkSinusoidSpatialFunction_h

#include "itkSpatialFunction.h"
#include "itkFixedArray.h"

namespace itk
{
/** \class SinusoidSpatialFunction
 * \brief N-dimensional sinusoid spatial function class
 *
 * \f[
 *   I(\mathbf{x}) = \cos(2\pi\sum_i \omega_i x_i + \phi)
 * \f]
 *
 * where \f$\omega_i\f$ is the frequency, in spatial units, in direction
 * \f$i\f$, and \f$\phi\f$ is a phase shift.
 *
 * \sa SinusoidImageSource
 *
 * \ingroup SpatialFunctions
 * \ingroup ITKCommon
 */
template <typename TOutput = double,
          unsigned int VImageDimension = 3,
          typename TInput = Point<SpacePrecisionType, VImageDimension>>
class SinusoidSpatialFunction : public SpatialFunction<TOutput, VImageDimension, TInput>
{
public:
  /** Standard class typedefs. */
  typedef SinusoidSpatialFunction                           Self;
  typedef SpatialFunction<TOutput, VImageDimension, TInput> Superclass;
  typedef SmartPointer<Self>                                Pointer;
  typedef SmartPointer<const Self>                          ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SinusoidSpatialFunction, SpatialFunction);

  /** Input type for the function. */
  typedef typename Superclass::InputType InputType;

  /** Output type for the function. */
  typedef typename Superclass::OutputType OutputType;

  /** Type used to store gaussian parameters. */
  typedef FixedArray<double, VImageDimension> ArrayType;

  /** Evaluate the function at a given position. */
  OutputType
  Evaluate(const TInput & position) const;

  /** Set/Get the sinusoid phase shift in radians. */
  itkSetMacro(phaseOffset, double);
  itkGetConstMacro(phaseOffset, double);
  /** Set/Get the per-direction frequency in cycles / spatial unit. */
  itkSetMacro(Frequency, ArrayType);
  itkGetConstReferenceMacro(Frequency, ArrayType);

protected:
  SinusoidSpatialFunction();
  virtual ~SinusoidSpatialFunction();
  void
  PrintSelf(std::ostream & os, Indent indent) const;

private:
  SinusoidSpatialFunction(const Self &); // purposely not implemented
  void
  operator=(const Self &); // purposely not implemented

  /** The spatial frequency in each direction. */
  ArrayType m_Frequency;

  /** The phase shift. */
  double m_phaseOffset;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSinusoidSpatialFunction.hxx"
#endif

#endif
