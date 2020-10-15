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
#ifndef itkFourierSeriesPath_h
#define itkFourierSeriesPath_h

#include "itkParametricPath.h"
#include "itkVectorContainer.h"
#include "itkIndex.h"

namespace itk
{
/**
 *\class FourierSeriesPath
 * \brief  Represent a closed path through ND Space by its frequency components
 *
 * This class is intended to represent closed parametric paths through an image
 * which are defined by their Fourier coefficients (frequency components).  The
 * paths must be closed and defined over the interval [0,1], where the paths'
 * values at input 0 and input 1 are identical.  The user can control how many
 * harmonics (how high of frequency components) are represented by a given
 * instantiation of this class.  Classic applications of this class include
 * smoothing other closed paths (by finding and using only the first n harmonics
 * of their frequency components) and interpolating exact derivatives of other
 * closed paths by first converting the other paths to FourierSeriesPaths, which
 * have an exactly defined algebraic derivative.  (As many harmonics as are
 * necessary to adequately approximate the original path should be used when
 * approximating derivatives.)
 *
 * \sa OrthogonallyCorrectedParametricPath
 * \sa EllipseParametricPath
 * \sa PolyLineParametricPath
 * \sa ParametricPath
 * \sa ChainCodePath
 * \sa Path
 * \sa ContinuousIndex
 * \sa Index
 * \sa Offset
 * \sa Vector
 *
 * \ingroup PathObjects
 * \ingroup ITKPath
 */
template <unsigned int VDimension>
class ITK_TEMPLATE_EXPORT FourierSeriesPath : public ParametricPath<VDimension>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(FourierSeriesPath);

  /** Standard class type aliases. */
  using Self = FourierSeriesPath;
  using Superclass = ParametricPath<VDimension>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(FourierSeriesPath, ParametricPath);

  /** Input type */
  using InputType = typename Superclass::InputType;

  /** Output type */
  using OutputType = typename Superclass::OutputType;

  /** Basic data-structure types used */
  using ContinuousIndexType = ContinuousIndex<double, VDimension>;
  using IndexType = Index<VDimension>;
  using OffsetType = Offset<VDimension>;
  using VectorType = Vector<double, VDimension>;
  using CoefficientsType = VectorContainer<unsigned, VectorType>;
  using CoefficientsPointer = typename CoefficientsType::Pointer;

  /** Return the location of the parametric path at the specified location. */
  OutputType
  Evaluate(const InputType & input) const override;

  /** Evaluate the first derivative of the ND output with respect to the 1D
   * input.  This is an exact, algebraic function. */
  VectorType
  EvaluateDerivative(const InputType & input) const override;

  /** Add another harmonic's frequency coefficients. */
  void
  AddHarmonic(const VectorType & CosCoefficients, const VectorType & SinCoefficients);

  /** Clear all frequency coefficients (including the "DC" coefficients). */
  void
  Clear()
  {
    m_CosCoefficients->Initialize();
    m_SinCoefficients->Initialize();
    this->Modified();
  }

  /** New() method for dynamic construction */
  itkNewMacro(Self);

  /** Needed for Pipelining */
  void
  Initialize() override
  {
    this->Clear();
  }

protected:
  FourierSeriesPath();
  ~FourierSeriesPath() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  CoefficientsPointer m_CosCoefficients;
  CoefficientsPointer m_SinCoefficients;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkFourierSeriesPath.hxx"
#endif

#endif
