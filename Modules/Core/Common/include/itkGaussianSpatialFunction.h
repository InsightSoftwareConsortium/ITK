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
#ifndef itkGaussianSpatialFunction_h
#define itkGaussianSpatialFunction_h

#include "itkSpatialFunction.h"
#include "itkFixedArray.h"
#include "itkFloatTypes.h"

namespace itk
{
/** \class GaussianSpatialFunction
 * \brief N-dimensional Gaussian spatial function class
 *
 * GaussianSpatialFunction implements a standard Gaussian curve in N-d.
 * m_Normalized determines whether or not the Gaussian is normalized
 * (whether or not the sum over infinite space is 1.0)
 *
 * m_Scale scales the output of the Gaussian to span a range
 * larger than 0->1, and is often set to the maximum value
 * of the output data type (for instance, 255 for uchars)
 *
 * \ingroup SpatialFunctions
 * \ingroup ITKCommon
 */
template <typename TOutput = double,
          unsigned int VImageDimension = 3,
          typename TInput = Point<SpacePrecisionType, VImageDimension>>
class ITK_TEMPLATE_EXPORT GaussianSpatialFunction : public SpatialFunction<TOutput, VImageDimension, TInput>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(GaussianSpatialFunction);

  /** Standard class type aliases. */
  using Self = GaussianSpatialFunction;
  using Superclass = SpatialFunction<TOutput, VImageDimension, TInput>;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(GaussianSpatialFunction, SpatialFunction);

  /** Input type for the function. */
  using InputType = typename Superclass::InputType;

  /** Output type for the function. */
  using OutputType = typename Superclass::OutputType;

  /** Type used to store gaussian parameters. */
  using ArrayType = FixedArray<double, VImageDimension>;

  /** Evaluate the function at a given position. */
  OutputType
  Evaluate(const TInput & position) const override;

  /** Set/Get the scale factor to multiply the true value of the Gaussian. */
  itkSetMacro(Scale, double);
  itkGetConstMacro(Scale, double);

  /** Set/Get whether or not to normalize the Gaussian. Default is false. */
  itkSetMacro(Normalized, bool);
  itkGetConstMacro(Normalized, bool);
  itkBooleanMacro(Normalized);

  /** Set/Get the standard deviation in each direction. */
  itkSetMacro(Sigma, ArrayType);
  itkGetConstMacro(Sigma, ArrayType);

  /** Set/Get the mean in each direction. */
  itkSetMacro(Mean, ArrayType);
  itkGetConstMacro(Mean, ArrayType);

protected:
  GaussianSpatialFunction();
  ~GaussianSpatialFunction() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  ArrayType m_Sigma;

  ArrayType m_Mean;

  double m_Scale{ 1.0 };

  bool m_Normalized{ false };
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkGaussianSpatialFunction.hxx"
#endif

#endif
