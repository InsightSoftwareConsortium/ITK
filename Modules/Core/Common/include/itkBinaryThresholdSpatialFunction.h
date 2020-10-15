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
#ifndef itkBinaryThresholdSpatialFunction_h
#define itkBinaryThresholdSpatialFunction_h

#include "itkSpatialFunction.h"
#include "itkImageBase.h"

namespace itk
{
/** \class BinaryThresholdSpatialFunction
 * \brief A spatial functions that returns if the internal spatial function
 * is within user specified thresholds.
 *
 * BinaryThresholdSpatialFunction is a wrapper class for an internal
 * spatial function and returns true if it is within user specified
 * thresholds and false otherwise.
 *
 * This class is templated over the internal spatial function type.
 *
 * \sa SpatialFunction
 *
 *
 * \ingroup ITKCommon
 */
template <typename TFunction>
class ITK_TEMPLATE_EXPORT BinaryThresholdSpatialFunction
  : public SpatialFunction<bool, TFunction::ImageDimension, typename TFunction::InputType>
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(BinaryThresholdSpatialFunction);

  /** Standard class type aliases. */
  using Self = BinaryThresholdSpatialFunction;
  using Superclass = SpatialFunction<bool, TFunction::ImageDimension, typename TFunction::InputType>;

  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(BinaryThresholdSpatialFunction, SpatialFunction);

  /** New macro for creation of through the object factory. */
  itkNewMacro(Self);

  /** OutputType type alias support. */
  using OutputType = typename Superclass::OutputType;

  /** InputType type alias support. */
  using InputType = typename TFunction::InputType;

  /** Underlying function type. */
  using FunctionType = TFunction;

  /** Underlying function output type. */
  using FunctionOutputType = typename TFunction::OutputType;

  /** Set/Get the lower threshold. */
  itkSetMacro(LowerThreshold, FunctionOutputType);
  itkGetConstReferenceMacro(LowerThreshold, FunctionOutputType);

  /** Set/Get the upper threshold. */
  itkSetMacro(UpperThreshold, FunctionOutputType);
  itkGetConstReferenceMacro(UpperThreshold, FunctionOutputType);

  /** Set/Get the underlying function. */
  itkSetObjectMacro(Function, FunctionType);
  itkGetModifiableObjectMacro(Function, FunctionType);

  /** Evaluate the function at a given position. */
  OutputType
  Evaluate(const InputType & point) const override;

protected:
  BinaryThresholdSpatialFunction();
  ~BinaryThresholdSpatialFunction() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  FunctionOutputType m_LowerThreshold;
  FunctionOutputType m_UpperThreshold;

  typename FunctionType::Pointer m_Function;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBinaryThresholdSpatialFunction.hxx"
#endif

#endif
