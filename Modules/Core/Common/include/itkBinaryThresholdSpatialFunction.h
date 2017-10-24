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
template< typename TFunction >
class ITK_TEMPLATE_EXPORT BinaryThresholdSpatialFunction:
  public SpatialFunction< bool,
                          TFunction::ImageDimension,
                          typename TFunction::InputType >
{
public:
  /** Standard class typedefs. */
  typedef BinaryThresholdSpatialFunction Self;
  typedef SpatialFunction< bool,
                           TFunction::ImageDimension,
                           typename TFunction::InputType > Superclass;

  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(BinaryThresholdSpatialFunction, SpatialFunction);

  /** New macro for creation of through the object factory. */
  itkNewMacro(Self);

  /** OutputType typedef support. */
  typedef typename Superclass::OutputType OutputType;

  /** InputType typedef support. */
  typedef typename TFunction::InputType InputType;

  /** Underlying function type. */
  typedef TFunction FunctionType;

  /** Underlying function output type. */
  typedef typename TFunction::OutputType FunctionOutputType;

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
  virtual OutputType Evaluate(const InputType & point) const ITK_OVERRIDE;

protected:

  BinaryThresholdSpatialFunction();
  ~BinaryThresholdSpatialFunction() ITK_OVERRIDE;
  void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  FunctionOutputType m_LowerThreshold;
  FunctionOutputType m_UpperThreshold;

  typename FunctionType::Pointer m_Function;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(BinaryThresholdSpatialFunction);
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBinaryThresholdSpatialFunction.hxx"
#endif

#endif
