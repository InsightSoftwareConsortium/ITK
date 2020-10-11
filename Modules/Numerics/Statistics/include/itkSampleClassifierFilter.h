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
#ifndef itkSampleClassifierFilter_h
#define itkSampleClassifierFilter_h

#include <vector>

#include "itkMembershipSample.h"
#include "itkMembershipFunctionBase.h"
#include "itkDecisionRule.h"
#include "itkProcessObject.h"
#include "itkSimpleDataObjectDecorator.h"

namespace itk
{
namespace Statistics
{
/**
 *\class SampleClassifierFilter
 *
 *  \brief Sample classification class
 *
 *  This filter takes as input a Sample and produces as output a
 *  classification in the form of a MembershipSample object.
 *
 * \ingroup ITKStatistics
 */

template <typename TSample>
class ITK_TEMPLATE_EXPORT SampleClassifierFilter : public ProcessObject
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(SampleClassifierFilter);

  /** Standard class type alias */
  using Self = SampleClassifierFilter;
  using Superclass = ProcessObject;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Standard macros */
  itkTypeMacro(SampleClassifierFilter, ProcessObject);
  itkNewMacro(Self);

  /** Type of the input Sample */
  using SampleType = TSample;

  /** type alias Output type */
  using MembershipSampleType = MembershipSample<SampleType>;
  using MembershipSampleObjectPointer = typename MembershipSampleType::Pointer;

  /** type alias from SampleType object */
  using MeasurementType = typename SampleType::MeasurementType;
  using MeasurementVectorType = typename SampleType::MeasurementVectorType;

  /** type alias for the MembershipFunction */
  using MembershipFunctionType = MembershipFunctionBase<MeasurementVectorType>;
  using MembershipFunctionPointer = typename MembershipFunctionType::ConstPointer;
  using MembershipFunctionVectorType = std::vector<MembershipFunctionPointer>;
  using MembershipFunctionVectorObjectType = SimpleDataObjectDecorator<MembershipFunctionVectorType>;
  using MembershipFunctionVectorObjectPointer = typename MembershipFunctionVectorObjectType::Pointer;

  /** type alias for membership functions weight proprtion */
  using MembershipFunctionsWeightsArrayType = Array<double>;

  using MembershipFunctionsWeightsArrayObjectType = SimpleDataObjectDecorator<MembershipFunctionsWeightsArrayType>;
  using MembershipFunctionsWeightsArrayPointer = typename MembershipFunctionsWeightsArrayObjectType::Pointer;

  using ClassLabelType = IdentifierType;
  using ClassLabelVectorType = std::vector<ClassLabelType>;
  using ClassLabelVectorObjectType = SimpleDataObjectDecorator<ClassLabelVectorType>;
  using ClassLabelVectorObjectPointer = ClassLabelVectorObjectType::Pointer;

  /** type of the decision rule */
  using DecisionRuleType = DecisionRule;
  using DecisionRulePointer = DecisionRuleType::ConstPointer;

  /** Sets the input sample that will be classified by this filter. */
  using Superclass::SetInput;
  void
  SetInput(const SampleType * sample);

  const SampleType *
  GetInput() const;

  /** Returns the classification result */
  const MembershipSampleType *
  GetOutput() const;

  /** Number of classes. This must match the number of labels and membership
   * functions provided by the user, otherwise an exception will be thrown at
   */
  itkSetMacro(NumberOfClasses, unsigned int);
  itkGetConstMacro(NumberOfClasses, unsigned int);

  /** Set/Get the decision rule. */
  itkSetConstObjectMacro(DecisionRule, DecisionRuleType);
  itkGetConstObjectMacro(DecisionRule, DecisionRuleType);

  /** Sets input vector of class labels. The length of this vector must match
   * the number of classes, otherwise an exception will be thrown at run time.
   * */
  void
  SetClassLabels(const ClassLabelVectorObjectType * classLabels);

  /** Sets input vector of membership functions. The length of this vector must match
   * the number of classes, otherwise an exception will be thrown at run time.
   * */
  void
  SetMembershipFunctions(const MembershipFunctionVectorObjectType * membershipFunctions);

  /** Sets array of weights for the membership functions */
  void
  SetMembershipFunctionsWeightsArray(const MembershipFunctionsWeightsArrayObjectType * weightsArray);

protected:
  SampleClassifierFilter();
  ~SampleClassifierFilter() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Starts the classification process */
  void
  GenerateData() override;

  /** Make a DataObject of the correct type to used as the specified
   * output. This method
   * is automatically called when DataObject::DisconnectPipeline() is
   * called.
   * \sa ProcessObject
   */
  using DataObjectPointerArraySizeType = ProcessObject::DataObjectPointerArraySizeType;
  using Superclass::MakeOutput;
  DataObjectPointer
  MakeOutput(DataObjectPointerArraySizeType idx) override;

private:
  unsigned int m_NumberOfClasses;

  /** Decision Rule */
  DecisionRulePointer m_DecisionRule;
}; // end of class
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkSampleClassifierFilter.hxx"
#endif

#endif
