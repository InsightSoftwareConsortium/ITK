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
/** \class SampleClassifierFilter
 *
 *  \brief Sample classification class
 *
 *  This filter takes as input a Sample and produces as output a
 *  classification in the form of a MembershipSample object.
 *
 * \ingroup ITKStatistics
 */

template< typename TSample >
class ITK_TEMPLATE_EXPORT SampleClassifierFilter:
  public ProcessObject
{
public:
  /** Standard class typedef */
  typedef SampleClassifierFilter     Self;
  typedef ProcessObject              Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Standard macros */
  itkTypeMacro(SampleClassifierFilter, ProcessObject);
  itkNewMacro(Self);

  /** Type of the input Sample */
  typedef TSample SampleType;

  /** typedefs Output type */
  typedef MembershipSample< SampleType >         MembershipSampleType;
  typedef typename MembershipSampleType::Pointer MembershipSampleObjectPointer;

  /** typedefs from SampleType object */
  typedef typename SampleType::MeasurementType       MeasurementType;
  typedef typename SampleType::MeasurementVectorType MeasurementVectorType;

  /** typedef for the MembershipFunction */
  typedef MembershipFunctionBase< MeasurementVectorType > MembershipFunctionType;
  typedef typename MembershipFunctionType::ConstPointer   MembershipFunctionPointer;
  typedef std::vector< MembershipFunctionPointer >        MembershipFunctionVectorType;
  typedef SimpleDataObjectDecorator<
    MembershipFunctionVectorType >                        MembershipFunctionVectorObjectType;
  typedef typename
  MembershipFunctionVectorObjectType::Pointer MembershipFunctionVectorObjectPointer;

  /** typedef for membership functions weight proprtion */
  typedef Array< double > MembershipFunctionsWeightsArrayType;

  typedef SimpleDataObjectDecorator<
    MembershipFunctionsWeightsArrayType >                 MembershipFunctionsWeightsArrayObjectType;
  typedef typename
  MembershipFunctionsWeightsArrayObjectType::Pointer MembershipFunctionsWeightsArrayPointer;

  typedef IdentifierType                ClassLabelType;
  typedef std::vector< ClassLabelType > ClassLabelVectorType;
  typedef SimpleDataObjectDecorator<
    ClassLabelVectorType >                            ClassLabelVectorObjectType;
  typedef ClassLabelVectorObjectType::Pointer ClassLabelVectorObjectPointer;

  /** type of the decision rule */
  typedef DecisionRule                   DecisionRuleType;
  typedef DecisionRuleType::ConstPointer DecisionRulePointer;

  /** Sets the input sample that will be classified by this filter. */
  using Superclass::SetInput;
  void SetInput(const SampleType *sample);

  const SampleType *  GetInput() const;

  /** Returns the classification result */
  const MembershipSampleType * GetOutput() const;

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
  void SetClassLabels(const ClassLabelVectorObjectType *classLabels);

  /** Sets input vector of membership functions. The length of this vector must match
   * the number of classes, otherwise an exception will be thrown at run time.
   * */
  void SetMembershipFunctions(const MembershipFunctionVectorObjectType *membershipFunctions);

  /** Sets array of weights for the membership functions */
  void SetMembershipFunctionsWeightsArray(const MembershipFunctionsWeightsArrayObjectType *weightsArray);

protected:
  SampleClassifierFilter();
  virtual ~SampleClassifierFilter() ITK_OVERRIDE {}
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  ITK_DISALLOW_COPY_AND_ASSIGN(SampleClassifierFilter);

  /** Starts the classification process */
  virtual void GenerateData() ITK_OVERRIDE;

  /** Make a DataObject of the correct type to used as the specified
   * output. This method
   * is automatically called when DataObject::DisconnectPipeline() is
   * called.
   * \sa ProcessObject
   */
  typedef ProcessObject::DataObjectPointerArraySizeType DataObjectPointerArraySizeType;
  using Superclass::MakeOutput;
  virtual DataObjectPointer MakeOutput(DataObjectPointerArraySizeType idx) ITK_OVERRIDE;

private:

  unsigned int m_NumberOfClasses;

  /** Decision Rule */
  DecisionRulePointer m_DecisionRule;
};  // end of class
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSampleClassifierFilter.hxx"
#endif

#endif
