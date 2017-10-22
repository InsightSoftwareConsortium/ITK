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
#ifndef itkExpectationMaximizationMixtureModelEstimator_h
#define itkExpectationMaximizationMixtureModelEstimator_h

#include "itkMixtureModelComponentBase.h"
#include "itkGaussianMembershipFunction.h"
#include "itkSimpleDataObjectDecorator.h"

namespace itk
{
namespace Statistics
{
/** \class ExpectationMaximizationMixtureModelEstimator
 *  \brief This class generates the parameter estimates for a mixture
 *  model using expectation maximization strategy.
 *
 * The first template argument is the type of the target sample
 * data. This estimator expects one or more mixture model component
 * objects of the classes derived from the
 * MixtureModelComponentBase. The actual component (or module)
 * parameters are updated by each component. Users can think this
 * class as a strategy or a integration point for the EM
 * procedure. The initial proportion (SetInitialProportions), the
 * input sample (SetSample), the mixture model components
 * (AddComponent), and the maximum iteration (SetMaximumIteration) are
 * required. The EM procedure terminates when the current iteration
 * reaches the maximum iteration or the model parameters converge.
 *
 * <b>Recent API changes:</b>
 * The static const macro to get the length of a measurement vector,
 * \c MeasurementVectorSize  has been removed to allow the length of a measurement
 * vector to be specified at run time. It is now obtained at run time from the
 * sample set as input. Please use the function
 * GetMeasurementVectorSize() to get the length.
 *
 * \sa MixtureModelComponentBase, GaussianMixtureModelComponent
 * \ingroup ITKStatistics
 *
 * \wiki
 * \wikiexample{Statistics/ExpectationMaximizationMixtureModelEstimator_2D,2D Gaussian Mixture Model Expectation Maximization}
 * \endwiki
 */

template< typename TSample >
class ITK_TEMPLATE_EXPORT ExpectationMaximizationMixtureModelEstimator:public Object
{
public:
  /** Standard class typedef */
  typedef ExpectationMaximizationMixtureModelEstimator Self;
  typedef Object                                       Superclass;
  typedef SmartPointer< Self >                         Pointer;
  typedef SmartPointer< const Self >                   ConstPointer;

  /** Standard macros */
  itkTypeMacro(ExpectationMaximizationMixtureModelEstimator,
               Object);
  itkNewMacro(Self);

  /** TSample template argument related typedefs */
  typedef TSample                                 SampleType;
  typedef typename TSample::MeasurementType       MeasurementType;
  typedef typename TSample::MeasurementVectorType MeasurementVectorType;

  /** Typedef requried to generate dataobject decorated output that can
    * be plugged into SampleClassifierFilter */
  typedef GaussianMembershipFunction< MeasurementVectorType >
  GaussianMembershipFunctionType;

  typedef typename GaussianMembershipFunctionType::Pointer
  GaussianMembershipFunctionPointer;

  typedef MembershipFunctionBase< MeasurementVectorType > MembershipFunctionType;
  typedef typename MembershipFunctionType::ConstPointer   MembershipFunctionPointer;
  typedef std::vector< MembershipFunctionPointer >        MembershipFunctionVectorType;
  typedef SimpleDataObjectDecorator<
    MembershipFunctionVectorType >                        MembershipFunctionVectorObjectType;
  typedef typename
  MembershipFunctionVectorObjectType::Pointer MembershipFunctionVectorObjectPointer;

  /** Type of the mixture model component base class */
  typedef MixtureModelComponentBase< TSample > ComponentType;

  /** Type of the component pointer storage */
  typedef std::vector< ComponentType * > ComponentVectorType;

  /** Type of the membership function base class */
  typedef MembershipFunctionBase< MeasurementVectorType >
  ComponentMembershipFunctionType;

  /** Type of the array of the proportion values */
  typedef Array< double > ProportionVectorType;

  /** Sets the target data that will be classified by this */
  void SetSample(const TSample *sample);

  /** Returns the target data */
  const TSample * GetSample() const;

  /** Set/Gets the initial proportion values. The size of proportion
   * vector should be same as the number of component (or classes) */
  void SetInitialProportions(ProportionVectorType & propotion);

  const ProportionVectorType & GetInitialProportions() const;

  /** Gets the result proportion values */
  const ProportionVectorType & GetProportions() const;

  /** typedef for decorated array of proportion */
  typedef SimpleDataObjectDecorator<
    ProportionVectorType >                 MembershipFunctionsWeightsArrayObjectType;
  typedef typename
  MembershipFunctionsWeightsArrayObjectType::Pointer MembershipFunctionsWeightsArrayPointer;

  /** Get method for data decorated Membership functions weights array */
  const MembershipFunctionsWeightsArrayObjectType * GetMembershipFunctionsWeightsArray() const;

  /** Set/Gets the maximum number of iterations. When the optimization
   * process reaches the maximum number of interations, even if the
   * class parameters aren't converged, the optimization process
   * stops. */
  void SetMaximumIteration(int numberOfIterations);

  int GetMaximumIteration() const;

  /** Gets the current iteration. */
  int GetCurrentIteration()
  {
    return m_CurrentIteration;
  }

  /** Adds a new component (or class). */
  int AddComponent(ComponentType *component);

  /** Gets the total number of classes currently plugged in. */
  unsigned int GetNumberOfComponents() const;

  /** Runs the optimization process. */
  void Update();

  /** Termination status after running optimization */
  enum TERMINATION_CODE { CONVERGED = 0, NOT_CONVERGED = 1 };

  /** Gets the termination status */
  TERMINATION_CODE GetTerminationCode() const;

  /** Gets the membership function specified by componentIndex
  argument. */
  ComponentMembershipFunctionType * GetComponentMembershipFunction(int componentIndex) const;

  /** Output Membership function vector containing the membership functions with
    * the final optimized parameters */
  const MembershipFunctionVectorObjectType * GetOutput() const;

protected:
  ExpectationMaximizationMixtureModelEstimator();
  virtual ~ExpectationMaximizationMixtureModelEstimator() ITK_OVERRIDE {}
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  bool CalculateDensities();

  double CalculateExpectation() const;

  bool UpdateComponentParameters();

  bool UpdateProportions();

  /** Starts the estimation process */
  void GenerateData();

private:
  /** Target data sample pointer*/
  const TSample *m_Sample;

  int m_MaxIteration;
  int m_CurrentIteration;

  TERMINATION_CODE     m_TerminationCode;
  ComponentVectorType  m_ComponentVector;
  ProportionVectorType m_InitialProportions;
  ProportionVectorType m_Proportions;

  MembershipFunctionVectorObjectPointer  m_MembershipFunctionsObject;
  MembershipFunctionsWeightsArrayPointer m_MembershipFunctionsWeightArrayObject;
};  // end of class
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkExpectationMaximizationMixtureModelEstimator.hxx"
#endif

#endif
