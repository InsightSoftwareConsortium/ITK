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
#ifndef itkExpectationMaximizationMixtureModelEstimator_h
#define itkExpectationMaximizationMixtureModelEstimator_h

#include <ITKStatisticsExport.h>
#include "itkMixtureModelComponentBase.h"
#include "itkGaussianMembershipFunction.h"
#include "itkSimpleDataObjectDecorator.h"

namespace itk
{
namespace Statistics
{
/**\class ExpectationMaximizationMixtureModelEstimatorEnums
 * \brief Contains all enum classes used by ExpectationMaximizationMixtureModelEstimator class.
 * \ingroup ITKStatistics
 */
class ExpectationMaximizationMixtureModelEstimatorEnums
{
public:
  /**\class TERMINATION_CODE
   * \ingroup ITKStatistics
   * Termination status after running optimization */
  enum class TERMINATION_CODE : uint8_t
  {
    CONVERGED = 0,
    NOT_CONVERGED = 1
  };
};
// Define how to print enumeration
extern ITKStatistics_EXPORT std::ostream &
                            operator<<(std::ostream & out, const ExpectationMaximizationMixtureModelEstimatorEnums::TERMINATION_CODE value);

/**
 *\class ExpectationMaximizationMixtureModelEstimator
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
 * \sphinx
 * \sphinxexample{Numerics/Statistics/2DGaussianMixtureModelExpectMax,2D Gaussian Mixture Model Expectation Maximum}
 * \sphinxexample{Numerics/Statistics/DistributionOfPixelsUsingGMM,Distribution Of Pixels Using GMM EM}
 * \sphinxexample{Numerics/Statistics/DistributeSamplingUsingGMM,Distribute Sampling Using GMM EM}
 * \endsphinx
 */

template <typename TSample>
class ITK_TEMPLATE_EXPORT ExpectationMaximizationMixtureModelEstimator : public Object
{
public:
  /** Standard class type alias */
  using Self = ExpectationMaximizationMixtureModelEstimator;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Standard macros */
  itkTypeMacro(ExpectationMaximizationMixtureModelEstimator, Object);
  itkNewMacro(Self);

  /** TSample template argument related type alias */
  using SampleType = TSample;
  using MeasurementType = typename TSample::MeasurementType;
  using MeasurementVectorType = typename TSample::MeasurementVectorType;

  /** Typedef required to generate dataobject decorated output that can
   * be plugged into SampleClassifierFilter */
  using GaussianMembershipFunctionType = GaussianMembershipFunction<MeasurementVectorType>;

  using GaussianMembershipFunctionPointer = typename GaussianMembershipFunctionType::Pointer;

  using MembershipFunctionType = MembershipFunctionBase<MeasurementVectorType>;
  using MembershipFunctionPointer = typename MembershipFunctionType::ConstPointer;
  using MembershipFunctionVectorType = std::vector<MembershipFunctionPointer>;
  using MembershipFunctionVectorObjectType = SimpleDataObjectDecorator<MembershipFunctionVectorType>;
  using MembershipFunctionVectorObjectPointer = typename MembershipFunctionVectorObjectType::Pointer;

  /** Type of the mixture model component base class */
  using ComponentType = MixtureModelComponentBase<TSample>;

  /** Type of the component pointer storage */
  using ComponentVectorType = std::vector<ComponentType *>;

  /** Type of the membership function base class */
  using ComponentMembershipFunctionType = MembershipFunctionBase<MeasurementVectorType>;

  /** Type of the array of the proportion values */
  using ProportionVectorType = Array<double>;

  /** Sets the target data that will be classified by this */
  void
  SetSample(const TSample * sample);

  /** Returns the target data */
  const TSample *
  GetSample() const;

  /** Set/Gets the initial proportion values. The size of proportion
   * vector should be same as the number of component (or classes) */
  void
  SetInitialProportions(ProportionVectorType & propotion);

  const ProportionVectorType &
  GetInitialProportions() const;

  /** Gets the result proportion values */
  const ProportionVectorType &
  GetProportions() const;

  /** type alias for decorated array of proportion */
  using MembershipFunctionsWeightsArrayObjectType = SimpleDataObjectDecorator<ProportionVectorType>;
  using MembershipFunctionsWeightsArrayPointer = typename MembershipFunctionsWeightsArrayObjectType::Pointer;

  /** Get method for data decorated Membership functions weights array */
  const MembershipFunctionsWeightsArrayObjectType *
  GetMembershipFunctionsWeightsArray() const;

  /** Set/Gets the maximum number of iterations. When the optimization
   * process reaches the maximum number of iterations, even if the
   * class parameters aren't converged, the optimization process
   * stops. */
  void
  SetMaximumIteration(int numberOfIterations);

  int
  GetMaximumIteration() const;

  /** Gets the current iteration. */
  int
  GetCurrentIteration()
  {
    return m_CurrentIteration;
  }

  /** Adds a new component (or class). */
  int
  AddComponent(ComponentType * component);

  /** Gets the total number of classes currently plugged in. */
  unsigned int
  GetNumberOfComponents() const;

  /** Runs the optimization process. */
  void
  Update();

  using TERMINATION_CODE_ENUM = ExpectationMaximizationMixtureModelEstimatorEnums::TERMINATION_CODE;
#if !defined(ITK_LEGACY_REMOVE)
  /**Exposes enums values for backwards compatibility*/
  static constexpr TERMINATION_CODE_ENUM CONVERGED = TERMINATION_CODE_ENUM::CONVERGED;
  static constexpr TERMINATION_CODE_ENUM NOT_CONVERGED = TERMINATION_CODE_ENUM::NOT_CONVERGED;
#endif

  /** Gets the termination status */
  TERMINATION_CODE_ENUM
  GetTerminationCode() const;

  /** Gets the membership function specified by componentIndex
  argument. */
  ComponentMembershipFunctionType *
  GetComponentMembershipFunction(int componentIndex) const;

  /** Output Membership function vector containing the membership functions with
   * the final optimized parameters */
  const MembershipFunctionVectorObjectType *
  GetOutput() const;

protected:
  ExpectationMaximizationMixtureModelEstimator();
  ~ExpectationMaximizationMixtureModelEstimator() override = default;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  bool
  CalculateDensities();

  double
  CalculateExpectation() const;

  bool
  UpdateComponentParameters();

  bool
  UpdateProportions();

  /** Starts the estimation process */
  void
  GenerateData();

private:
  /** Target data sample pointer*/
  const TSample * m_Sample;

  int m_MaxIteration{ 100 };
  int m_CurrentIteration{ 0 };

  TERMINATION_CODE_ENUM m_TerminationCode{ TERMINATION_CODE_ENUM::NOT_CONVERGED };
  ComponentVectorType   m_ComponentVector;
  ProportionVectorType  m_InitialProportions;
  ProportionVectorType  m_Proportions;

  MembershipFunctionVectorObjectPointer  m_MembershipFunctionsObject;
  MembershipFunctionsWeightsArrayPointer m_MembershipFunctionsWeightArrayObject;
}; // end of class
} // end of namespace Statistics
} // end of namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkExpectationMaximizationMixtureModelEstimator.hxx"
#endif

#endif
