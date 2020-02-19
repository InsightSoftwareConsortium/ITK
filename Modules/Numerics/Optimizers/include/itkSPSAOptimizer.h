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
#ifndef itkSPSAOptimizer_h
#define itkSPSAOptimizer_h

#include "itkSingleValuedNonLinearOptimizer.h"
#include "itkMersenneTwisterRandomVariateGenerator.h"
#include "ITKOptimizersExport.h"

namespace itk
{
/**\class SPSAOptimizerEnums
 * \brief Contains all enum classes used by SPSAOptimizer class.
 * \ingroup ITKOptimizers
 */
class SPSAOptimizerEnums
{
public:
  /** \class StopConditionSPSAOptimizer
   * \ingroup ITKOptimizers
   * Codes of stopping conditions */
  enum class StopConditionSPSAOptimizer : uint8_t
  {
    Unknown,
    MaximumNumberOfIterations,
    BelowTolerance,
    MetricError
  };
};
// Define how to print enumeration
extern ITKOptimizers_EXPORT std::ostream &
                            operator<<(std::ostream & out, const SPSAOptimizerEnums::StopConditionSPSAOptimizer value);
/**
 * \class SPSAOptimizer
 * \brief An optimizer based on simultaneous perturbation...
 *
 * This optimizer is an implementation of the Simultaneous
 * Perturbation Stochastic Approximation method, described in:
 *
 * - http://www.jhuapl.edu/SPSA/
 *
 * - Spall, J.C. (1998), "An Overview of the Simultaneous
 * Perturbation Method for Efficient Optimization," Johns
 * Hopkins APL Technical Digest, vol. 19, pp. 482-492
 *
 * \ingroup Optimizers
 * \ingroup ITKOptimizers
 */

class ITKOptimizers_EXPORT SPSAOptimizer : public SingleValuedNonLinearOptimizer
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(SPSAOptimizer);

  /** Standard class type aliases. */
  using Self = SPSAOptimizer;
  using Superclass = SingleValuedNonLinearOptimizer;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SPSAOptimizer, SingleValuedNonLinearOptimizer);

  using StopConditionSPSAOptimizerEnum = SPSAOptimizerEnums::StopConditionSPSAOptimizer;
#if !defined(ITK_LEGACY_REMOVE)
  // We need to expose the enum values at the class level
  // for backwards compatibility
  static constexpr StopConditionSPSAOptimizerEnum Unknown = StopConditionSPSAOptimizerEnum::Unknown;
  static constexpr StopConditionSPSAOptimizerEnum MaximumNumberOfIterations =
    StopConditionSPSAOptimizerEnum::MaximumNumberOfIterations;
  static constexpr StopConditionSPSAOptimizerEnum BelowTolerance = StopConditionSPSAOptimizerEnum::BelowTolerance;
  static constexpr StopConditionSPSAOptimizerEnum MetricError = StopConditionSPSAOptimizerEnum::MetricError;
#endif
  /** Advance one step following the gradient direction. */
  virtual void
  AdvanceOneStep();

  /** Start optimization. */
  void
  StartOptimization() override;

  /** Resume previously stopped optimization with current parameters
   * \sa StopOptimization. */
  void
  ResumeOptimization();

  /** Stop optimization.
   * \sa ResumeOptimization */
  void
  StopOptimization();

  /** Get the cost function value at the current position. */
  virtual MeasureType
  GetValue() const;

  /** Get the cost function value at any position */
  virtual MeasureType
  GetValue(const ParametersType & parameters) const;

  /** Guess the parameters a and A. This function needs the
   * number of GradientEstimates used for estimating a and A and
   * and the expected initial step size (where step size is
   * defined as the maximum of the absolute values of the
   * parameter update). Make sure you set c, Alpha, Gamma,
   * the MaximumNumberOfIterations, the Scales, and the
   * the InitialPosition before calling this method.
   *
   * Described in:
   * Spall, J.C. (1998), "Implementation of the Simultaneous Perturbation
   * Algorithm for Stochastic Optimization", IEEE Trans. Aerosp. Electron.
   * Syst. 34(3), 817-823.
   */
  virtual void
  GuessParameters(SizeValueType numberOfGradientEstimates, double initialStepSize);

  /** Get the current iteration number. */
  itkGetConstMacro(CurrentIteration, SizeValueType);

  /** Get Stop condition. */
  itkGetConstMacro(StopCondition, StopConditionSPSAOptimizerEnum);

  /** Get the current LearningRate (a_k) */
  itkGetConstMacro(LearningRate, double);

  /** Get the GradientMagnitude of the latest computed gradient */
  itkGetConstMacro(GradientMagnitude, double);

  /** Get the latest computed gradient */
  itkGetConstReferenceMacro(Gradient, DerivativeType);

  /** Set/Get a. */
  itkSetMacro(Sa, double);
  itkGetConstMacro(Sa, double);
  // For backward compatibility
  void
  Seta(double a)
  {
    SetSa(a);
  }
  double
  Geta() const
  {
    return GetSa();
  }

  /** Set/Get c. */
  itkSetMacro(Sc, double);
  itkGetConstMacro(Sc, double);
  // For backward compatibility
  void
  Setc(double c)
  {
    SetSc(c);
  }
  double
  Getc() const
  {
    return GetSc();
  }

  /** Set/Get A. */
  itkSetMacro(A, double);
  itkGetConstMacro(A, double);

  /** Set/Get alpha. */
  itkSetMacro(Alpha, double);
  itkGetConstMacro(Alpha, double);

  /** Set/Get gamma. */
  itkSetMacro(Gamma, double);
  itkGetConstMacro(Gamma, double);

  /** Methods to configure the cost function. */
  itkGetConstMacro(Maximize, bool);
  itkSetMacro(Maximize, bool);
  itkBooleanMacro(Maximize);
  bool
  GetMinimize() const
  {
    return !m_Maximize;
  }
  void
  SetMinimize(bool v)
  {
    this->SetMaximize(!v);
  }
  void
  MinimizeOn()
  {
    this->MaximizeOff();
  }
  void
  MinimizeOff()
  {
    this->MaximizeOn();
  }

  /** Set/Get the number of perturbation used to construct
   * a gradient estimate g_k.
   * q = NumberOfPerturbations
   * g_k = 1/q sum_{j=1..q} g^(j)_k
   */
  itkSetMacro(NumberOfPerturbations, SizeValueType);
  itkGetConstMacro(NumberOfPerturbations, SizeValueType);

  /**
   * Get the state of convergence in the last iteration. When the
   * StateOfConvergence is lower than the Tolerance, and the minimum
   * number of iterations has been performed, the optimization
   * stops.
   *
   * The state of convergence (SOC) is initialized with 0.0 and
   * updated after each iteration as follows:
   *   SOC *= SOCDecayRate
   *   SOC += a_k * GradientMagnitude
   */
  itkGetConstMacro(StateOfConvergence, double);

  /** Set/Get StateOfConvergenceDecayRate (number between 0 and 1). */
  itkSetMacro(StateOfConvergenceDecayRate, double);
  itkGetConstMacro(StateOfConvergenceDecayRate, double);

  /** Set/Get the minimum number of iterations */
  itkSetMacro(MinimumNumberOfIterations, SizeValueType);
  itkGetConstMacro(MinimumNumberOfIterations, SizeValueType);

  /** Set/Get the maximum number of iterations. */
  itkSetMacro(MaximumNumberOfIterations, SizeValueType);
  itkGetConstMacro(MaximumNumberOfIterations, SizeValueType);

  /** Set/Get Tolerance */
  itkSetMacro(Tolerance, double);
  itkGetConstMacro(Tolerance, double);

  /** Get the reason for termination */
  const std::string
  GetStopConditionDescription() const override;

protected:
  SPSAOptimizer();
  ~SPSAOptimizer() override = default;

  /** PrintSelf method. */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Variables updated during optimization */
  DerivativeType m_Gradient;

  double m_LearningRate;

  DerivativeType m_Delta;

  bool m_Stop{ false };

  StopConditionSPSAOptimizerEnum m_StopCondition;

  double m_StateOfConvergence;

  SizeValueType m_CurrentIteration;

  /** Random number generator */
  Statistics::MersenneTwisterRandomVariateGenerator::Pointer m_Generator;

  /** Method to compute the learning rate at iteration k (a_k). */
  virtual double
  Compute_a(SizeValueType k) const;

  /**
   * Method to compute the gain factor for the perturbation
   * at iteration k (c_k).
   */
  virtual double
  Compute_c(SizeValueType k) const;

  /** Method to generate a perturbation vector. Takes scales into account. */
  virtual void
  GenerateDelta(const unsigned int spaceDimension);

  /**
   * Compute the gradient at a position. m_NumberOfPerturbations are used,
   * and scales are taken into account.
   */
  virtual void
  ComputeGradient(const ParametersType & parameters, DerivativeType & gradient);

private:
  /** Settings.*/
  SizeValueType m_MinimumNumberOfIterations;
  SizeValueType m_MaximumNumberOfIterations;
  double        m_StateOfConvergenceDecayRate;
  double        m_Tolerance;
  bool          m_Maximize;
  double        m_GradientMagnitude;
  SizeValueType m_NumberOfPerturbations;

  /** Parameters, as described by Spall.*/
  double m_Sa;
  double m_Sc;
  double m_A;
  double m_Alpha;
  double m_Gamma;
}; // end class SPSAOptimizer

// Define how to print enumeration
extern ITKOptimizers_EXPORT std::ostream &
                            operator<<(std::ostream & out, const SPSAOptimizer::StopConditionSPSAOptimizerEnum value);

} // end namespace itk

#endif // end #ifndef itkSPSAOptimizer_h
