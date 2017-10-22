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

#ifndef itkParticleSwarmOptimizerBase_h
#define itkParticleSwarmOptimizerBase_h

#include "itkSingleValuedNonLinearOptimizer.h"
#include "itkMersenneTwisterRandomVariateGenerator.h"
#include "ITKOptimizersExport.h"

namespace itk
{
/** \class ParticleSwarmOptimizerBase
 * \brief Abstract implementation of a Particle Swarm Optimization (PSO) algorithm.
 *
 * The PSO algorithm was originally presented in:<br>
 * J. Kennedy, R. Eberhart, "Particle Swarm Optimization",
 * Proc. IEEE Int. Neural Networks, 1995.<br>
 *
 * The algorithm is a stochastic global search optimization approach.
 * Optimization is performed by maintaining a swarm of particles that traverse
 * the parameter space, searching for the optimal function value. Associated
 * with each particle are its location and speed, in parameter space.
 *
 * Swarm initialization is performed within the user supplied parameter bounds
 * using either a uniform distribution or a normal distribution centered on
 * the initial parameter values supplied by the user. The search terminates when
 * the maximal number of iterations has been reached or when the change in the
 * best value in the past \f$g\f$ generations is below a threshold and the swarm
 * has collapsed (i.e. best personal particle locations are close to the
 * swarm's best location in parameter space).
 *
 * The actual optimization procedure, updating the swarm, is performed in the
 * subclasses, required to implement the UpdateSwarm() method.
 *
 * NOTE: This implementation only performs minimization.
 *
 * \ingroup Numerics Optimizers
 * \ingroup ITKOptimizers
 */
class ITKOptimizers_EXPORT ParticleSwarmOptimizerBase :
  public SingleValuedNonLinearOptimizer
{
public:
  /** Standard "Self" typedef. */
  typedef ParticleSwarmOptimizerBase          Self;
  typedef SingleValuedNonLinearOptimizer      Superclass;
  typedef SmartPointer<Self>                  Pointer;
  typedef SmartPointer<const Self>            ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro( ParticleSwarmOptimizerBase, SingleValuedNonLinearOptimizer )

  typedef std::vector< std::pair<ParametersType::ValueType,
                                 ParametersType::ValueType> > ParameterBoundsType;

  struct ParticleData
  {
    ParametersType m_CurrentParameters;
    ParametersType m_CurrentVelocity;
    CostFunctionType::MeasureType m_CurrentValue;
    ParametersType m_BestParameters;
    CostFunctionType::MeasureType m_BestValue;
  };

  typedef std::vector<ParticleData>         SwarmType;
  typedef unsigned int                      NumberOfIterationsType;
  typedef unsigned int                      NumberOfParticlesType;
  typedef unsigned int                      NumberOfGenerationsType;
  typedef CostFunctionType::MeasureType     MeasureType;
  typedef ParametersType::ValueType         ValueType;
  typedef Statistics::MersenneTwisterRandomVariateGenerator
                                            RandomVariateGeneratorType;

  /** Specify whether to initialize the particles using a normal distribution
    * centered on the user supplied initial value or a uniform distribution.
    * If the optimum is expected to be near the initial value it is likely
    * that initializing with a normal distribution will result in faster
    * convergence.*/
  itkSetMacro( InitializeNormalDistribution, bool )
  itkGetMacro( InitializeNormalDistribution, bool )
  itkBooleanMacro( InitializeNormalDistribution )

  /**
   * Specify the initial swarm. Useful for evaluating PSO variants. If the
   * initial swarm is set it will be used. To revert to random initialization
   * (uniform or normal particle distributions) set using an empty swarm.
   */
  void SetInitialSwarm( const SwarmType &initialSwarm );
  void ClearSwarm();

  /**
   * Indicate whether or not to output the swarm information when printing an
   * object. By default this option is turned off as it generates too much
   * information.
   */
  itkSetMacro( PrintSwarm, bool )
  itkGetMacro( PrintSwarm, bool )
  itkBooleanMacro( PrintSwarm )

  /** Start optimization. */
  virtual void StartOptimization( void ) ITK_OVERRIDE;


  /** Set/Get number of particles in the swarm - the maximal number of function
      evaluations is m_MaximalNumberOfIterations*m_NumberOfParticles */
  void SetNumberOfParticles( NumberOfParticlesType n );
  itkGetMacro( NumberOfParticles, NumberOfParticlesType )

  /** Set/Get maximal number of iterations - the maximal number of function
      evaluations is m_MaximalNumberOfIterations*m_NumberOfParticles */
  itkSetMacro( MaximalNumberOfIterations, NumberOfIterationsType )
  itkGetMacro( MaximalNumberOfIterations, NumberOfIterationsType )

  /** Set/Get the number of generations to continue with minimal improvement in
   *  the function value, |f_best(g_i) - f_best(g_k)|<threshold where
   *  k <= i+NumberOfGenerationsWithMinimalImprovement
   *  Minimal value is one.*/
  itkSetMacro( NumberOfGenerationsWithMinimalImprovement, NumberOfGenerationsType )
  itkGetMacro( NumberOfGenerationsWithMinimalImprovement, NumberOfGenerationsType )

  /**Set/Get the parameter bounds. Search for optimal value is inside these
     bounds. NOTE: It is assumed that the first entry is the minimal value,
     second is the maximal value. */
  virtual void SetParameterBounds( ParameterBoundsType & bounds );
  void SetParameterBounds( std::pair<ParametersType::ValueType,
                           ParametersType::ValueType> &bounds,
                           unsigned int n );

  ParameterBoundsType GetParameterBounds() const;

    /** The optimization algorithm will terminate when the function improvement
     *  in the last m_NumberOfGenerationsWithMinimalImprovement generations
     *  is less than m_FunctionConvergenceTolerance and the maximal distance
     *  between particles and the best particle in each dimension is less than
     *  m_ParametersConvergenceTolerance[i] for the specified percentage of the
     *  particles.
     *  That is, we haven't improved the best function value for a while and in
     *  the parameter space most (m%) of our particles attained their best value
     *  close to the swarm's best value.
     *  NOTE: The use of different tolerances for each dimension is desired when
     *         optimizing over non-commensurate parameters (e.g. rotation and
     *         translation). Alternatively, we could use ITK's parameter scaling
     *         approach. The current approach seems more intuitive.
     */
  itkSetMacro( FunctionConvergenceTolerance, MeasureType )
  itkGetMacro( FunctionConvergenceTolerance, MeasureType )
  /**Set parameters convergence tolerance using the same value for all, sz,
     parameters*/
  void SetParametersConvergenceTolerance( ValueType convergenceTolerance,
                                          unsigned int sz );
  itkSetMacro( ParametersConvergenceTolerance, ParametersType )
  itkGetMacro( ParametersConvergenceTolerance, ParametersType )
  itkGetMacro( PercentageParticlesConverged, double )
  itkSetMacro( PercentageParticlesConverged, double )

  /**Set the random number seed for the swarm. Use this method to
   * produce reaptible results, typically, for testing.
   */
  itkSetMacro( Seed, RandomVariateGeneratorType::IntegerType)
  itkGetMacro( Seed, RandomVariateGeneratorType::IntegerType)

  /** Use a specific seed to initialize the random number
    * generator. If On, use m_Seed to seed the random number
    * generator. Default is Off. */
  itkSetMacro( UseSeed, bool )
  itkGetMacro( UseSeed, bool )
  itkBooleanMacro( UseSeed)

  /** Get the function value for the current position.
   *  NOTE: This value is only valid during and after the execution of the
   *        StartOptimization() method.*/
  MeasureType GetValue() const;

  /** Get the reason for termination */
  virtual const std::string GetStopConditionDescription() const ITK_OVERRIDE;

  /** Print the swarm information to the given output stream. Each line
   * (particle data) is of the form:
   * current_parameters current_velocity current_value best_parameters best_value
   */
  void PrintSwarm( std::ostream& os, Indent indent ) const;

protected:
  ParticleSwarmOptimizerBase();
  virtual ~ParticleSwarmOptimizerBase() ITK_OVERRIDE;
  virtual void PrintSelf( std::ostream& os, Indent indent ) const ITK_OVERRIDE;
  void PrintParamtersType(  const ParametersType& x, std::ostream& os ) const;

  /**
   * Implement your update rule in this function.*/
  virtual void UpdateSwarm() = 0;

  ITK_DISALLOW_COPY_AND_ASSIGN(ParticleSwarmOptimizerBase);

  virtual void ValidateSettings();

  /**
   * Initialize the particle swarm, and seed the random number generator.
   */
  virtual void Initialize();

  void RandomInitialization();
  void FileInitialization();

  bool                                         m_PrintSwarm;
  std::ostringstream                           m_StopConditionDescription;
  bool                                         m_InitializeNormalDistribution;
  NumberOfParticlesType                        m_NumberOfParticles;
  NumberOfIterationsType                       m_MaximalNumberOfIterations;
  NumberOfGenerationsType                      m_NumberOfGenerationsWithMinimalImprovement;
  ParameterBoundsType                          m_ParameterBounds;
  ParametersType                               m_ParametersConvergenceTolerance;
  double                                       m_PercentageParticlesConverged;
  CostFunctionType::MeasureType                m_FunctionConvergenceTolerance;
  std::vector<ParticleData>                    m_Particles;
  CostFunctionType::MeasureType                m_FunctionBestValue;
  std::vector<MeasureType>                     m_FunctionBestValueMemory;
  ParametersType                               m_ParametersBestValue;
  NumberOfIterationsType                       m_IterationIndex;
  RandomVariateGeneratorType::IntegerType      m_Seed;
  bool                                         m_UseSeed;
};
} // end namespace itk

#endif
