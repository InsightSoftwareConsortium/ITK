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

#ifndef itkInitializationBiasedParticleSwarmOptimizer_h
#define itkInitializationBiasedParticleSwarmOptimizer_h

#include "itkParticleSwarmOptimizerBase.h"
#include "ITKOptimizersExport.h"

namespace itk
{
/** \class InitializationBiasedParticleSwarmOptimizer
 * \brief Implementation of a biased/regularized Particle Swarm Optimization
 *        (PSO) algorithm.
 *
 * This PSO algorithm was originally described in:
 * M. P. Wachowiak, R. Smolikova, Y. Zheng, J. M. Zurada, A. S. Elmaghraby,
 * "An approach to multimodal biomedical image registration utilizing particle
 * swarm optimization", IEEE Trans. Evol. Comput., vol. 8(3): 289-301, 2004.
 *
 * The algorithm uses a stochastic optimization approach. Optimization
 * is performed by maintaining a swarm (flock) of
 * particles that traverse the parameter space, searching for the optimal
 * function value. Associated with each particle are its location and speed, in
 * parameter space. A particle's next location is determined by its current
 * location, its current speed, the location of the best function value it
 * previously encountered, the location of the best function value the
 * particles in its neighborhood previously encountered and the initial position
 * the user specified.
 *
 * The assumption is that the user's initial parameter settings are close to the
 * minimum, which is often the case for registration. The initial parameter
 * values are incorporated into the PSO's update rules, biasing the search in
 * their direction. The swarms update equations are thus:
 *
 * \f$v_i(t+1) = wv_i(t) + c_1u_1(p_i-x_i(t)) + c_2u_2(p_g-x_i(t)) +
 *               c_3u_3(x_{init} - x_i(t))\f$
 * \f$x_i(t+1) = clampToBounds(x_i(t) + v_i(t+1))\f$
 *
 * where \f$u_i\f$ are \f$~U(0,1)\f$ and \f$w,c_1,c_2, c_3\f$ are user selected
 * weights, and c_3 is linearly decreased per iteration so that it is in
 * \f$c_3=initial, 0\f$.
 *
 * Swarm initialization is performed within the user supplied parameter bounds
 * using a uniform distribution or a normal distribution centered on
 * the initial parameter values supplied by the user, \f$x_{init}\f$. The search
 * terminates when the maximal number of iterations has been reached or when the
 * change in the best value in the past \f$g\f$ generations is below a threshold
 * and the swarm has collapsed (i.e. particles are close to each other in
 * parameter space).
 *
 * \note This implementation only performs minimization.
 *
 * \ingroup Numerics Optimizers
 * \ingroup ITKOptimizers
 */
class ITKOptimizers_EXPORT InitializationBiasedParticleSwarmOptimizer :
  public ParticleSwarmOptimizerBase
{
public:
  /** Standard "Self" typedef. */
  typedef InitializationBiasedParticleSwarmOptimizer  Self;
  typedef ParticleSwarmOptimizerBase                  Superclass;
  typedef SmartPointer<Self>                          Pointer;
  typedef SmartPointer<const Self>                    ConstPointer;

  typedef double                                      CoefficientType;

  /** Method for creation through the object factory. */
  itkNewMacro( Self )

  /** Run-time type information (and related methods). */
  itkTypeMacro( InitializationBiasedParticleSwarmOptimizer,
                ParticleSwarmOptimizerBase )

  /** The Particle swarm optimizer uses the following update formula:
   * \f[c_3 =  c_{3initial}(1.0 - IterationIndex/MaximalNumberOfIterations)\f]
   * \f[v_i(t+1) = w*v_i(t) +
   *            c_1*uniform(0,1)*(p_i-x_i(t)) +
   *            c_2*uniform(0,1)*(p_g-x_i(t)) +
   *            c_3*uniform(0,1)*(x_{init}-x_i(t))\f]
   * \f[x_i(t+1) = clampToBounds(x_i(t) + v_i(t+1))\f]
   * where
   * \f$w\f$ - inertia constant
   * \f$c_1\f$ - personal coefficient
   * \f$c_2\f$ - global coefficient
   * \f$c_3\f$ - initial location coefficient
   * \f$p_i\f$ - parameters yielding the best function value obtained by this particle
   * \f$p_g\f$ - parameters yielding the best function value obtained by all particles
   * \f$x_{init}\f$ - initial parameter values provided by user
   */
  itkSetMacro( InertiaCoefficient, CoefficientType )
  itkGetMacro( InertiaCoefficient, CoefficientType )
  itkSetMacro( PersonalCoefficient, CoefficientType )
  itkGetMacro( PersonalCoefficient, CoefficientType )
  itkSetMacro( GlobalCoefficient, CoefficientType )
  itkGetMacro( GlobalCoefficient, CoefficientType )
  itkSetMacro( InitializationCoefficient, CoefficientType )
  itkGetMacro( InitializationCoefficient, CoefficientType )

protected:
  InitializationBiasedParticleSwarmOptimizer();
  virtual ~InitializationBiasedParticleSwarmOptimizer() ITK_OVERRIDE {};
  virtual void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;
  virtual void UpdateSwarm() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(InitializationBiasedParticleSwarmOptimizer);

  ParametersType::ValueType                    m_InertiaCoefficient;
  ParametersType::ValueType                    m_PersonalCoefficient;
  ParametersType::ValueType                    m_GlobalCoefficient;
  ParametersType::ValueType                    m_InitializationCoefficient;
};

} // end namespace itk

#endif
