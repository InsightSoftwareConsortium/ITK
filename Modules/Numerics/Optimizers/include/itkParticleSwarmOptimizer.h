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
#ifndef itkParticleSwarmOptimizer_h
#define itkParticleSwarmOptimizer_h

#include "itkParticleSwarmOptimizerBase.h"
#include "ITKOptimizersExport.h"

namespace itk
{
/** \class ParticleSwarmOptimizer
 * \brief Implementation of a Particle Swarm Optimization (PSO) algorithm.
 *
 * The PSO algorithm was originally presented in:<br>
 * J. Kennedy, R. Eberhart, "Particle Swarm Optimization",
 * Proc. IEEE Int. Neural Networks, 1995.<br>
 *
 * The algorithm uses a stochastic optimization approach. Optimization
 * is performed by maintaining a swarm (flock) of
 * particles that traverse the parameter space, searching for the optimal
 * function value. Associated with each particle are its location and speed, in
 * parameter space. A particle's next location is determined by its current
 * location, its current speed, the location of the best function value it
 * previously encountered, and the location of the best function value the
 * particles in its neighborhood previously encountered. In this implementation
 * we use a global neighborhood with the following update equations:<br>
 * \f[v_i(t+1) = wv_i(t) + c_1u_1(p_i-x_i(t)) + c_2u_2(p_g-x_i(t))\f]
 * \f[x_i(t+1) = clampToBounds(x_i(t) + v_i(t+1))\f]
 *
 * where \f$u_i\f$ are \f$~U(0,1)\f$ and \f$w,c_1,c_2\f$ are user selected
 * weights.
 *
 * Swarm initialization is performed within the user supplied parameter bounds
 * using a uniform distribution or a normal distribution centered on
 * the initial parameter values supplied by the user. The search terminates when
 * the maximal number of iterations has been reached or when the change in the
 * best value in the past \f$g\f$ generations is below a threshold and the swarm
 * has collapsed (i.e. particles are close to each other in parameter space).
 *
 * NOTE: This implementation only performs minimization.
 *
 * \ingroup Numerics Optimizers
 * \ingroup ITKOptimizers
 */
class ITKOptimizers_EXPORT ParticleSwarmOptimizer :
  public ParticleSwarmOptimizerBase
{
public:
  /** Standard "Self" typedef. */
  typedef ParticleSwarmOptimizer              Self;
  typedef ParticleSwarmOptimizerBase          Superclass;
  typedef SmartPointer<Self>                  Pointer;
  typedef SmartPointer<const Self>            ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro( Self )

  /** Run-time type information (and related methods). */
  itkTypeMacro( ParticleSwarmOptimizer, ParticleSwarmOptimizerBase )

  /** The Particle swarm optimizer uses the following update formula:
   * v_i(t+1) = w*v_i(t) +
   *            c_1*uniform(0,1)*(p_i-x_i(t)) +
   *            c_2*uniform(0,1)*(p_g-x_i(t))
   * x_i(t+1) = clampToBounds(x_i(t) + v_i(t+1))
   * where
   * w - inertia constant
   * c_1 - personal coefficient
   * c_2 - global coefficient
   * p_i - parameters yielding the best function value obtained by this particle
   * p_g - parameters yielding the best function value obtained by all particles
   */
  itkSetMacro( InertiaCoefficient, double )
  itkGetMacro( InertiaCoefficient, double )
  itkSetMacro( PersonalCoefficient, double )
  itkGetMacro( PersonalCoefficient, double )
  itkSetMacro( GlobalCoefficient, double )
  itkGetMacro( GlobalCoefficient, double )

protected:
  ParticleSwarmOptimizer();
  virtual ~ParticleSwarmOptimizer() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream& os, Indent indent) const ITK_OVERRIDE;
  virtual void UpdateSwarm() ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(ParticleSwarmOptimizer);

  ParametersType::ValueType                    m_InertiaCoefficient;
  ParametersType::ValueType                    m_PersonalCoefficient;
  ParametersType::ValueType                    m_GlobalCoefficient;
};

} // end namespace itk

#endif
