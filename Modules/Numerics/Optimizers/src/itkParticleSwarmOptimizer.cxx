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
#include "itkParticleSwarmOptimizer.h"

namespace itk
{


ParticleSwarmOptimizer::ParticleSwarmOptimizer()
{
  // magic numbers based on the analysis described in M. Clerc, J. Kennedy,
  //"The particle swarm - explosion, stability, and convergence in a
  // multidimensional complex space"
  this->m_InertiaCoefficient = 0.7298;
  this->m_PersonalCoefficient = 1.49609;
  this->m_GlobalCoefficient = 1.49609;
}


ParticleSwarmOptimizer::~ParticleSwarmOptimizer() = default;

void
ParticleSwarmOptimizer::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << "Acceleration coefficients [inertia, personal, global]: ";
  os << "[" << this->m_InertiaCoefficient << ", " << this->m_PersonalCoefficient << ", ";
  os << this->m_GlobalCoefficient << "]\n";
}

void
ParticleSwarmOptimizer::UpdateSwarm()
{
  unsigned int                                                    j, k, n;
  itk::Statistics::MersenneTwisterRandomVariateGenerator::Pointer randomGenerator =
    Statistics::MersenneTwisterRandomVariateGenerator::GetInstance();

  n = static_cast<unsigned int>((GetCostFunction())->GetNumberOfParameters());

  for (j = 0; j < m_NumberOfParticles; j++)
  {
    ParticleData &            p = m_Particles[j];
    ParametersType::ValueType phi1, phi2;
    phi1 = randomGenerator->GetVariateWithClosedRange() * this->m_PersonalCoefficient;
    phi2 = randomGenerator->GetVariateWithClosedRange() * this->m_GlobalCoefficient;
    for (k = 0; k < n; k++)
    { // update velocity
      p.m_CurrentVelocity[k] = m_InertiaCoefficient * p.m_CurrentVelocity[k] +
                               phi1 * (p.m_BestParameters[k] - p.m_CurrentParameters[k]) +
                               phi2 * (m_ParametersBestValue[k] - p.m_CurrentParameters[k]);
      // update location and ensure that it stays
      // inside the feasible region
      p.m_CurrentParameters[k] += p.m_CurrentVelocity[k];
      if (p.m_CurrentParameters[k] < m_ParameterBounds[k].first)
      {
        p.m_CurrentParameters[k] = m_ParameterBounds[k].first;
      }
      else if (p.m_CurrentParameters[k] > m_ParameterBounds[k].second)
      {
        p.m_CurrentParameters[k] = m_ParameterBounds[k].second;
      }
    } // evaluate function at new position
    p.m_CurrentValue = m_CostFunction->GetValue(p.m_CurrentParameters);
    if (p.m_CurrentValue < p.m_BestValue)
    {
      p.m_BestValue = p.m_CurrentValue;
      p.m_BestParameters = p.m_CurrentParameters;
    }
  }
}

} // namespace itk
