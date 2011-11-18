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
#include "itkInitializationBiasedParticleSwarmOptimizer.h"

namespace itk
{

InitializationBiasedParticleSwarmOptimizer
::InitializationBiasedParticleSwarmOptimizer(void)
{
  //magic numbers from Wachowiak et al. "An approach to multimodal biomedical
  //image registration utilizing particle swarm optimization"
  this->m_InertiaCoefficient = 0.7298;
  this->m_PersonalCoefficient = 1.49609;
  this->m_GlobalCoefficient = 1.49609;
  this->m_InitializationCoefficient = 1.49609;
}


void
InitializationBiasedParticleSwarmOptimizer
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );
  os<<"Acceleration coefficients [inertia, personal, global, initialization]: ";
  os<<"["<<this->m_InertiaCoefficient<<", "<<this->m_PersonalCoefficient<<", ";
  os<<this->m_GlobalCoefficient<<", "<<this->m_InitializationCoefficient<<"]\n";
}


void
InitializationBiasedParticleSwarmOptimizer
::UpdateSwarm( void )
{
  unsigned int j, k, n;
  itk::Statistics::MersenneTwisterRandomVariateGenerator::Pointer
    randomGenerator = Statistics::MersenneTwisterRandomVariateGenerator::GetInstance();
  ParametersType initialPosition = GetInitialPosition();

  n = static_cast<unsigned int>( ( GetCostFunction() )->GetNumberOfParameters() );
          //linear decrease in the weight of the initial parameter values
  double initializationCoefficient = this->m_InitializationCoefficient*
    ( 1.0 - static_cast<double>(m_IterationIndex)/static_cast<double>(m_MaximalNumberOfIterations));

  for( j=0; j<m_NumberOfParticles; j++ )
    {
    ParticleData & p = m_Particles[j];
    ParametersType::ValueType phi1, phi2, phi3;
    phi1 = randomGenerator->GetVariateWithClosedRange() *
           this->m_PersonalCoefficient;
    phi2 = randomGenerator->GetVariateWithClosedRange() *
           this->m_GlobalCoefficient;
    phi3 = randomGenerator->GetVariateWithClosedRange() *
           initializationCoefficient;
    for( k=0; k<n; k++ )
      {       //update velocity
      p.m_CurrentVelocity[k] =
        m_InertiaCoefficient*p.m_CurrentVelocity[k] +
        phi1*(p.m_BestParameters[k]-p.m_CurrentParameters[k]) +
        phi2*(m_ParametersBestValue[k]-p.m_CurrentParameters[k]) +
        phi3*(initialPosition[k]-p.m_CurrentParameters[k]);
                       //update location and ensure that it stays
                       //inside the feasible region
      p.m_CurrentParameters[k] += p.m_CurrentVelocity[k];
      if( p.m_CurrentParameters[k] < m_ParameterBounds[k].first )
        {
        p.m_CurrentParameters[k] = m_ParameterBounds[k].first;
        }
      else if (p.m_CurrentParameters[k] > m_ParameterBounds[k].second )
        {
        p.m_CurrentParameters[k] = m_ParameterBounds[k].second;
        }
      }       //evaluate function at new position
    p.m_CurrentValue = m_CostFunction->GetValue( p.m_CurrentParameters );
    if( p.m_CurrentValue < p.m_BestValue )
      {
      p.m_BestValue = p.m_CurrentValue;
      p.m_BestParameters = p.m_CurrentParameters;
      }
    }
}

}
