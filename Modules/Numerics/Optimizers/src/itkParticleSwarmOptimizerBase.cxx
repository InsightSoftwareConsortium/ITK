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
#include <algorithm>
#include "itkParticleSwarmOptimizerBase.h"

namespace itk
{


ParticleSwarmOptimizerBase
::ParticleSwarmOptimizerBase(void):
  m_FunctionBestValue(0),
  m_IterationIndex(0)
{
  this->m_PrintSwarm = false;
  this->m_InitializeNormalDistribution = false;
  this->m_NumberOfParticles = 35;
  this->m_MaximalNumberOfIterations = 200;
  this->m_NumberOfGenerationsWithMinimalImprovement = 1;
  this->m_ParametersConvergenceTolerance.Fill( 1e-8 );
  this->m_PercentageParticlesConverged = 0.6;
  this->m_FunctionConvergenceTolerance = 1e-4;
  this->m_Seed = 0;
  this->m_UseSeed = false;
}

ParticleSwarmOptimizerBase
::~ParticleSwarmOptimizerBase()
{
}


void
ParticleSwarmOptimizerBase
::SetNumberOfParticles( unsigned int n )
{
  if( !this->m_Particles.empty() && n != this->m_Particles.size() )
    {
    itkExceptionMacro(<<"swarm already set with different size, clear swarm and then set")
    }
  if( this->m_NumberOfParticles != n )
    {
    this->m_NumberOfParticles = n;
    Modified();
    }
}


void
ParticleSwarmOptimizerBase
::SetInitialSwarm( const SwarmType &initialSwarm )
{
  //Always clear the m_Particles.
  this->m_Particles.clear();
  if( !initialSwarm.empty() )
    {
    const SwarmType::const_iterator initialSwarm_END = initialSwarm.end();
    const unsigned int n = initialSwarm[0].m_CurrentParameters.GetSize();
    //check that the dimensions of the swarm data are consistent
    for( SwarmType::const_iterator it = initialSwarm.begin();
      it != initialSwarm_END; ++it )
      {
      if( (*it).m_CurrentParameters.GetSize() != n ||
        (*it).m_CurrentVelocity.GetSize() != n ||
        (*it).m_BestParameters.GetSize() != n )
        {
        itkExceptionMacro(<<"inconsistent dimensions in swarm data")
        }
      }
    this->m_Particles.insert( m_Particles.begin(),
      initialSwarm.begin(), initialSwarm_END );
    this->m_NumberOfParticles = static_cast<NumberOfParticlesType>( m_Particles.size() );
    }
  Modified();
}


void
ParticleSwarmOptimizerBase
::ClearSwarm()
{
  if( !this->m_Particles.empty() )
    {
    this->m_Particles.clear();
    Modified();
    }
}


void
ParticleSwarmOptimizerBase
::SetParameterBounds( ParameterBoundsType & bounds )
{
  this->m_ParameterBounds.clear();
  this->m_ParameterBounds.insert( m_ParameterBounds.begin(),
                            bounds.begin(), bounds.end() );
  Modified();
}


void
ParticleSwarmOptimizerBase
::SetParameterBounds( std::pair<ParametersType::ValueType,
                      ParametersType::ValueType> &bounds,
                      unsigned int n )
{
  this->m_ParameterBounds.clear();
  this->m_ParameterBounds.insert( m_ParameterBounds.begin(), n, bounds );
  Modified();
}


ParticleSwarmOptimizerBase::ParameterBoundsType
ParticleSwarmOptimizerBase
::GetParameterBounds() const
{
  return this->m_ParameterBounds;
}


void
ParticleSwarmOptimizerBase
::SetParametersConvergenceTolerance( ParametersType::ValueType
                                     convergenceTolerance, unsigned int sz )
{
  this->m_ParametersConvergenceTolerance.SetSize( sz );
  this->m_ParametersConvergenceTolerance.Fill( convergenceTolerance );
}


ParticleSwarmOptimizerBase::CostFunctionType::MeasureType
ParticleSwarmOptimizerBase
::GetValue() const
{
  return this->m_FunctionBestValue;
}


const std::string
ParticleSwarmOptimizerBase
::GetStopConditionDescription() const
{
  return this->m_StopConditionDescription.str();
}


void
ParticleSwarmOptimizerBase
::PrintSelf( std::ostream& os, Indent indent ) const
{

  Superclass::PrintSelf( os, indent );
  os<<indent<<"Create swarm using [normal, uniform] distribution: ";
  os<<"["<<this->m_InitializeNormalDistribution<<", ";
  os<<!this->m_InitializeNormalDistribution<<"]\n";
  os<<indent<<"Number of particles in swarm: "<<this->m_NumberOfParticles<<"\n";
  os<<indent<<"Maximal number of iterations: "<<this->m_MaximalNumberOfIterations<<"\n";
  os<<indent<<"Number of generations with minimal improvement: ";
  os<<this->m_NumberOfGenerationsWithMinimalImprovement<<"\n";
  ParameterBoundsType::const_iterator it, end;
  end = this->m_ParameterBounds.end();
  os<<indent<<"Parameter bounds: [";
  for( it=this->m_ParameterBounds.begin(); it != end; ++it )
    os<<" ["<<(*it).first<<", "<<(*it).second<<"]";
  os<<" ]\n";
  os<<indent<<"Parameters' convergence tolerance: "<<this->m_ParametersConvergenceTolerance;
  os<<"\n";
  os<<indent<<"Function convergence tolerance: "<<this->m_FunctionConvergenceTolerance << std::endl;
  os<<indent<<"UseSeed: " << m_UseSeed << std::endl;
  os<<indent<<"Seed: " << m_Seed << std::endl;

  os<<"\n";
          //printing the swarm, usually should be avoided (too much information)
  if( this->m_PrintSwarm && !m_Particles.empty() )
    {
    os<<indent<<"swarm data [current_parameters current_velocity current_value ";
    os<<"best_parameters best_value]:\n";
    PrintSwarm( os, indent );
    }
}


void
ParticleSwarmOptimizerBase
::PrintSwarm( std::ostream& os, Indent indent ) const
{
  std::vector<ParticleData>::const_iterator it, end;
  end = this->m_Particles.end();
  os<<indent<<"[\n";
  for( it = this->m_Particles.begin(); it != end; ++it )
    {
    const ParticleData & p = *it;
    os<<indent;
    PrintParamtersType( p.m_CurrentParameters, os );
    os<<" ";
    PrintParamtersType( p.m_CurrentVelocity, os );
    os<<" "<<p.m_CurrentValue<<" ";
    PrintParamtersType( p.m_BestParameters, os );
    os<<" "<<p.m_BestValue<<"\n";
    }
  os<<indent<<"]\n";
}


void
ParticleSwarmOptimizerBase
::PrintParamtersType( const ParametersType & x, std::ostream & os ) const
{
  unsigned int sz = x.size();
  for ( unsigned int i=0; i<sz; i++ )
    {
    os << x[i] << " ";
    }
}


void
ParticleSwarmOptimizerBase
::StartOptimization( void )
{
  unsigned int j, k, n, index, prevIndex;
  bool converged = false;
  unsigned int bestValueMemorySize =
    this->m_NumberOfGenerationsWithMinimalImprovement+1;
  unsigned int percentileIndex =
    static_cast<unsigned int>( this->m_PercentageParticlesConverged*
                               (this->m_NumberOfParticles-1) + 0.5 );

  ValidateSettings();
          //initialize particle locations, velocities, etc.
  Initialize();

  InvokeEvent( StartEvent() );

          //run the simulation
  n = static_cast<unsigned int>( ( GetCostFunction() )->GetNumberOfParameters() );
  for( this->m_IterationIndex=1; m_IterationIndex<m_MaximalNumberOfIterations && !converged; m_IterationIndex++ )
    {

    UpdateSwarm();

              //update the best function value/parameters
    for( j=0; j<this->m_NumberOfParticles; j++ )
      {
      if( this->m_Particles[j].m_BestValue < m_FunctionBestValue )
        {
        this->m_FunctionBestValue = m_Particles[j].m_BestValue;
        this->m_ParametersBestValue = m_Particles[j].m_BestParameters;
        }
      }

    SetCurrentPosition( this->m_ParametersBestValue );

                //update the best value memory
    index = this->m_IterationIndex%bestValueMemorySize;
    this->m_FunctionBestValueMemory[index] = m_FunctionBestValue;
              //check for convergence. the m_FunctionBestValueMemory is a ring
              //buffer with m_NumberOfGenerationsWithMinimalImprovement+1
              //elements. the optimizer has converged if: (a) the difference
              //between the first and last elements currently in the ring buffer
              //is less than the user specificed threshold. and (b) the particles
             //are close enough to the best particle.
    if( this->m_IterationIndex>=m_NumberOfGenerationsWithMinimalImprovement )
      {
      if( index ==  this->m_NumberOfGenerationsWithMinimalImprovement )
        prevIndex = 0;
      else
        prevIndex = index+1;
                  //function value hasn't improved for a while, check the
                  //parameter space to see if the "best" swarm has collapsed
                  //around the swarm's best parameters, indicating convergence
      if( ( this->m_FunctionBestValueMemory[prevIndex] -
            this->m_FunctionBestValueMemory[index] ) <
            this->m_FunctionConvergenceTolerance )
        {
        converged = true;
        std::vector<ParametersType::ValueType> parameterDiffs( this->m_NumberOfParticles );
        for( k=0; k<n && converged; k++ )
          {
          for( j=0; j<this->m_NumberOfParticles; j++ )
            {
              parameterDiffs[j] =
                fabs( this->m_Particles[j].m_BestParameters[k] -
                      this->m_ParametersBestValue[k] );
            }
          std::nth_element( parameterDiffs.begin(),
                            parameterDiffs.begin()+percentileIndex,
                            parameterDiffs.end() );
          converged = converged &&
                      parameterDiffs[percentileIndex] < this->m_ParametersConvergenceTolerance[k];
          }
        }
      }
    InvokeEvent( IterationEvent() );
    }

    this->m_StopConditionDescription << GetNameOfClass() << ": ";
    if( converged )
      this->m_StopConditionDescription << "successfuly converged after "<< m_IterationIndex <<" iterations";
    else
      this->m_StopConditionDescription << "terminated after "<< m_IterationIndex <<" iterations";
    InvokeEvent( EndEvent() );
}


void
ParticleSwarmOptimizerBase
::ValidateSettings()
{
  unsigned int i,n;

               //we have to have a cost function
  if( GetCostFunction() == ITK_NULLPTR )
    {
    itkExceptionMacro(<<"ITK_NULLPTR cost function")
    }
        //if we got here it is safe to get the number of parameters the cost
        //function expects
  n =
    static_cast<unsigned int>( ( GetCostFunction() )->GetNumberOfParameters() );

        //check that the number of parameters match
  ParametersType initialPosition = GetInitialPosition();
  if( initialPosition.Size() != n )
    {
    itkExceptionMacro(<<"cost function and initial position dimensions mismatch")
    }
              //at least one particle
  if( this->m_NumberOfParticles == 0 )
    {
    itkExceptionMacro(<<"number of particles is zero")
    }
               //at least one iteration (the initialization phase)
  if( this->m_MaximalNumberOfIterations == 0 )
    {
    itkExceptionMacro(<<"number of iterations is zero")
    }
                    //we need at least one generation difference to
                    //compare to the previous one
  if( this->m_NumberOfGenerationsWithMinimalImprovement == 0 )
    {
    itkExceptionMacro(<<"number of generations with minimal improvement is zero")
    }

  if( this->m_ParameterBounds.size() != n )
    {
    itkExceptionMacro(<<"cost function and parameter bounds dimensions mismatch")
    }
  for( i=0; i<n; i++ )
    {
      if( initialPosition[i] < this->m_ParameterBounds[i].first ||
          initialPosition[i] > this->m_ParameterBounds[i].second )
        {
        itkExceptionMacro(<<"initial position is outside specified parameter bounds")
        }
    }
        //if the user set an initial swarm, check that the number of parameters
        //match and that they are inside the feasible region
  if( !this->m_Particles.empty() )
    {
    if(this->m_Particles[0].m_CurrentParameters.GetSize() != n )
      {
      itkExceptionMacro(<<"cost function and particle data dimensions mismatch")
      }
    std::vector<ParticleData>::iterator it, end = this->m_Particles.end();
    for( it=this->m_Particles.begin(); it != end; ++it )
      {
      ParticleData &p = (*it);
      for( i=0; i<n; i++ )
        {
          if( p.m_CurrentParameters[i] < m_ParameterBounds[i].first ||
              p.m_CurrentParameters[i] > m_ParameterBounds[i].second ||
              p.m_BestParameters[i] < m_ParameterBounds[i].first ||
              p.m_BestParameters[i] > m_ParameterBounds[i].second )
          {
          itkExceptionMacro(<<"initial position is outside specified parameter bounds")
          }
        }
      }
    }
             //parameters' convergence tolerance has to be positive
  for( i=0; i<n; i++ )
    {
    if ( this->m_ParametersConvergenceTolerance[i] < 0 )
      {
      itkExceptionMacro(<<"negative parameters convergence tolerance")
      }
    }
           //function convergence tolerance has to be positive
  if ( this->m_FunctionConvergenceTolerance < 0 )
    {
      itkExceptionMacro(<<"negative function convergence tolerance")
    }
}

void
ParticleSwarmOptimizerBase
::Initialize()
{
  itk::Statistics::MersenneTwisterRandomVariateGenerator::Pointer
    randomGenerator = Statistics::MersenneTwisterRandomVariateGenerator::GetInstance();
  if (m_UseSeed)
    {
    randomGenerator->SetSeed(m_Seed);
    }
  else
    {
    randomGenerator->SetSeed();
    }

  this->m_StopConditionDescription.str("");

  SetCurrentPosition( GetInitialPosition() );

  this->m_IterationIndex = 0;

  this->m_FunctionBestValueMemory.resize( m_NumberOfGenerationsWithMinimalImprovement
                                    + 1 );
              //user did not supply initial swarm
  if( this->m_Particles.empty() )
    {
    RandomInitialization();
    }
            //initialize best function value
  this->m_FunctionBestValue =
    itk::NumericTraits<CostFunctionType::MeasureType>::max();
  for(unsigned int i=0; i<this->m_Particles.size(); i++ )
    {
    if( this->m_FunctionBestValue > m_Particles[i].m_BestValue )
      {
      this->m_FunctionBestValue =  m_Particles[i].m_BestValue;
      this->m_ParametersBestValue = m_Particles[i].m_BestParameters;
      }
    }
  this->m_FunctionBestValueMemory[0] = m_FunctionBestValue;
}


void
ParticleSwarmOptimizerBase
::RandomInitialization()
{
  unsigned int i, j, n;
  n = GetInitialPosition().Size();
  ParameterBoundsType parameterBounds( this->m_ParameterBounds );
  ParametersType mean = GetInitialPosition();
  itk::Statistics::MersenneTwisterRandomVariateGenerator::Pointer
    randomGenerator = Statistics::MersenneTwisterRandomVariateGenerator::GetInstance();

               //create swarm
  this->m_Particles.resize( m_NumberOfParticles );
  for( i=0; i<this->m_NumberOfParticles; i++ )
    {
      this->m_Particles[i].m_BestParameters.SetSize( n );
      this->m_Particles[i].m_CurrentParameters.SetSize( n );
      this->m_Particles[i].m_CurrentVelocity.SetSize( n );
    }

    //user supplied initial position is always one of the particles
  this->m_Particles[0].m_CurrentParameters = mean;

           //create particles with a normal distribution around the initial
           //parameter values, inside the feasible region
  if( this->m_InitializeNormalDistribution )
    {
    ParametersType variance( mean.GetSize() );
           //variance is set so that 3sigma == bounds
    for( i=0; i<n; i++ )
      {
        variance[i] = ( parameterBounds[i].second - parameterBounds[i].first )/3.0;
        variance[i] *= variance[i];
      }

    for( i=1; i<this->m_NumberOfParticles; i++ )
      {
      for( j=0; j<n; j++ )
        {
        this->m_Particles[i].m_CurrentParameters[j] =
          randomGenerator->GetNormalVariate( mean[j], variance[j] );
             //ensure that the particle is inside the feasible region
        if( this->m_Particles[i].m_CurrentParameters[j] < parameterBounds[j].first ||
            this->m_Particles[i].m_CurrentParameters[j] > parameterBounds[j].second )
          {
            j--;
          }
        }
      }
    }
     //create particles with uniform distribution inside the feasible region
  else
    {
    for( i=1; i<this->m_NumberOfParticles; i++ )
      {
      for( j=0; j<n; j++ )
        {
        this->m_Particles[i].m_CurrentParameters[j] =
          randomGenerator->GetUniformVariate( parameterBounds[j].first,
                                              parameterBounds[j].second );
        }
      }
    }

         //initialize the particles' velocity, so that x_i+v_i is inside the
         //feasible region, and set the best parameters to the current ones
  for( i=0; i<this->m_NumberOfParticles; i++ )
    {
    for( j=0; j<n; j++ )
      {
      this->m_Particles[i].m_CurrentVelocity[j] =
          ( randomGenerator->GetUniformVariate( parameterBounds[j].first,
                                                parameterBounds[j].second ) -
           this->m_Particles[i].m_CurrentParameters[j] );
      this->m_Particles[i].m_BestParameters[j] =
        this->m_Particles[i].m_CurrentParameters[j];
      }
    }
            //initial function evaluations
  for( i=0; i<this->m_NumberOfParticles; i++ )
    {
    this->m_Particles[i].m_CurrentValue =
      this->m_CostFunction->GetValue( m_Particles[i].m_CurrentParameters );
    this->m_Particles[i].m_BestValue = m_Particles[i].m_CurrentValue;
    }
}

}
