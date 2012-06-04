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

#include "itkParticleSwarmOptimizerDOMWriter.h"

namespace itk
{

/**
 * This function is called automatically when update functions are performed.
 * It should fill the contents of the intermediate DOM object by pulling information from the input object.
 */
void
ParticleSwarmOptimizerDOMWriter::GenerateData( DOMNodeType* outputdom, const void* ) const
{
  LoggerType* logger = this->GetLogger();

  const InputType* input = this->GetInput();

  // In general users should not remove the constness of the input object when generating
  // the corresponding XML DOM; however, some "Get" methods of PSO used here do not have the
  // "const" modifier due to the use of itkGetMacro instead of itkGetConstMacro, so we
  // have to use a workaround here by temporarily removing the constness of the input object,
  // but users should never modify the input object during the writing process.
  ParticleSwarmOptimizer* ipobj = const_cast<ParticleSwarmOptimizer*>(input);

  outputdom->SetName( "optimizer" );
  outputdom->SetAttribute( "type", "ParticleSwarmOptimizer" );

  // use a StringObject instead of a std::string in order to pickup the right overloaded
  // "operator<<()" functions for writing PSO data to strings
  FancyString s;

  logger->Info( "writing NumberOfParticles ...\n" );
  s << ClearContent << ipobj->GetNumberOfParticles();
  outputdom->SetAttribute( "NumberOfParticles", s );

  logger->Info( "writing MaximumNumberOfIterations ...\n" );
  s << ClearContent << ipobj->GetMaximalNumberOfIterations();
  outputdom->SetAttribute( "MaximumNumberOfIterations", s );

  logger->Info( "writing InertiaCoefficient ...\n" );
  s << ClearContent << ipobj->GetInertiaCoefficient();
  outputdom->SetAttribute( "InertiaCoefficient", s );

  logger->Info( "writing GlobalCoefficient ...\n" );
  s << ClearContent << ipobj->GetGlobalCoefficient();
  outputdom->SetAttribute( "GlobalCoefficient", s );

  logger->Info( "writing PersonalCoefficient ...\n" );
  s << ClearContent << ipobj->GetPersonalCoefficient();
  outputdom->SetAttribute( "PersonalCoefficient", s );

  std::vector<double> lbound;
  std::vector<double> ubound;
  ParticleSwarmOptimizer::ParameterBoundsType bounds = ipobj->GetParameterBounds();
  for ( size_t i = 0; i < bounds.size(); i++ )
    {
    lbound.push_back( bounds[i].first );
    ubound.push_back( bounds[i].second );
    }
  // generate and insert the DOM node for the lower bound of the parameters
  logger->Info( "writing LowerBound ...\n" );
  DOMNode::Pointer nodelb = DOMNode::New();
  nodelb->SetName( "bound" );
  nodelb->SetAttribute( "id", "lower" );
  nodelb->SetAttribute( "value", s << ClearContent << lbound );
  outputdom->AddChildAtEnd( nodelb );
  // generate and insert the DOM node for the upper bound of the parameters
  logger->Info( "writing UpperBound ...\n" );
  DOMNode::Pointer nodeub = DOMNode::New();
  nodeub->SetName( "bound" );
  nodeub->SetAttribute( "id", "upper" );
  nodeub->SetAttribute( "value", s << ClearContent << ubound );
  outputdom->AddChildAtEnd( nodeub );

  logger->Info( "writing ParametersConvergenceTolerance ...\n" );
  DOMNode::Pointer nodeptols = DOMNode::New();
  nodeptols->SetName( "ParametersConvergenceTolerance" );
  outputdom->AddChildAtEnd( nodeptols );
  s << ClearContent << (Array<double>)ipobj->GetParametersConvergenceTolerance(); // the casting is necessary to select the right templated function
  nodeptols->AddTextChildAtEnd( s );

  logger->Info( "writing FunctionConvergenceTolerance ...\n" );
  s << ClearContent << ipobj->GetFunctionConvergenceTolerance();
  outputdom->SetAttribute( "FunctionConvergenceTolerance", s );

  logger->Info( "writing ConvergedPercentageToStop ...\n" );
  s << ClearContent << ipobj->GetPercentageParticlesConverged();
  outputdom->SetAttribute( "ConvergedPercentageToStop", s );
}

} // namespace itk
