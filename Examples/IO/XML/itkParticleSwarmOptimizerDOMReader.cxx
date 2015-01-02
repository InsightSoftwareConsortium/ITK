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

#include "itkParticleSwarmOptimizerDOMReader.h"

namespace itk
{

/**
 * This function is called automatically when update functions are performed.
 * It should fill the contents of the output object by pulling information from the intermediate DOM object.
 */
void
ParticleSwarmOptimizerDOMReader::GenerateData( const DOMNodeType* inputdom, const void* )
{
  LoggerType* logger = this->GetLogger();

  OutputType* output = this->GetOutput();
  if ( output == ITK_NULLPTR )
    {
    logger->Info( "creating the output PSO object ...\n" );
    OutputType::Pointer object = OutputType::New();
    output = (OutputType*)object;
    this->SetOutput( output );
    }
  else
    {
    logger->Info( "filling an existing output PSO object ...\n" );
    }

  // Two functions can be used to read a single value or a std::vector/itk::Array from a FancyString:
  //   1. s.ToData( s, data );
  //   2. s >> data;
  // The two functions are identical when reading a single value.
  // For std::vector/itk::Array, version 1 read all data elements in the string, while version 2
  // reads a fixed number of data elements that is specified by data.size() or data.GetSize().
  //
  // During data reading, a std::ios_base::failure exception will be thrown if error occurred due to
  // wrong data format, missing data elements (for std::vector/itk::Array), etc.
  try
    {
    FancyString s;

    // read an integer value from an attribute
    logger->Info( "reading NumberOfParticles ...\n" );
    s = inputdom->GetAttribute("NumberOfParticles");
    int nop = 0; s >> nop;
    output->SetNumberOfParticles( nop );

    // read an integer value from an attribute
    logger->Info( "reading MaximumNumberOfIterations ...\n" );
    s = inputdom->GetAttribute("MaximumNumberOfIterations");
    int noi = 0; s >> noi;
    output->SetMaximalNumberOfIterations( noi );

    // read a double value from an attribute
    logger->Info( "reading InertiaCoefficient ...\n" );
    s = inputdom->GetAttribute("InertiaCoefficient");
    double icoef = 0; s >> icoef;
    output->SetInertiaCoefficient( icoef );

    // read a double value from an attribute
    logger->Info( "reading GlobalCoefficient ...\n" );
    s = inputdom->GetAttribute("GlobalCoefficient");
    double gcoef = 0; s >> gcoef;
    output->SetGlobalCoefficient( gcoef );

    // read a double value from an attribute
    logger->Info( "reading PersonalCoefficient ...\n" );
    s = inputdom->GetAttribute("PersonalCoefficient");
    double pcoef = 0; s >> pcoef;
    output->SetPersonalCoefficient( pcoef );

    // read a std::vector of double from an attribute
    // lower bound
    logger->Info( "reading LowerBound ...\n" );
    const DOMNode* nodelb = inputdom->GetChildByID( "lower" );
    s = nodelb->GetAttribute("value");
    std::vector<double> lbound;
    s.ToData( lbound ); // read all data elements in the string
    // upper bound
    logger->Info( "reading UpperBound ...\n" );
    const DOMNode* nodeub = inputdom->GetChildByID( "upper" );
    s = nodeub->GetAttribute("value");
    std::vector<double> ubound;
    s.ToData( ubound ); // read all data elements in the string
    // combine the two
    ParticleSwarmOptimizer::ParameterBoundsType bounds;
    for ( size_t i=0; i<lbound.size(); i++ )
      {
      std::pair<double,double> value;
      value.first = lbound[i];
      value.second = ubound[i];
      bounds.push_back( value );
      }
    output->SetParameterBounds( bounds );

    // read an itk::Array of double from a text child node
    logger->Info( "reading ParametersConvergenceTolerance ...\n" );
    const DOMNode* nodeptols = inputdom->GetChild( "ParametersConvergenceTolerance" );
    s = nodeptols->GetTextChild()->GetText();
    itk::Array<double> ptols;
    s.ToData( ptols ); // read all data elements in the string
    output->SetParametersConvergenceTolerance( ptols );

    // read a double value from an attribute
    logger->Info( "reading FunctionConvergenceTolerance ...\n" );
    s = inputdom->GetAttribute("FunctionConvergenceTolerance");
    double ftol = 0; s >> ftol;
    output->SetFunctionConvergenceTolerance( ftol );

    // read a double value from an attribute
    logger->Info( "reading ConvergedPercentageToStop ...\n" );
    s = inputdom->GetAttribute("ConvergedPercentageToStop");
    double stoppercent = 0; s >> stoppercent;
    output->SetPercentageParticlesConverged( stoppercent );
    }
  catch ( const std::ios_base::failure& f )
    {
    std::string s = f.what();
    s.append( "\n" );
    logger->Critical( s );
    // translate the STD exception to ITK exception
    ExceptionObject e( __FILE__, __LINE__ );
    e.SetDescription( f.what() );
    throw e;
    }
}

} // namespace itk
