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

#include "itkParticleSwarmOptimizerSAXReader.h"

#include "itksys/SystemTools.hxx"
#include <sstream>

namespace itk
{

/**
 * Check that whether the file with given name is readable.
 */
int ParticleSwarmOptimizerSAXReader::CanReadFile( const char* name )
{
  std::ifstream ifs( name );
  int yes = ifs.is_open();
  if (yes)
    {
    ifs.close();
    }
  return yes;
}

/**
 * Method called when a new xml tag start is encountered.
 */
void ParticleSwarmOptimizerSAXReader::StartElement( const char* name, const char** atts )
{
  if ( itksys::SystemTools::Strucmp( name, "optimizer" ) == 0 && this->ContextIs( "/" ) )
    {
    this->ProcessOptimizerAttributes( atts, this->m_OutputObject );
    }
  else if ( itksys::SystemTools::Strucmp( name, "bound" ) == 0 && this->ContextIs( "/optimizer" ) )
    {
    std::vector<double>* bound = 0;

    const char* id = this->GetAttribute( atts, "id" );
    if ( id == 0 )
      {
      itkExceptionMacro( "Bound ID is missing!\n" );
      }
    else if ( itksys::SystemTools::Strucmp( id, "lower" ) == 0 )
      {
      bound = &this->m_LowerBound;
      }
    else if ( itksys::SystemTools::Strucmp( id, "upper" ) == 0 )
      {
      bound = &this->m_UpperBound;
      }
    else
      {
      itkExceptionMacro( "Bad attribute for bound:" << id);
      }
    this->ProcessBoundAttributes( atts, *bound );
    }
  else if ( itksys::SystemTools::Strucmp( name, "ParametersConvergenceTolerance" ) == 0 && this->ContextIs( "/optimizer" ) )
    {
    // nothing to do as it does not have attributes
    }

  this->m_CurrentTags.push_back( name );
}

/**
 * Method called when an xml tag end is encountered.
 */
void ParticleSwarmOptimizerSAXReader::EndElement( const char* name )
{
  if ( itksys::SystemTools::Strucmp( name, "optimizer" ) == 0 && this->ContextIs( "/optimizer" ) )
    {
    // all children have been read, now incorporate them into the output object
    ParticleSwarmOptimizer::ParameterBoundsType bounds;
    for ( size_t i = 0; i < this->m_LowerBound.size(); i++ )
      {
      std::pair<double,double> value;
      value.first = this->m_LowerBound[i];
      value.second = this->m_UpperBound[i];
      bounds.push_back( value );
      }
    this->m_OutputObject->SetParameterBounds( bounds );

    this->m_OutputObject->SetParametersConvergenceTolerance( this->m_ParametersConvergenceTolerance );
    }
  else if ( itksys::SystemTools::Strucmp( name, "bound" ) == 0 && this->ContextIs( "/optimizer/bound" ) )
    {
    // nothing to do as it does not have children
    }
  else if ( itksys::SystemTools::Strucmp( name, "ParametersConvergenceTolerance" ) == 0 && this->ContextIs( "/optimizer/ParametersConvergenceTolerance" ) )
    {
    // nothing to do as it does not have children
    }

  this->m_CurrentTags.pop_back();
}

/**
 * Method for handling the data inside an xml tag.
 */
void ParticleSwarmOptimizerSAXReader::CharacterDataHandler( const char* inData, int inLength )
{
  if ( this->ContextIs( "/optimizer/ParametersConvergenceTolerance" ) )
    {
    std::vector<double> data;

    std::string s( inData, inLength );
    std::istringstream iss( s );
    while ( iss.good() )
      {
      double value = 0;
      iss >> value;
      data.push_back( value );
      }

    Array<double> ptols( static_cast<Array<double>::SizeValueType>( data.size() ) );
    for ( unsigned int i = 0; i < data.size(); i++ )
      {
      ptols[i] = data[i];
      }
    this->m_ParametersConvergenceTolerance = ptols;
    }
}

/**
* Method for performing XML reading and output generation.
*/
int ParticleSwarmOptimizerSAXReader::ReadFile()
{
  try
    {
    if ( !this->CanReadFile( this->m_Filename.c_str() ) )
      {
      // itkExceptionMacro does not accept a variable as the input,
      // so the following is needed if we want to include the file name in the exception message
      ExceptionObject e( __FILE__, __LINE__ );
      std::string message = "Cannot read from ";
      message += this->m_Filename;
      message += "!\n";
      e.SetDescription( message.c_str() );
      throw e;
      }

    if ( this->m_OutputObject == 0 )
      {
      itkExceptionMacro( "Object to be read is null!\n" );
      }

    this->m_CurrentTags.clear();

    // the real work is performed in this method
    this->GenerateOutputInformation();

    return 1;
    }
  catch (...)
    {
    return 0;
    }
}

/** Process tag 'optimizer' attributes. */
void ParticleSwarmOptimizerSAXReader::ProcessOptimizerAttributes( const char** atts, ParticleSwarmOptimizer* opt )
{
  // go over all the attribute-value pairs
  for ( size_t i = 0; atts[i] != 0; i += 2 )
    {
    if ( itksys::SystemTools::Strucmp( atts[i], "NumberOfParticles" ) == 0 )
      {
      std::istringstream iss( atts[i+1] );
      int nop = 0;
      iss >> nop;
      opt->SetNumberOfParticles( nop );
      }
    else if ( itksys::SystemTools::Strucmp( atts[i], "MaximumNumberOfIterations" ) == 0 )
      {
      std::istringstream iss( atts[i+1] );
      int noi = 0;
      iss >> noi;
      opt->SetMaximalNumberOfIterations( noi );
      }
    else if ( itksys::SystemTools::Strucmp( atts[i], "InertiaCoefficient" ) == 0 )
      {
      std::istringstream iss( atts[i+1] );
      double icoef = 0;
      iss >> icoef;
      opt->SetInertiaCoefficient( icoef );
      }
    else if ( itksys::SystemTools::Strucmp( atts[i], "GlobalCoefficient" ) == 0 )
      {
      std::istringstream iss( atts[i+1] );
      double gcoef = 0;
      iss >> gcoef;
      opt->SetGlobalCoefficient( gcoef );
      }
    else if ( itksys::SystemTools::Strucmp( atts[i], "PersonalCoefficient" ) == 0 )
      {
      std::istringstream iss( atts[i+1] );
      double pcoef = 0;
      iss >> pcoef;
      opt->SetPersonalCoefficient( pcoef );
      }
    else if ( itksys::SystemTools::Strucmp( atts[i], "FunctionConvergenceTolerance" ) == 0 )
      {
      std::istringstream iss( atts[i+1] );
      double ftol = 0;
      iss >> ftol;
      opt->SetFunctionConvergenceTolerance( ftol );
      }
    else if ( itksys::SystemTools::Strucmp( atts[i], "ConvergedPercentageToStop" ) == 0 )
      {
      std::istringstream iss( atts[i+1] );
      double stoppercent = 0;
      iss >> stoppercent;
      opt->SetPercentageParticlesConverged( stoppercent );
      }
    }
}

/** Process tag 'bound' attributes. */
void ParticleSwarmOptimizerSAXReader::ProcessBoundAttributes( const char** atts, std::vector<double>& bound )
{
  // go over all the attribute-value pairs
  for ( size_t i = 0; atts[i] != 0; i += 2 )
    {
    if ( itksys::SystemTools::Strucmp( atts[i], "value" ) == 0 )
      {
      bound.clear();
      //
      std::istringstream iss( atts[i+1] );
      while ( iss.good() )
        {
        double value = 0;
        iss >> value;
        if ( !iss.fail() && !iss.bad() )
          {
          bound.push_back( value );
          }
        }
      } // end if
    } // end for
}

/** Search for and return a particular attribute from the attribute list. */
const char* ParticleSwarmOptimizerSAXReader::GetAttribute( const char** atts, const char* key )
{
  // go over all the attribute-value pairs
  for ( size_t i = 0; atts[i] != 0; i += 2 )
    {
    if ( itksys::SystemTools::Strucmp( atts[i], key ) == 0 )
      {
      return atts[i+1];
      }
    }
  return 0;
}

/** Check the current tags to see whether it matches a user input. */
bool ParticleSwarmOptimizerSAXReader::ContextIs( const char* test ) const
{
  std::string s = "";
  for ( size_t i = 0; i < this->m_CurrentTags.size(); i++ )
    {
    s += "/" + std::string(this->m_CurrentTags[i]);
    }
  if ( s.size() == 0 )
    {
    s = "/";
    }
  return ( s == std::string(test) );
}

} // namespace itk
