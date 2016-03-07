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

#include "itkParticleSwarmOptimizerSAXWriter.h"

#include "itksys/SystemTools.hxx"

namespace itk
{

/**
 * Check that whether the file with given name is writable.
 */
int ParticleSwarmOptimizerSAXWriter::CanWriteFile( const char* name )
{
  std::ofstream ofs( name );
  int yes = ofs.is_open();
  if (yes)
    {
    ofs.close();
    }
  return yes;
}

/**
 * Method for performing XML file generation from the input object.
 */
int ParticleSwarmOptimizerSAXWriter::WriteFile()
{
  try
    {
    if ( !this->CanWriteFile( this->m_Filename.c_str() ) )
      {
      ExceptionObject e( __FILE__, __LINE__ );
      std::string message = "Cannot write to ";
      message += this->m_Filename;
      message += "!\n";
      e.SetDescription( message.c_str() );
      throw e;
      }

    if ( this->m_InputObject == 0 )
      {
      itkExceptionMacro( "Object to be written is null!\n" );
      }

    std::ofstream ofs( this->m_Filename.c_str() );

    // start the tag 'optimizer'

    ofs << "<optimizer type=\"ParticleSwarmOptimizer\"";

    ofs << " NumberOfParticles=\"" << this->m_InputObject->GetNumberOfParticles() << "\"";

    ofs << " MaximumNumberOfIterations=\"" << this->m_InputObject->GetMaximalNumberOfIterations() << "\"";

    ofs << " InertiaCoefficient=\"" << this->m_InputObject->GetInertiaCoefficient() << "\"";

    ofs << " GlobalCoefficient=\"" << this->m_InputObject->GetGlobalCoefficient() << "\"";

    ofs << " PersonalCoefficient=\"" << this->m_InputObject->GetPersonalCoefficient() << "\"";

    ofs << " FunctionConvergenceTolerance=\"" << this->m_InputObject->GetFunctionConvergenceTolerance() << "\"";

    ofs << " ConvergedPercentageToStop=\"" << this->m_InputObject->GetPercentageParticlesConverged() << "\"";

    ofs << ">";
    ofs << "\n";

    //
    ParticleSwarmOptimizer::ParameterBoundsType bounds = this->m_InputObject->GetParameterBounds();

    // write the lower bound

    ofs << "  <bound id=\"lower\"";

    ofs << " value=\"";
    for ( size_t i = 0; i < bounds.size(); i++ )
      {
      ofs << " " << bounds[i].first;
      }
    ofs << "\"";

    ofs << "/>";
    ofs << "\n";

    // write the upper bound

    ofs << "  <bound id=\"upper\"";

    ofs << " value=\"";
    for ( size_t i = 0; i < bounds.size(); i++ )
      {
      ofs << " " << bounds[i].second;
      }
    ofs << "\"";

    ofs << "/>";
    ofs << "\n";

    // write the ParametersConvergenceTolerance

    ofs << "  <ParametersConvergenceTolerance>";

    Array<double> ptols = this->m_InputObject->GetParametersConvergenceTolerance();
    // Note: The data-cast to unsigned int is required
    //       because itk::Array only supports 'unsigned int' number of elements.
    for ( unsigned int i = 0; i < ptols.GetSize(); i++ )
      {
      ofs << " " << ptols[ i ];
      }

    ofs << "</ParametersConvergenceTolerance>";
    ofs << "\n";

    // close the tag 'optimizer'

    ofs << "</optimizer>";
    ofs << "\n";

    ofs.close();

    return 1;
    }
  catch (...)
    {
    return 0;
    }
}

} // namespace itk
