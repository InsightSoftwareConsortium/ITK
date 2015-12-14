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

#ifndef itkDOMWriter_hxx
#define itkDOMWriter_hxx

#include "itkDOMWriter.h"
#include "itksys/SystemTools.hxx"
#include "itkStdStreamLogOutput.h"
#include "itkFileTools.h"

namespace itk
{

template< typename TInput >
DOMWriter<TInput>::DOMWriter() : m_Input( ITK_NULLPTR )
{
  // Create the logger.
  this->m_Logger = LoggerType::New();
  // by default logged messages go to the console
  typename StdStreamLogOutput::Pointer defout = StdStreamLogOutput::New();
  defout->SetStream( std::cout );
  this->m_Logger->AddLogOutput( defout );
  // settings that may be important
  this->m_Logger->SetName( this->GetNameOfClass() );
  this->m_Logger->SetPriorityLevel( Logger::NOTSET ); // log everything
  this->m_Logger->SetLevelForFlushing( Logger::MUSTFLUSH ); // never flush (MUSTFLUSH actually leads to no flush, a bug in Logger)
  // some other settings
  this->m_Logger->SetTimeStampFormat( Logger::HUMANREADABLE );
  this->m_Logger->SetHumanReadableFormat( "%Y-%b-%d %H:%M:%S" ); // time stamp format
}

/** Set the input object to be written. */
template< typename TInput >
void
DOMWriter<TInput>::SetInput( const InputType* input )
{
  this->m_Input = input;
  this->m_InputHolder = dynamic_cast<const LightObject*>(input);
  this->Modified();
}

/** Get the input object to be written. */
template< typename TInput >
const typename DOMWriter<TInput>::InputType *
DOMWriter<TInput>::GetInput() const
{
  return this->m_Input;
}

/**
 * Function called by Update() or end-users to write the input object to a DOM object.
 * Some derived writers may accept an incomplete input object during the writing process, in those cases
 * the optional argument 'userdata' can be used to provide the missed information.
 */
template< typename TInput >
void
DOMWriter<TInput>::Update( DOMNodeType* outputdom, const void* userdata )
{
  if ( outputdom == ITK_NULLPTR )
    {
    itkExceptionMacro( "write to an invalid DOM object" );
    }

  if ( this->GetInput() == ITK_NULLPTR )
    {
    itkExceptionMacro( "input object is null" );
    }

  // remove previous data
  outputdom->RemoveAllAttributesAndChildren();

  // group subsequent logging under this writer
  this->GetLogger()->SetName( this->GetNameOfClass() );

  // variable/info needed for logging
  FancyString info;
  FancyString objname = this->GetInput()->GetNameOfClass();

  // log start of writing
  info << ClearContent << "Writing \"" << objname << "\" ...\n";
  this->GetLogger()->Info( info );

  // perform actual writing
  this->GenerateData( outputdom, userdata );

  // log end of writing
  info << ClearContent << "Writing \"" << objname << "\" done!\n";
  this->GetLogger()->Info( info );
}

/**
 * Function called by end-users to write the input object to the output XML file.
 */
template< typename TInput >
void
DOMWriter<TInput>::Update()
{
  if ( this->m_IntermediateDOM.IsNull() )
    {
    typename DOMNodeType::Pointer temp= DOMNodeType::New();
    this->SetIntermediateDOM( temp );
    }

  FancyString fn( this->m_FileName );

  // create the output file if necessary
  // this is to make sure that the output directory is created if it does exist
  FileTools::CreateFile( fn );

  // save the current working directory (WD), and change the WD to where the XML file is located
  FancyString sOldWorkingDir = itksys::SystemTools::GetCurrentWorkingDirectory();
  FancyString sNewWorkingDir = itksys::SystemTools::GetFilenamePath( fn );
  itksys::SystemTools::ChangeDirectory( sNewWorkingDir );

  this->Update( this->m_IntermediateDOM );

  // change the WD back to the previously saved
  itksys::SystemTools::ChangeDirectory( sOldWorkingDir );

  // write the newly updated DOM object to the output XML file
  typename DOMNodeXMLWriter::Pointer writer = DOMNodeXMLWriter::New();
  writer->SetFileName( fn.ToString() );
  writer->SetInput( this->m_IntermediateDOM );
  writer->Update();
}

} // namespace itk

#endif // itkDOMWriter_hxx
