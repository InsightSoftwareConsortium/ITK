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

#ifndef itkDOMReader_hxx
#define itkDOMReader_hxx

#include "itkDOMReader.h"
#include "itksys/SystemTools.hxx"
#include "itkStdStreamLogOutput.h"

namespace itk
{

template< typename TOutput >
DOMReader<TOutput>::DOMReader() : m_Output( ITK_NULLPTR )
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

/**
 * The output object will be created automatically, but the user
 * can appoint a user object as the output by calling this function.
 */
template< typename TOutput >
void
DOMReader<TOutput>::SetOutput( OutputType* output )
{
  this->m_Output = output;
  this->m_OutputHolder = dynamic_cast<LightObject*>(output);
  this->Modified();
}

/** Get the output object for full access. */
template< typename TOutput >
typename DOMReader<TOutput>::OutputType *
DOMReader<TOutput>::GetOutput()
{
  return this->m_Output;
}

/** Get the output object for read-only access. */
template< typename TOutput >
const typename DOMReader<TOutput>::OutputType *
DOMReader<TOutput>::GetOutput() const
{
  return this->m_Output;
}

/**
 * Function called by Update() or end-users to generate the output object from a DOM object.
 * Some derived readers may accept an incomplete DOM object during the reading process, in those cases
 * the optional argument 'userdata' can be used to provide the missed information.
 */
template< typename TOutput >
void
DOMReader<TOutput>::Update( const DOMNodeType* inputdom, const void* userdata )
{
  if ( inputdom == ITK_NULLPTR )
    {
    itkExceptionMacro( "read from an invalid DOM object" );
    }

  // group subsequent logging under this reader
  this->GetLogger()->SetName( this->GetNameOfClass() );

  // variable/info needed for logging
  FancyString info;
  FancyString tagname = inputdom->GetName();

  // log start of reading
  info << ClearContent << "Reading \"" << tagname << "\" ...\n";
  this->GetLogger()->Info( info );

  // perform actual reading
  this->GenerateData( inputdom, userdata );

  // log end of reading
  info << ClearContent << "Reading \"" << tagname << "\" done!\n";
  this->GetLogger()->Info( info );

  if ( this->GetOutput() == ITK_NULLPTR )
    {
    itkExceptionMacro( "no valid output object was generated" );
    }
}

/**
 * Function called by end-users to generate the output object from the input XML file.
 */
template< typename TOutput >
void
DOMReader<TOutput>::Update()
{
  // create the intermediate DOM object if it is not set
  if ( this->m_IntermediateDOM.IsNull() )
    {
    DOMNodePointer node = DOMNodeType::New();
    this->SetIntermediateDOM( node.GetPointer() );
    }

  FancyString fn( this->m_FileName );

  // remove previous data from the DOM object
  this->m_IntermediateDOM->RemoveAllAttributesAndChildren();

  // read the input XML file and update the DOM object
  typename DOMNodeXMLReader::Pointer reader = DOMNodeXMLReader::New();
  reader->SetOutput( this->m_IntermediateDOM );
  reader->SetFileName( fn.ToString() );
  reader->Update();

  // save the current working directory (WD), and change the WD to where the XML file is located
  FancyString sOldWorkingDir = itksys::SystemTools::GetCurrentWorkingDirectory();
  FancyString sNewWorkingDir = itksys::SystemTools::GetFilenamePath( fn );
  itksys::SystemTools::ChangeDirectory( sNewWorkingDir );

  this->Update( this->m_IntermediateDOM );

  // change the WD back to the previously saved
  itksys::SystemTools::ChangeDirectory( sOldWorkingDir );
}

} // namespace itk

#endif // itkDOMReader_hxx
