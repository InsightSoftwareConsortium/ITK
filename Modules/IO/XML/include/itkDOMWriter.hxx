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

#ifndef __itkDOMWriter_hxx
#define __itkDOMWriter_hxx

#include "itkDOMWriter.h"
#include "itksys/SystemTools.hxx"
#include "itkStdStreamLogOutput.h"
#include "itkFileTools.h"

namespace itk
{

template< class T >
DOMWriter<T>::DOMWriter()
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
 * Function called by Update() or end-users to write the input object to a DOM object.
 * Some derived writers may accept an incomplete input object during the writing process, in those cases
 * the optional argument 'userdata' can be used to provide the missed information.
 */
template< class T >
void
DOMWriter<T>::Update( DOMNodeType* outputdom, const void* userdata )
{
  if ( outputdom == NULL )
    {
    itkExceptionMacro( "write to an invalid DOM object" );
    }

  if ( this->GetInput() == NULL )
    {
    itkExceptionMacro( "input object is null" );
    }

  // remove previous data
  outputdom->RemoveAllAttributesAndChildren();

  this->GenerateData( outputdom, userdata );
}

/**
 * Function called by end-users to write the input object to the output XML file.
 */
template< class T >
void
DOMWriter<T>::Update()
{
  DOMNodeType* domobj = this->GetIntermediateDOM();
  if ( domobj == NULL )
    {
    DOMNodePointer node = DOMNodeType::New();
    domobj = (DOMNodeType*)node;
    this->SetIntermediateDOM( domobj );
    }

  FancyString fn( this->m_FileName );

  // create the output file if necessary
  // this is to make sure that the output directory is created if it does exist
  FileTools::CreateFile( fn );

  // save the current working directory (WD), and change the WD to where the XML file is located
  FancyString sOldWorkingDir = itksys::SystemTools::GetCurrentWorkingDirectory();
  FancyString sNewWorkingDir = itksys::SystemTools::GetFilenamePath( fn );
  itksys::SystemTools::ChangeDirectory( sNewWorkingDir );

  this->Update( domobj );

  // change the WD back to the previously saved
  itksys::SystemTools::ChangeDirectory( sOldWorkingDir );

  // write the newly updated DOM object to the output XML file
  typename DOMNodeXMLWriter::Pointer writer = DOMNodeXMLWriter::New();
  writer->SetFileName( fn );
  writer->SetInput( domobj );
  writer->Update();
}

} // namespace itk

#endif // __itkDOMWriter_hxx
