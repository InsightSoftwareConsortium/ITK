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

#ifndef __itkDOMReader_hxx
#define __itkDOMReader_hxx

#include "itkDOMReader.h"
#include "itksys/SystemTools.hxx"
#include "itkStdStreamLogOutput.h"

namespace itk
{

template< class T >
DOMReader<T>::DOMReader()
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
 * Function called by Update() or end-users to generate the output object from a DOM object.
 * Some derived readers may accept an incomplete DOM object during the reading process, in those cases
 * the optional argument 'userdata' can be used to provide the missed information.
 */
template< class T >
void
DOMReader<T>::Update( const DOMNodeType* inputdom, const void* userdata )
{
  if ( inputdom == NULL )
    {
    itkExceptionMacro( "read from an invalid DOM object" );
    }

  this->GenerateData( inputdom, userdata );

  if ( this->GetOutput() == NULL )
    {
    itkExceptionMacro( "not valid output object was generated" );
    }
}

/**
 * Function called by end-users to generate the output object from the input XML file.
 */
template< class T >
void
DOMReader<T>::Update()
{
  // create the intermediate DOM object if it is not set
  DOMNodeType* domobj = this->GetIntermediateDOM();
  if ( domobj == NULL )
    {
    DOMNodePointer node = DOMNodeType::New();
    domobj = (DOMNodeType*)node;
    this->SetIntermediateDOM( domobj );
    }

  FancyString fn( this->m_FileName );

  // remove previous data from the DOM object
  domobj->RemoveAllAttributesAndChildren();

  // read the input XML file and update the DOM object
  typename DOMNodeXMLReader::Pointer reader = DOMNodeXMLReader::New();
  reader->SetOutput( domobj );
  reader->SetFileName( fn );
  reader->Update();

  // save the current working directory (WD), and change the WD to where the XML file is located
  FancyString sOldWorkingDir = itksys::SystemTools::GetCurrentWorkingDirectory();
  FancyString sNewWorkingDir = itksys::SystemTools::GetFilenamePath( fn );
  itksys::SystemTools::ChangeDirectory( sNewWorkingDir );

  this->Update( domobj );

  // change the WD back to the previously saved
  itksys::SystemTools::ChangeDirectory( sOldWorkingDir );
}

} // namespace itk

#endif // __itkDOMReader_hxx
