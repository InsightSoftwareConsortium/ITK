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

#include "itkDOMNodeXMLReader.h"
#include "expat.h"

#include <fstream>

namespace itk
{

/**
 * The following three functions are called by the expat XML parser during the parsing process,
 * and the calls are forwarded to the callback functions of DOMNodeXMLReader.
 */

static void itkXMLParserStartElement( void* parser, const char* name, const char** atts )
{
  // Begin element handler that is registered with the XML_Parser.
  // This just casts the user data to a itkXMLParser and calls
  // StartElement.
  static_cast<DOMNodeXMLReader*>(parser)->StartElement( name, atts );
}

static void itkXMLParserEndElement( void* parser, const char* name )
{
  // End element handler that is registered with the XML_Parser.  This
  // just casts the user data to a itkXMLParser and calls EndElement.
  static_cast<DOMNodeXMLReader*>(parser)->EndElement( name );
}

static void itkXMLParserCharacterDataHandler( void* parser, const char* data, int length )
{
  // Character data handler that is registered with the XML_Parser.
  // This just casts the user data to a itkXMLParser and calls
  // CharacterDataHandler.
  static_cast<DOMNodeXMLReader*>(parser)->CharacterDataHandler( data, length );
}

DOMNodeXMLReader::DOMNodeXMLReader() : m_Context(ITK_NULLPTR)
{
}

/**
 * Function called by Update() or end-users to generate the output DOM object
 * from an input stream such as file, string, etc.
 */
void
DOMNodeXMLReader::Update( std::istream& is )
{
  if ( m_Output.IsNull() )
    {
    OutputType::Pointer temp = OutputType::New();
    this->SetOutput( temp );
    }
  m_Output->RemoveAllAttributesAndChildren();
  this->m_Context = ITK_NULLPTR;

  is >> std::noskipws;
  std::string s;
  while ( true )
    {
    char c = 0;
    is >> c;
    if ( !is.good() )
      {
      break;
      }
    s.append( 1, c );
    }

  XML_Parser parser = XML_ParserCreate( ITK_NULLPTR );
  XML_SetElementHandler( parser, &itkXMLParserStartElement, &itkXMLParserEndElement );
  XML_SetCharacterDataHandler( parser, &itkXMLParserCharacterDataHandler );
  XML_SetUserData( parser, this );

  bool ok = XML_Parse( parser, s.data(), static_cast<int>( s.size() ), false );
  if ( !ok )
    {
    ExceptionObject e( __FILE__, __LINE__ );
    std::string message( XML_ErrorString(XML_GetErrorCode(parser)) );
    e.SetDescription( message.c_str() );
    throw e;
    }

  XML_ParserFree( parser );
}

/**
 * Function called by end-users to generate the output DOM object from the input XML file.
 */
void
DOMNodeXMLReader::Update()
{
  std::ifstream is( this->m_FileName.c_str() );
  if ( !is.is_open() )
    {
    itkExceptionMacro( "failed openning the input XML file" );
    }

  this->Update( is );

  is.close();
}

/** Callback function -- called from XML parser with start-of-element
 * information.
 */
void
DOMNodeXMLReader::StartElement( const char* name, const char** atts )
{
  OutputType* node = ITK_NULLPTR;
  if ( this->m_Context )
    {
    OutputPointer node1 = OutputType::New();
    node = (OutputType*)node1;
    this->m_Context->AddChildAtEnd( node );
    }
  else
    {
    node = this->m_Output;
    }

  node->SetName( name );

  size_t i = 0;
  while ( atts[i] )
    {
    std::string key( atts[i++] );
    std::string value( atts[i++] );
    if ( StringTools::MatchWith(key,"id") )
      {
      node->SetID( value );
      }
    else
      {
      node->SetAttribute( key, value );
      }
    }

  this->m_Context = node;
}

/** Callback function -- called from XML parser when ending tag
 * encountered.
 */
void
DOMNodeXMLReader::EndElement( const char* name )
{
  if ( this->m_Context->GetName() != name )
    {
    itkExceptionMacro( "start/end tag names mismatch" );
    }

  this->m_Context = this->m_Context->GetParent();
}

/** Callback function -- called from XML parser with the character data
 * for an XML element.
 */
void
DOMNodeXMLReader::CharacterDataHandler( const char* text, int len )
{
  std::string s( text, len );

  StringTools::Trim( s );
  if ( s.size() == 0 )
    {
    return;
    }

  this->m_Context->AddTextChildAtEnd( s );
}

} // namespace itk
