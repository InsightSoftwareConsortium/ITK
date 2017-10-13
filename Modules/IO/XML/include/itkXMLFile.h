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
#ifndef itkXMLFile_h
#define itkXMLFile_h
#include "itkLightProcessObject.h"
#include "ITKIOXMLExport.h"
#include <fstream>

namespace itk
{
/** \class XMLReaderBase
 * XMLReaderBase encapsulates the expat library (Insight/Utilities/expat
 * and defines the methods needed in a derived class to receive the
 * contents of an XML file in a structured manner.  It's 'impure virtual'
 * in that some functions that are generic to opening and parsing a file
 * are implemented here.
 * \ingroup ITKIOXML
 */
class ITKIOXML_EXPORT XMLReaderBase:public LightProcessObject
{
public:
  typedef XMLReaderBase Self;

  /** Set the filename to write */
  itkSetStringMacro(Filename);

  /** Get the filename to write */
  itkGetStringMacro(Filename);

  /** determine whether a file can be opened and read */
  virtual int CanReadFile(const char *name) = 0;

  /** do the actual parsing of the input file */
  virtual void GenerateOutputInformation();

  /** Callback function -- called from XML parser with start-of-element
   * information.
   */
  virtual void StartElement(const char *name, const char **atts) = 0;

  /** Callback function -- called from XML parser when ending tag
   * encountered
   */
  virtual void EndElement(const char *name) = 0;

  /** Callback function -- called from XML parser with the character data
   * for an XML element
   */
  virtual void CharacterDataHandler(const char *inData, int inLength) = 0;

protected:
  XMLReaderBase() {}
  virtual ~XMLReaderBase() ITK_OVERRIDE {}
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  /** Instantiates and invokes the XML parser for the file named by
   * m_Filename.  The parser will throw an exception in the case of XML
   * syntax errors, missing filenames, unreadable input file, etc.
   */
  void parse();

  std::string m_Filename;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(XMLReaderBase);
};

/** \class XMLReader
 * \brief template base class for an XMLReader
 * It's purpose really is just to define the simple interface for
 * extracting the object resulting from reading the XML File.
 * Since it doesn't define any of the pure virtual methods in XMLReaderBase,
 * It can't be instantiated by itself
 * \ingroup ITKIOXML
 */
template< typename T >
class XMLReader: public XMLReaderBase
{
public:
  typedef XMLReader Self;

  /** Set the output object.  Doesn't make sense for a client of the XMLReader,
   * but could be used in derived class to assign pointer to result object.
   */
  void SetOutputObject(T *obj) { m_OutputObject = obj; }
  /** Get the output object, after an XML File has been successfully parsed.
   */
  T * GetOutputObject(void) { return m_OutputObject; }

protected:
  XMLReader() :
    m_OutputObject(ITK_NULLPTR)
  {}

  virtual ~XMLReader() {}

  T *m_OutputObject;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(XMLReader);
};

/** \class XMLWriterBase
 *
 * 'Impure virtual' base class for XML File writing.
 * Defines the interface for an XML file writer and provides
 * a few utility functions for writing XML files. A derived
 * class needs to implement writing the file completely by
 * implementing WriteFile.
 * \ingroup ITKIOXML
 */
template< typename T >
class XMLWriterBase:public LightProcessObject
{
public:
  typedef XMLWriterBase Self;

  /** Constructor
   * Sets object pointer to zero.
   */
  XMLWriterBase()
  {
    m_InputObject = ITK_NULLPTR;
  }

  /** Set the filename to write */
  itkSetStringMacro(Filename);
  /** Get the filename to write */
  itkGetStringMacro(Filename);
  /** Return non-zero if the filename given is writeable. */
  virtual int CanWriteFile(const char *name) = 0;

  /** Give a pointer to the object to be written out to an XML file. */
  void SetObject(T *toWrite) { m_InputObject = toWrite; }
  /** Write the XML file, based on the Input Object */
  virtual int WriteFile() = 0;

  /** Write out a start element tag */
  void WriteStartElement(const char *const tag, std::ofstream & file)
  {
    file << '<' << tag << '>';
  }

  /** Write an end element tag */
  void WriteEndElement(const char *const tag, std::ofstream & file)
  {
    file << '<' << '/'  << tag << '>';
  }

  /** Write character data inside a tag. */
  void WriteCharacterData(const char *const data, std::ofstream & file)
  {
    file << data;
  }

  /** Write a start element tag */
  void WriteStartElement(std::string & tag, std::ofstream & file)
  {
    WriteStartElement(tag.c_str(), file);
  }

  /** Write an end element tag */
  void WriteEndElement(std::string & tag, std::ofstream & file)
  {
    WriteEndElement(tag.c_str(), file);
  }

  /** Write character data inside a tag. */
  void WriteCharacterData(std::string & data, std::ofstream & file)
  {
    WriteCharacterData(data.c_str(), file);
  }

protected:
  T          *m_InputObject;    // object to write out to an XML file
  std::string m_Filename;       // name of file to write.

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(XMLWriterBase);
};
}
#endif
