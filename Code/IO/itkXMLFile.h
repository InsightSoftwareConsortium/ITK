/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkXMLFile.h
  Language:  C++
  Date:      $Date$
  Version:   $1.0$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkXMLFile_h
#define __itkXMLFile_h
#include "itkLightProcessObject.h"
#include "expat.h"
#include <fstream>

namespace itk
{

/** \class XMLReaderBase
 * XMLReaderBase encapsulates the expat library (Insight/Utilities/expat
 * and defines the methods needed in a derived class to receive the
 * contents of an XML file in a structured manner.  It's 'impure virtual'
 * in that some functions that are generic to opening and parsing a file
 * are implemented here.
 */
class 
XMLReaderBase : public LightProcessObject
{
public:
  /** Set the filename to write */
  itkSetStringMacro(Filename);

  /** Get the filename to write */
  itkGetStringMacro(Filename);

  /** determine whether a file can be opened and read */
  virtual int CanReadFile(const char* name) = 0;
  /** do the actual parsing of the input file */
  virtual void GenerateOutputInformation();
  /** Callback function -- called from XML parser with start-of-element
   * information.
   */
  virtual void StartElement(const char * name,const char **atts) = 0;
  /** Callback function -- called from XML parser when ending tag
   * encountered
   */
  virtual void EndElement(const char *name) = 0;
  /** Callback function -- called from XML parser with the character data
   * for an XML element
   */
  virtual void CharacterDataHandler(const char *inData, int inLength) = 0;
protected:
  /** Instantiates and invokes the XML parser for the file named by
   * m_Filename.  The parser will throw an exception in the case of XML
   * syntax errors, missing filenames, unreadable input file, etc.
   */
  void parse(void);
  std::string m_Filename;
};

/** \class XMLReader -- template base class for an XMLReader
 * It's purpose really is just to define the simple interface for
 * extracting the object resulting from reading the XML File.
 * Since it doesn't define any of the pure virtual methods in XMLReaderBase,
 * It can't be instantiated by itself
 */
template <class T> class 
XMLReader : public XMLReaderBase
{
public:
  /** Set the output object.  Doesn't make sense for a client of the XMLReader,
   * but could be used in derived class to assign pointer to result object.
   */
  void SetOutputObject(T *obj) { m_OutputObject = obj; }
  /** Get the output object, after an XML File has been successfully parsed.
   */
  T *GetOutputObject(void) { return m_OutputObject; }
protected:
  T *m_OutputObject;
};

/** \class XMLWriterBase
 *
 * 'Impure virtual' base class for XML File writing.
 * Defines the interface for an XML file writer and provides
 * a few utility functions for writing XML files. A derived
 * class needs to implement writing the file completely by
 * implementing WriteFile.
 */
template <class T>
class XMLWriterBase : public LightProcessObject
{
public:
  /** Constructor
   * Sets object pointer to zero.
   */
  XMLWriterBase() {
    m_InputObject = 0;
  }
  /** Set the filename to write */
  itkSetStringMacro(Filename);
  /** Get the filename to write */
  itkGetStringMacro(Filename);
  /** Return non-zero if the filename given is writeable. */
  virtual int CanWriteFile(const char* name) = 0;
  /** Give a pointer to the object to be written out to an XML file. */
  void SetObject(T *toWrite) { m_InputObject = toWrite; }
  /** Write the XML file, based on the Input Object */
  virtual int WriteFile() = 0;
  /** Write out a start element tag */
  void WriteStartElement(const char *const tag,std::ofstream &file)
  {
    file << '<' << tag << '>';
  }
  /** Write an end element tag */
  void WriteEndElement(const char *const tag,std::ofstream &file) 
  {
    file << '<' << '/'  << tag << '>';
  }
  /** Write character data inside a tag. */
  void WriteCharacterData(const char *const data,std::ofstream &file) 
  {
    file << data;
  }
  /** Write a start element tag */
  void WriteStartElement(std::string &tag,std::ofstream &file) 
  {
    WriteStartElement(tag.c_str(),file);
  }
  /** Write an end element tag */
  void WriteEndElement(std::string &tag,std::ofstream &file) 
  {
    WriteEndElement(tag.c_str(),file);
  }
  /** Write character data inside a tag. */
  void WriteCharacterData(std::string &data,std::ofstream &file)
  {
    WriteCharacterData(data.c_str(),file);
  }
protected:
  T *m_InputObject;             // object to write out to an XML file
  std::string m_Filename;       // name of file to write.
};

}
#endif
