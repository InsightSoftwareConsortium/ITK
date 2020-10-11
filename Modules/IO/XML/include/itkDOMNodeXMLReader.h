/*=========================================================================
 *
 *  Copyright NumFOCUS
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

#ifndef itkDOMNodeXMLReader_h
#define itkDOMNodeXMLReader_h

#include "itkDOMNode.h"
#include "itkObject.h"
#include "ITKIOXMLExport.h"

#include <istream>

namespace itk
{

/**
 * \class DOMNodeXMLReader
 * \brief Class to read a DOM object from an XML file or an input stream.
 *
 * This class produces a DOM object, which can be subsequently used to create a
 * corresponding user object.
 *
 * We do not recommend to directly use this class for user object reading, because the user has to handle
 * both DOM object generation from the input XML file and user object generation from the intermediate DOM object.
 * Users should derive from the DOMReader, because it performs the former step automatically and lets the users
 * concentrate on the latter step, thus implementation of such a reader is simplified.
 *
 * Note: Though this class behaves similar to ProcessObject, it is not derived from ProcessObject. This is
 *       because the output of this class, i.e. a DOMNode object, is not a DataObject, thus this class cannot
 *       be connected to an ITK process pipeline.
 *
 * The following code snippet demonstrates how to read a DOM object from an XML file:
 *
 *     itk::DOMNode::Pointer output_dom_object;
 *     const char* input_xml_file_name = ...
 *     itk::DOMNodeXMLReader::Pointer reader = itk::DOMNodeXMLReader::New();
 *     reader->SetFileName( input_xml_file_name );
 *     reader->Update();
 *     output_dom_object = reader->GetOutput();
 *
 * \sa DOMReader
 * \sa DOMNode
 *
 * \ingroup ITKIOXML
 */
class ITKIOXML_EXPORT DOMNodeXMLReader : public Object
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(DOMNodeXMLReader);

  /** Standard class type aliases. */
  using Self = DOMNodeXMLReader;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(DOMNodeXMLReader, Object);

  using OutputType = DOMNode;
  using OutputPointer = OutputType::Pointer;

  /** Set the input XML filename. */
  itkSetStringMacro(FileName);

  /** Get the input XML filename. */
  itkGetStringMacro(FileName);

  /**
   * Get/Set The output DOM object will be created automatically, but the user
   * can appoint a user DOM object as the output by calling this function.
   */
  itkSetObjectMacro(DOMNodeXML, OutputType);
#if !defined(ITK_LEGACY_REMOVE)
  // Provide backwards compatible interface
  virtual void
  SetOutput(OutputType * _arg)
  {
    this->SetDOMNodeXML(_arg);
  }
#endif

  /**
   * Provide an interface to match that
   * of other ProcessObjects
   * for this source generation object
   * by returning a non-const pointer
   * for the generated Object.
   */
  // NOTE:  The m_DOMNodeXML is only
  //       exposed via the Source generation interface
  //       by the GetOutput() method that mimics
  //       a process object.
  virtual const OutputType *
  GetOutput() const
  {
    return this->m_DOMNodeXML.GetPointer();
  }
  virtual OutputType *
  GetOutput()
  {
    return this->m_DOMNodeXML.GetPointer();
  }

#if !defined(ITK_LEGACY_REMOVE)
  // This interface was exposed in ITKv4 when the itkGetModifiableObjectMacro was used
  virtual OutputType *
  GetModifiedOutput()
  {
    return this->m_DOMNodeXML.GetPointer();
  }
#endif

  /**
   * Function called by Update() or end-users to generate the output DOM object
   * from an input stream such as file, string, etc.
   */
  void
  Update(std::istream & is);

  /**
   * Function called by end-users to generate the output DOM object from the input XML file.
   */
  virtual void
  Update();

  /** Callback function -- called from XML parser with start-of-element
   * information.
   */
  virtual void
  StartElement(const char * name, const char ** atts);

  /** Callback function -- called from XML parser when ending tag
   * encountered.
   */
  virtual void
  EndElement(const char * name);

  /** Callback function -- called from XML parser with the character data
   * for an XML element.
   */
  virtual void
  CharacterDataHandler(const char * text, int len);

protected:
  DOMNodeXMLReader();

private:
  /** Variable to hold the input XML file name. */
  std::string m_FileName;

  /** Variable to hold the output DOM object, created internally or supplied by the user. */
  OutputPointer m_DOMNodeXML;

  /** Variable to keep the current context during XML parsing. */
  OutputType * m_Context{ nullptr };
};

} // namespace itk

/** The operator ">>" is overloaded such that a DOM object can be conveniently read from an input stream. */
inline std::istream &
operator>>(std::istream & is, itk::DOMNode & object)
{
  itk::DOMNodeXMLReader::Pointer reader = itk::DOMNodeXMLReader::New();
  reader->SetDOMNodeXML(&object);
  reader->Update(is);
  return is;
}

#endif // itkDOMNodeXMLReader_h
