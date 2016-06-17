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

#ifndef itkDOMNodeXMLWriter_h
#define itkDOMNodeXMLWriter_h

#include "itkDOMNode.h"
#include "itkObject.h"
#include "ITKIOXMLExport.h"

#include <ostream>

namespace itk
{

/**
 * \class DOMNodeXMLWriter
 * \brief Class to write a DOM object to an XML file or an output stream.
 *
 * This class takes a user DOM object and write it to a destination such as file, string, console, etc.
 * Before using this class, the DOM object should have been generated from a user object.
 *
 * We do not recommend to directly use this class for user object writing, because the user has to handle
 * both DOM object generation from the user object and XML output from the intermediate DOM object.
 * Users should derive from the DOMWriter, because it performs the latter step automatically and lets the users
 * concentrate on the former step, thus implementation of such a writer is simplified.
 *
 * Note: Though this class behaves similar to ProcessObject, it is not derived from ProcessObject. This is
 *       because the input of this class, i.e. a DOMNode object, is not a DataObject, thus this class cannot
 *       be connected to an ITK process pipeline.
 *
 * The following code snippet demonstrates how to write a DOM object to an XML file:
 *
 *     itk::DOMNode::Pointer input_dom_object = ...
 *     const char* output_xml_file_name = ...
 *     itk::DOMNodeXMLWriter::Pointer writer = itk::DOMNodeXMLWriter::New();
 *     writer->SetInput( input_dom_object );
 *     writer->SetFileName( output_xml_file_name );
 *     writer->Update();
 *
 * \sa DOMWriter
 * \sa DOMNode
 *
 * \ingroup ITKIOXML
 */
class ITKIOXML_EXPORT DOMNodeXMLWriter : public Object
{
public:
  /** Standard class typedefs. */
  typedef DOMNodeXMLWriter            Self;
  typedef Object                      Superclass;
  typedef SmartPointer< Self >        Pointer;
  typedef SmartPointer< const Self >  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(DOMNodeXMLWriter, Object);

  typedef DOMNode                 InputType;
  typedef InputType::ConstPointer ConstInputPointer;

  /** Set the output XML filename. */
  itkSetStringMacro(FileName);

  /** Get the output XML filename. */
  itkGetStringMacro(FileName);

  /** Get/Set the input DOM object to be written. */
  itkSetConstObjectMacro( Input, InputType );
  itkGetConstObjectMacro(Input, InputType );

  /**
   * Function called by Update() or end-users to write the input DOM object
   * to an output stream such as file, string, console, etc.
   */
  void Update( std::ostream& os, std::string indent = "" );

  /**
   * Function called by end-users to write the input DOM object to the output XML file.
   */
  virtual void Update();

protected:
  DOMNodeXMLWriter();

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(DOMNodeXMLWriter);

  /** Variable to hold the output XML file name. */
  std::string m_FileName;

  /** Variable to hold the input DOM object. */
  ConstInputPointer m_Input;

  /** Variable to hold the indentation (i.e. number of white spaces) for a child node w.r.t. its parent. */
  std::string m_IndentStep;
};

} // namespace itk

/** The operator "<<" is overloaded such that a DOM object can be conveniently write to an output stream. */
inline std::ostream& operator<<( std::ostream& os, const itk::DOMNode& object )
{
  itk::DOMNodeXMLWriter::Pointer writer = itk::DOMNodeXMLWriter::New();
  writer->SetInput( &object );
  writer->Update( os );
  return os;
}

#endif // itkDOMNodeXMLWriter_h
