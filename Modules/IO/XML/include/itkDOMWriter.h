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

#ifndef itkDOMWriter_h
#define itkDOMWriter_h

#include "itkDOMNodeXMLWriter.h"
#include "itkObject.h"
#include "itkLogger.h"

namespace itk
{

/**
 * \class DOMWriter
 * \brief Class to write an ITK object to an XML file or a DOM object, using the DOM APIs.
 *
 * End-users need to derive from this class to implement writers for user objects. In subclasses, users need
 * to provide an implementation for the virtual function GenerateData(-,-).
 *
 * This class performs similar functions as the XMLWriterBase - both provide the base for handling object writing to
 * an XML destination. The difference is that, writers derived from this class perform object writing using the
 * easy-to-use DOM APIs, while XMLWriterBase-based writers directly generate textual XML documents, which is tedious
 * and more error prone.
 *
 * Internally, this class first generates an intermediate DOM object from the input object, then the DOM object
 * is implicitly written to the output XML file using the DOMNodeXMLWriter.
 *
 * Note: Though this class behaves similar to ProcessObject, it is not derived from ProcessObject. This is
 *       because many user objects to be written, e.g., registrations, transforms, optimizers, and so on, are not objects
 *       of type DataObject, thus this class cannot be connected to an ITK process pipeline.
 *
 * The following code snippet demontrates how to use a DOM-based writer that is derived from this class:
 *
 * \code
 *     itk::MyObjectType::Pointer input_object = ...
 *     const char* output_xml_file_name = ...
 *     itk::MyObjectDOMWriter::Pointer writer = itk::MyObjectDOMWriter::New();
 *     writer->SetInput( input_object );
 *     writer->SetFileName( output_xml_file_name );
 *     writer->Update();
 * \endcode
 *
 * \sa XMLWriterBase
 * \sa DOMNodeXMLWriter
 * \sa DOMNode
 *
 * \ingroup ITKIOXML
 */
template< typename TInput >
class ITK_TEMPLATE_EXPORT DOMWriter : public Object
{
public:
  /** Standard class typedefs. */
  typedef DOMWriter Self;

  itkTypeMacro(DOMWriter, Object);

  typedef TInput                            InputType;

  typedef DOMNode                           DOMNodeType;
  typedef typename DOMNodeType::Pointer     DOMNodePointer;

  typedef Logger                            LoggerType;
  typedef typename LoggerType::Pointer      LoggerPointer;

  /** Set the output XML filename. */
  itkSetStringMacro(FileName);

  /** Get the output XML filename. */
  itkGetStringMacro(FileName);

  /** Set the input object to be written. */
  virtual void SetInput( const InputType* input );

  /** Get the input object to be written. */
  const InputType* GetInput() const;

  /**
   * Return the internal logger so that users can change the
   * output format or add/remove logging destinations.
   */
  itkGetConstMacro( Logger, LoggerType* );

  /**
   * Function called by Update() or end-users to write the input object to a DOM object.
   * Some derived writers may accept an incomplete input object during the writing process, in those cases
   * the optional argument 'userdata' can be used to provide the missed information.
   */
  void Update( DOMNodeType* outputdom, const void* userdata = ITK_NULLPTR );

  /**
   * Function called by end-users to write the input object to the output XML file.
   */
  virtual void Update();

protected:
  DOMWriter();

  /**
   * Function to be implemented in subclasses. It is called automatically
   * when update functions are performed. It should fill the contents of the intermediate DOM object
   * by pulling information from the input object.
   * Some derived writers may accept an incomplete input object during the writing process, in those cases
   * the optional argument 'userdata' can be used to provide the missed information.
   */
  virtual void GenerateData( DOMNodeType* outputdom, const void* userdata ) const = 0;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(DOMWriter);

  /** Get/Set the intermediate DOM object. */
  itkSetObjectMacro( IntermediateDOM, DOMNodeType );
  itkGetModifiableObjectMacro(IntermediateDOM, DOMNodeType );

  /** Variable to hold the output XML file name. */
  std::string m_FileName;

  /** Variable to hold the input object. */
  const InputType* m_Input;
  /** Variable to hold the input object if it is a smart object. */
  typename LightObject::ConstPointer m_InputHolder;

  /** Variable to hold the intermediate DOM object. */
  DOMNodePointer m_IntermediateDOM;

  /** Variable to log various messages during the writing process. */
  mutable LoggerPointer m_Logger;
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDOMWriter.hxx"
#endif

#endif // itkDOMWriter_h
