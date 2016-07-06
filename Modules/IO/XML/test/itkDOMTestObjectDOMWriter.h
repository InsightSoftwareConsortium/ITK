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

#ifndef itkDOMTestObjectDOMWriter_h
#define itkDOMTestObjectDOMWriter_h

#include "itkDOMWriter.h"
#include "itkDOMTestObject.h"
#include "itkFileTools.h"

namespace itk
{

class DOMTestObjectDOMWriter : public DOMWriter<DOMTestObject>
{
public:
  /** Standard class typedefs. */
  typedef DOMTestObjectDOMWriter      Self;
  typedef DOMWriter<DOMTestObject>    Superclass;
  typedef SmartPointer< Self >        Pointer;
  typedef SmartPointer< const Self >  ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(DOMTestObjectDOMWriter, DOMWriter);

protected:
  DOMTestObjectDOMWriter() {}

  /**
   * This function is called automatically when update functions are performed.
   * It should fill the contents of the intermediate DOM object by pulling information from the input object.
   */
  virtual void GenerateData( DOMNodeType* outputdom, const void* ) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(DOMTestObjectDOMWriter);
};

inline void
DOMTestObjectDOMWriter::GenerateData( DOMNodeType* outputdom, const void* ) const
{
  const InputType* input = this->GetInput();

  std::ofstream ofs;
  FancyString fn;

  outputdom->SetName( "DOMTestObject" );

  // write child foo
  fn = input->GetFooFileName();
  DOMNodePointer foo = DOMNodeType::New();
  foo->SetName( "foo" );
  foo->SetAttribute( "fname", fn );
  outputdom->AddChild( foo );
  // create the output file if it does not exist
  FileTools::CreateFile( fn );
  // write the foo value to file
  ofs.open( fn.ToString().c_str() );
  if ( !ofs.is_open() )
    {
    itkExceptionMacro( "cannot write foo file" );
    }
  ofs << input->GetFooValue();
  ofs.close();
}

} // namespace itk

#endif // itkDOMTestObjectDOMWriter_h
