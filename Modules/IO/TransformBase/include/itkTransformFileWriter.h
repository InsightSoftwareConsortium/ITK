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
#ifndef itkTransformFileWriter_h
#define itkTransformFileWriter_h

#include "itkTransformIOBase.h"
#include <iostream>
#include <fstream>

namespace itk
{
  /** \class TransformFileWriterTemplate
   *
   * \brief TODO
   * \ingroup ITKIOTransformBase
   *
   * \wiki
   * \wikiexample{IO/TransformFileWriter,Write a transform to a file}
   * \endwiki
   */
template<typename TParametersValueType>
class TransformFileWriterTemplate:public LightProcessObject
{
public:

  /** SmartPointer typedef support */
  typedef TransformFileWriterTemplate Self;
  typedef LightProcessObject          Superclass;
  typedef SmartPointer<Self>          Pointer;
  typedef SmartPointer<const Self>    ConstPointer;

  typedef TParametersValueType                             ParametersValueType;
  typedef ParametersValueType /* double */                 FixedParametersValueType;
  typedef TransformBaseTemplate<ParametersValueType>       TransformType;
  typedef TransformIOBaseTemplate<ParametersValueType>     TransformIOType;
  typedef typename TransformIOType::TransformPointer       TransformPointer;
  typedef typename TransformIOType::ConstTransformPointer  ConstTransformPointer;
  typedef typename TransformIOType::ConstTransformListType ConstTransformListType;

  /** Method for creation through the object factory */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(TransformFileWriterTemplate, LightProcessObject);

  /** Set the filename  */
  itkSetStringMacro(FileName);

  /** Get the filename */
  itkGetStringMacro(FileName);

  /** Set/Get the write mode (append/overwrite) for the Filter */
  void SetAppendOff();

  void SetAppendOn();

  void SetAppendMode(bool mode);

  bool GetAppendMode();

  /** Set/Get the input transform to write */
  void SetInput(const Object *transform);

  const TransformType * GetInput();

  /** Add a transform to be written */
  void AddTransform(const Object *transform);

  /** Write out the transform */
  void Update();

  /** Set/Get the TransformIO class used internally to read to transform. */
  itkSetObjectMacro( TransformIO, TransformIOType );
  itkGetConstObjectMacro( TransformIO, TransformIOType );

protected:
  TransformFileWriterTemplate();
  virtual ~TransformFileWriterTemplate();

  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  void PushBackTransformList(const Object *transObj);
  void OpenStream(std::ofstream & out, bool binary);

  std::string                       m_FileName;
  ConstTransformListType            m_TransformList;
  bool                              m_AppendMode;
  typename TransformIOType::Pointer m_TransformIO;

  TransformFileWriterTemplate(const Self &); //purposely not implemented
  void operator=(const Self &);              //purposely not implemented
};

/** This helps to meet backward compatibility */
typedef itk::TransformFileWriterTemplate<double> TransformFileWriter;

} // namespace itk

#ifdef ITK_IO_FACTORY_REGISTER_MANAGER
#include "itkTransformIOFactoryRegisterManager.h"
#endif

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTransformFileWriter.hxx"
#endif

#endif // itkTransformFileWriter_h
