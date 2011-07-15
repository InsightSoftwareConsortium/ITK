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
#ifndef __itkTransformFileReader_h
#define __itkTransformFileReader_h

#include "itkTransformIOBase.h"

namespace itk
{
/** \class TransformFileReader
 *
 * \brief TODO
 * \ingroup ITKTransform
 *
 * \wiki
 * \wikiexample{IO/TransformFileReader,Read a transform from a file}
 * \endwiki
 */
class ITK_EXPORT TransformFileReader:public LightProcessObject
{
public:

  /** SmartPointer typedef support */
  typedef TransformFileReader                 Self;
  typedef SmartPointer< Self >                Pointer;
  typedef TransformBase                       TransformType;

  typedef TransformType::ParametersType      ParametersType;
  typedef TransformIOBase::TransformPointer  TransformPointer;
  typedef TransformIOBase::TransformListType TransformListType;

  /** Method for creation through the object factory */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  typedef Object Superclass;
  itkTypeMacro(TransformFileReader, LightProcessObject);

  /** Set the filename  */
  itkSetStringMacro(FileName);

  /** Get the filename */
  itkGetStringMacro(FileName);

  /** Read the transform */
  virtual void Update();

  /** Get the list of transform */
  TransformListType * GetTransformList() { return &m_TransformList; }
protected:
  TransformIOBase::Pointer m_TransformIO;
  TransformFileReader(const Self &); //purposely not implemented
  void operator=(const Self &);      //purposely not implemented
  void PrintSelf(std::ostream & os, Indent indent) const;

  std::string m_FileName;

  TransformFileReader();
  virtual ~TransformFileReader();
  void CreateTransform(TransformPointer & ptr, const std::string & ClassName);

  /** Flag set by CompositeTransformReader.
   * \warning This is TEMPORARY and should only be used
   * internally. Used for testing during transition to final CompositeTransform
   * IO method */
  bool m_ReadingCompositeTransform;

private:
  TransformListType m_TransformList;
};
} // namespace itk

#endif // __itkTransformFileReader_h
