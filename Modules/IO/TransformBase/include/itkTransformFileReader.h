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
#ifndef itkTransformFileReader_h
#define itkTransformFileReader_h

#include "itkTransformIOBase.h"

namespace itk
{
  /** \class TransformFileReaderTemplate
   *
   * \brief TODO
   * \ingroup ITKIOTransformBase
   *
   * \wiki
   * \wikiexample{IO/TransformFileReader,Read a transform from a file}
   * \endwiki
   */
template<typename ScalarType>
class TransformFileReaderTemplate: public LightProcessObject
{
public:

  /** SmartPointer typedef support */
  typedef TransformFileReaderTemplate         Self;
  typedef SmartPointer< Self >                Pointer;
  typedef TransformBaseTemplate< ScalarType > TransformType;

  typedef typename TransformType::ParametersType      ParametersType;
  typedef TransformIOBaseTemplate< ScalarType >       TransformIOType;
  typedef typename TransformIOType::TransformPointer  TransformPointer;
  typedef typename TransformIOType::TransformListType TransformListType;

  /** Method for creation through the object factory */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  typedef Object Superclass;
  itkTypeMacro(TransformFileReaderTemplate, LightProcessObject);

  /** Set the filename  */
  itkSetStringMacro(FileName);

  /** Get the filename */
  itkGetStringMacro(FileName);

  /** Read the transform */
  virtual void Update();

  /** Get the list of transform */
  TransformListType * GetTransformList() { return &m_TransformList; }

  /** Set/Get the TransformIO class used internally to read to transform. */
  itkSetObjectMacro( TransformIO, TransformIOType );
  itkGetConstObjectMacro( TransformIO, TransformIOType );

protected:
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  TransformFileReaderTemplate();
  virtual ~TransformFileReaderTemplate();
  void CreateTransform(TransformPointer & ptr, const std::string & ClassName);

  TransformListType                 m_TransformList;
  typename TransformIOType::Pointer m_TransformIO;
  std::string                       m_FileName;

private:
  TransformFileReaderTemplate(const Self &); //purposely not implemented
  void operator=(const Self &);              //purposely not implemented
};

/** This helps to meet backward compatibility */
typedef itk::TransformFileReaderTemplate<double> TransformFileReader;

} // namespace itk

#ifdef ITK_IO_FACTORY_REGISTER_MANAGER
#include "itkTransformIOFactoryRegisterManager.h"
#endif

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTransformFileReader.hxx"
#endif

#endif // itkTransformFileReade_h
