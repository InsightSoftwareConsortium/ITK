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
#ifndef itkTransformIOBase_h
#define itkTransformIOBase_h

#include "ITKIOTransformBaseExport.h"

#include "itkLightProcessObject.h"
#include "itkTransformBase.h"
#include <list>
#include <iostream>
#include <fstream>
#include <string>

#ifndef ITKIOTransformBase_TEMPLATE_EXPORT
  #if defined(ITK_TEMPLATE_VISIBILITY_DEFAULT) || defined(__linux__) && defined(ITK_BUILD_SHARED_LIBS)
    // Make everything visible
    #define ITKIOTransformBase_TEMPLATE_EXPORT __attribute__ ((visibility ("default")))
  #else
    #define ITKIOTransformBase_TEMPLATE_EXPORT
  #endif
#endif

namespace itk
{

/** \class TransformIOBaseTemplate
 *
 * \brief Abstract superclass defining the Transform IO interface.
 *
 * TransformIOBaseTemplate is a pure virtual base class for derived classes that
 * read/write transform data considering the type of input transform.
 * First, TransformIOBase is derived from this class for legacy read/write transform.
 * This class also is used by the TransformFileReader and TransformFileWriter
 * classes. End users don't directly manipulate classes derived from TransformIOBaseTemplate;
 * the TransformIOFactory is used by the Reader/Writer to pick a concrete derived class to do
 * the actual reading/writing of transforms.
 *
 * \ingroup ITKIOTransformBase
 */
template<typename TParametersValueType>
class ITKIOTransformBase_TEMPLATE_EXPORT TransformIOBaseTemplate:public LightProcessObject
{
public:
  /** Standard class typedefs */
  typedef TransformIOBaseTemplate Self;
  typedef LightProcessObject      Superclass;
  typedef SmartPointer<Self>      Pointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(TransformIOBaseTemplate, Superclass);

  /** Transform types */
  typedef TParametersValueType              ScalarType; //For backwards compatibility
  typedef TParametersValueType              ParametersValueType;
  typedef double                            FixedParametersValueType;

  typedef TransformBaseTemplate<ParametersValueType> TransformType;

  /** For writing, a const transform list gets passed in, for
   * reading, a non-const transform list is created from the file.
   */
  typedef typename TransformType::Pointer             TransformPointer;
  typedef std::list< TransformPointer >               TransformListType;
  typedef typename TransformType::ConstPointer        ConstTransformPointer;
  typedef std::list< ConstTransformPointer >          ConstTransformListType;

  /** Set/Get the name of the file to be read. */
  itkSetStringMacro(FileName);
  itkGetStringMacro(FileName);

  /** Reads the data from disk into the memory buffer provided. */
  virtual void Read() = 0;

  /** Writes the transform list to disk. */
  virtual void Write() = 0;

  /** Determine the file type. Returns true if this TransformIO can read the
   * file specified. */
  virtual bool CanReadFile(const char *) = 0;

  /** Determine the file type. Returns true if this TransformIO can read the
   * file specified. */
  virtual bool CanWriteFile(const char *)  = 0;

  /** Get the list of transforms resulting from a file read */
  TransformListType & GetTransformList() { return m_ReadTransformList; }
  TransformListType & GetReadTransformList() { return m_ReadTransformList; }
  ConstTransformListType & GetWriteTransformList() { return m_WriteTransformList; }

  /** Set the list of transforms before writing */
  void SetTransformList(ConstTransformListType & transformList);

  /** Set the writer to append to the specified file */
  itkSetMacro(AppendMode, bool);
  itkGetConstMacro(AppendMode, bool);
  itkBooleanMacro(AppendMode);

  /** The transform type has a string representation used when reading
   * and writing transform files.  In the case where a double-precision
   * transform is to be written as float, or vice versa, the transform
   * type name used to write the file needs to patched in order for the
   * transform I/O hierachy to work correctly. These template functions
   * will be chosen at compile time within template classes in order to
   * patch up the type name.
   *  */
  static inline void CorrectTransformPrecisionType( std::string & itkNotUsed(inputTransformName) )
    {
    itkGenericExceptionMacro(<< "Unknown ScalarType" << typeid(ScalarType).name());
    }

protected:
  TransformIOBaseTemplate();
  virtual ~TransformIOBaseTemplate() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  void OpenStream(std::ofstream & outputStream, bool binary);

  void CreateTransform(TransformPointer & ptr, const std::string & ClassName);

  std::string            m_FileName;
  TransformListType      m_ReadTransformList;
  ConstTransformListType m_WriteTransformList;
  bool                   m_AppendMode;

  /* The following struct returns the string name of computation type */
  /* default implementation */
  static inline const std::string GetTypeNameString()
    {
    itkGenericExceptionMacro(<< "Unknown ScalarType" << typeid(ScalarType).name());
    }
};


template <>
inline void
TransformIOBaseTemplate<float>
::CorrectTransformPrecisionType( std::string & inputTransformName )
{
  // output precision type is not found in input transform.
 if(inputTransformName.find("float") == std::string::npos)
   {
   const std::string::size_type begin = inputTransformName.find("double");
   inputTransformName.replace(begin, 6, "float");
   }
}

template <>
inline void
TransformIOBaseTemplate<double>
::CorrectTransformPrecisionType( std::string & inputTransformName )
{
  // output precision type is not found in input transform.
 if(inputTransformName.find("double") == std::string::npos)
   {
   const std::string::size_type begin = inputTransformName.find("float");
   inputTransformName.replace(begin, 5, "double");
   }
}

template <>
inline const std::string
TransformIOBaseTemplate<float>
::GetTypeNameString()
{
  return std::string("float");
}

template <>
inline const std::string
TransformIOBaseTemplate<double>
::GetTypeNameString()
{
  return std::string("double");
}

/** This helps to meet backward compatibility */
typedef itk::TransformIOBaseTemplate<double> TransformIOBase;

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkTransformIOBase.hxx"
#endif

#endif // itkTransformIOBase_h

/** Explicit instantiations */
#ifndef ITK_TEMPLATE_EXPLICIT_TransformIOBase
// Explicit instantiation is required to ensure correct dynamic_cast
// behavior across shared libraries.
//
// IMPORTANT: Since within the same compilation unit,
//            ITK_TEMPLATE_EXPLICIT_<classname> defined and undefined states
//            need to be considered. This code *MUST* be *OUTSIDE* the header
//            guards.
//
#  if defined( ITKIOTransformBase_EXPORTS )
//   We are building this library
#    define ITKIOTransformBase_EXPORT_EXPLICIT ITK_FORWARD_EXPORT
#  else
//   We are using this library
#    define ITKIOTransformBase_EXPORT_EXPLICIT ITKIOTransformBase_EXPORT
#  endif
namespace itk
{

#ifdef ITK_HAS_GCC_PRAGMA_DIAG_PUSHPOP
  ITK_GCC_PRAGMA_DIAG_PUSH()
#endif
ITK_GCC_PRAGMA_DIAG(ignored "-Wattributes")

extern template class ITKIOTransformBase_EXPORT_EXPLICIT TransformIOBaseTemplate< double >;
extern template class ITKIOTransformBase_EXPORT_EXPLICIT TransformIOBaseTemplate< float >;

#ifdef ITK_HAS_GCC_PRAGMA_DIAG_PUSHPOP
  ITK_GCC_PRAGMA_DIAG_POP()
#else
  ITK_GCC_PRAGMA_DIAG(warning "-Wattributes")
#endif

} // end namespace itk
#  undef ITKIOTransformBase_EXPORT_EXPLICIT
#endif
