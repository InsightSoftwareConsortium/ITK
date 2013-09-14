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
#ifndef __itkTransformIOBase_hxx
#define __itkTransformIOBase_hxx

#include "itkTransformIOBase.h"
#include "itkTransformFactoryBase.h"
#include <iostream>
#include <fstream>

namespace itk
{

namespace
{

/* The following struct returns the string name of computation type */
/* default implementation */
template <typename ScalarType>
inline const std::string GetTypeNameString()
{
  return std::string(typeid(ScalarType).name());
}

/* a specialization for each "float" and "double" type that we support
   and don't like the string returned by typeid */
template <>
inline const std::string GetTypeNameString<float>()
{
  return std::string("float");
}

template <>
inline const std::string GetTypeNameString<double>()
{
  return std::string("double");
}

template<typename ScalarType>
inline void CorrectTransformPrecisionType( std::string & inputTransformName )
{
  const std::string & outputScalarTypeAsString = GetTypeNameString<ScalarType>();
  if( inputTransformName.find(outputScalarTypeAsString) == std::string::npos ) // output precision type is not found in input transform.
    {
    const std::string floatString("float");
    const std::string doubleString("double");
    if( outputScalarTypeAsString.compare(floatString) == 0 ) // output precision type is float, so we should search for double and replace that.
      {
      const std::string::size_type begin = inputTransformName.find(doubleString);
      inputTransformName.replace(begin, doubleString.size(), floatString);
      }
    else
      {
      const std::string::size_type begin = inputTransformName.find(floatString);
      inputTransformName.replace(begin, floatString.size(), doubleString);
      }
    }
}
} // end anonymous namespace

template <typename ScalarType>
TransformIOBaseTemplate<ScalarType>
::TransformIOBaseTemplate() :
  m_AppendMode(false)
{
}

template <typename ScalarType>
TransformIOBaseTemplate<ScalarType>
::~TransformIOBaseTemplate()
{
}

template <typename ScalarType>
void TransformIOBaseTemplate<ScalarType>
::CreateTransform(TransformPointer & ptr, const std::string & ClassName)
{
  //
  // call to GetFactory has side effect of initializing the
  // TransformFactory overrides
  TransformFactoryBase *theFactory =
  TransformFactoryBase::GetFactory();

  // Instantiate the transform
  itkDebugMacro ("About to call ObjectFactory");
  LightObject::Pointer i = ObjectFactoryBase::CreateInstance ( ClassName.c_str() );
  itkDebugMacro ("After call ObjectFactory");
  ptr = dynamic_cast< TransformType * >( i.GetPointer() );
  if ( ptr.IsNull() )
    {
    std::ostringstream msg;
    msg << "Could not create an instance of " << ClassName << std::endl
    << "The usual cause of this error is not registering the "
    << "transform with TransformFactory" << std::endl;
    msg << "Currently registered Transforms: " << std::endl;
    std::list< std::string >           names = theFactory->GetClassOverrideWithNames();
    for ( std::list< std::string >::iterator it = names.begin(); it != names.end(); ++it )
      {
      msg << "\t\"" << *it << "\"" << std::endl;
      }
    itkExceptionMacro ( << msg.str() );
    }
  // Correct extra reference count from CreateInstance()
  ptr->UnRegister();
  }

template <typename ScalarType>
void TransformIOBaseTemplate<ScalarType>
::OpenStream(std::ofstream & outputStream, bool binary)
{
  std::ios::openmode mode(std::ios::out);

  if ( binary )
    {
    mode |= std::ios::binary;
    }
  if ( this->m_AppendMode )
    {
    mode |= std::ios::app;
    }

  outputStream.open(m_FileName.c_str(), mode);

  if ( outputStream.fail() )
    {
    outputStream.close();
    itkExceptionMacro("Failed opening file" << m_FileName);
    }
}

template <typename ScalarType>
void TransformIOBaseTemplate<ScalarType>
::SetTransformList(ConstTransformListType & transformList)
{
  this->m_WriteTransformList = transformList;
}

template <typename ScalarType>
void TransformIOBaseTemplate<ScalarType>
::PrintSelf(std::ostream & os, Indent indent) const
{
Superclass::PrintSelf(os, indent);

os << indent << "FileName: " << m_FileName << std::endl;
os << indent << "AppendMode: "
<< ( m_AppendMode ? "true" : "false" ) << std::endl;
if ( m_ReadTransformList.size() > 0 )
  {
  os << indent << "ReadTransformList: " << std::endl;
  typename TransformListType::const_iterator it = m_ReadTransformList.begin();
  while ( it != m_ReadTransformList.end() )
    {
    ( *it )->Print( os, indent.GetNextIndent() );
    ++it;
    }
  }
if ( m_WriteTransformList.size() > 0 )
  {
  os << indent << "WriteTransformList: " << std::endl;

  typename ConstTransformListType::const_iterator it = m_WriteTransformList.begin();
  while ( it != m_WriteTransformList.end() )
    {
    ( *it )->Print( os, indent.GetNextIndent() );
    ++it;
    }
  }
}
} // itk

#endif
