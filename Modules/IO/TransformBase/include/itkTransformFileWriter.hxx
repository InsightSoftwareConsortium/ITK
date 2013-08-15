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
#ifndef __itkTransformFileWriter_hxx
#define __itkTransformFileWriter_hxx

#include "itkTransformFileWriter.h"
#include "itkTransformFactoryBase.h"
#include "itkTransformIOFactory.h"

namespace itk
{
template<class ScalarType>
TransformFileWriterTemplate<ScalarType>
::TransformFileWriterTemplate() :
  m_FileName(""),
  m_AppendMode(false)
{
  TransformFactoryBase::RegisterDefaultTransforms();
}

template<class ScalarType>
TransformFileWriterTemplate<ScalarType>
::~TransformFileWriterTemplate()
{
}

/** Set the writer to append to the specified file */
template<class ScalarType>
void TransformFileWriterTemplate<ScalarType>
::SetAppendOn()
{
  this->SetAppendMode(true);
}

/** Set the writer to overwrite the specified file - This is the
* default mode. */
template<class ScalarType>
void TransformFileWriterTemplate<ScalarType>
::SetAppendOff()
{
  this->SetAppendMode(false);
}

/** Set the writer mode (append/overwrite). */
template<class ScalarType>
void TransformFileWriterTemplate<ScalarType>
::SetAppendMode(bool mode)
{
  this->m_AppendMode = mode;
}

/** Get the writer mode. */
template<class ScalarType>
bool TransformFileWriterTemplate<ScalarType>
::GetAppendMode()
{
  return ( this->m_AppendMode );
}

/** Set the input transform and reinitialize the list of transforms */
template<class ScalarType>
void TransformFileWriterTemplate<ScalarType>
::SetInput(const TransformType *transform)
{
  m_TransformList.clear();
  m_TransformList.push_back( ConstTransformPointer(transform) );
}

template<class ScalarType>
const typename TransformFileWriterTemplate<ScalarType>::TransformType *
TransformFileWriterTemplate<ScalarType>
::GetInput()
{
  ConstTransformPointer res = *(m_TransformList.begin());
  return res.GetPointer();
}

/** Add a transform to be written */
template<class ScalarType>
void TransformFileWriterTemplate<ScalarType>
::AddTransform(const TransformType *transform)
{
  /* Check for a CompositeTransform.
   * The convention is that there should be one, and it should
   * be the first transform in the file
   */
  std::string transformName = transform->GetNameOfClass();
  if( transformName.find("CompositeTransform") != std::string::npos )
    {
    if(this->m_TransformList.size() > 0)
      {
      itkExceptionMacro("Can only write a transform of type CompositeTransform "
                        "as the first transform in the file.");
      }
    }

  m_TransformList.push_back( ConstTransformPointer(transform) );
}

template<class ScalarType>
void TransformFileWriterTemplate<ScalarType>
::Update()
{
  if ( m_FileName == "" )
    {
    itkExceptionMacro ("No file name given");
    }
  typename TransformIOBaseTemplate<ScalarType>::Pointer transformIO =
    TransformIOFactoryTemplate<ScalarType>::CreateTransformIO( m_FileName.c_str(), /*TransformIOFactoryTemplate<ScalarType>::*/ WriteMode );
  if ( transformIO.IsNull() )
    {
    itkExceptionMacro("Can't Create IO object for file "
                      << m_FileName);
    }
  transformIO->SetAppendMode(this->m_AppendMode);
  transformIO->SetFileName(m_FileName);
  transformIO->SetTransformList(this->m_TransformList);
  transformIO->Write();
}

template<class ScalarType>
void TransformFileWriterTemplate<ScalarType>
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FileName: " << m_FileName << std::endl;
}

} // namespace itk

#endif
