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
#ifndef itkTransformFileWriter_hxx
#define itkTransformFileWriter_hxx

#include "itkTransformFileWriter.h"
#include "itkTransformFactoryBase.h"
#include "itkTransformIOFactory.h"
#include "itkCompositeTransformIOHelper.h"

namespace itk
{


template<typename TParametersValueType>
TransformFileWriterTemplate<TParametersValueType>
::TransformFileWriterTemplate() :
  m_FileName(""),
  m_AppendMode(false)
{
  TransformFactoryBase::RegisterDefaultTransforms();
}

template<typename TParametersValueType>
TransformFileWriterTemplate<TParametersValueType>
::~TransformFileWriterTemplate()
{
}

/** Set the writer to append to the specified file */
template<typename TParametersValueType>
void TransformFileWriterTemplate<TParametersValueType>
::SetAppendOn()
{
  this->SetAppendMode(true);
}

/** Set the writer to overwrite the specified file - This is the
* default mode. */
template<typename TParametersValueType>
void TransformFileWriterTemplate<TParametersValueType>
::SetAppendOff()
{
  this->SetAppendMode(false);
}

/** Set the writer mode (append/overwrite). */
template<typename TParametersValueType>
void TransformFileWriterTemplate<TParametersValueType>
::SetAppendMode(bool mode)
{
  this->m_AppendMode = mode;
}

/** Get the writer mode. */
template<typename TParametersValueType>
bool TransformFileWriterTemplate<TParametersValueType>
::GetAppendMode()
{
  return ( this->m_AppendMode );
}

template<>
void
TransformFileWriterTemplate<double>
::PushBackTransformList(const Object *transObj);

template<>
void
TransformFileWriterTemplate<float>
::PushBackTransformList(const Object *transObj);

/** Set the input transform and reinitialize the list of transforms */
template<typename TParametersValueType>
void TransformFileWriterTemplate<TParametersValueType>
::SetInput(const Object *transform)
{
  m_TransformList.clear();
  this->PushBackTransformList(transform);
}

template<typename TParametersValueType>
const typename TransformFileWriterTemplate<TParametersValueType>::TransformType *
TransformFileWriterTemplate<TParametersValueType>
::GetInput()
{
  ConstTransformPointer res = *(m_TransformList.begin());
  return res.GetPointer();
}

/** Add a transform to be written */
template<typename TParametersValueType>
void TransformFileWriterTemplate<TParametersValueType>
::AddTransform(const Object *transform)
{
  /* Check for a CompositeTransform.
   * The convention is that there should be one, and it should
   * be the first transform in the file
   */
  const std::string transformName = transform->GetNameOfClass();
  if( transformName.find("CompositeTransform") != std::string::npos )
    {
    if(this->m_TransformList.size() > 0)
      {
      itkExceptionMacro("Can only write a transform of type CompositeTransform "
                        "as the first transform in the file.");
      }
    }

  this->PushBackTransformList(transform);
}

template<typename TParametersValueType>
void TransformFileWriterTemplate<TParametersValueType>
::Update()
{
  if ( m_FileName == "" )
    {
    itkExceptionMacro ("No file name given");
    }

  if( m_TransformIO.IsNull() )
    {
    typedef TransformIOFactoryTemplate<TParametersValueType> TransformFactoryIOType;
    m_TransformIO = TransformFactoryIOType::CreateTransformIO( m_FileName.c_str(), WriteMode );

    if ( m_TransformIO.IsNull() )
      {
      std::ostringstream msg;
      msg << "Could not create Transform IO object for writing file "  << this->GetFileName() << std::endl;

      std::list< LightObject::Pointer > allobjects =  ObjectFactoryBase::CreateAllInstance("itkTransformIOBaseTemplate");

      if (allobjects.size() > 0)
        {
        msg << "  Tried to create one of the following:" << std::endl;
        for ( std::list< LightObject::Pointer >::iterator i = allobjects.begin();
              i != allobjects.end(); ++i )
          {
          const Object *obj = dynamic_cast<Object*>(i->GetPointer());
          msg << "    " << obj->GetNameOfClass() << std::endl;
          }
        msg << "  You probably failed to set a file suffix, or" << std::endl;
        msg << "    set the suffix to an unsupported type." << std::endl;
        }
      else
        {
        msg << "  There are no registered Transform IO factories." << std::endl;
        msg << "  Please visit https://www.itk.org/Wiki/ITK/FAQ#NoFactoryException to diagnose the problem." << std::endl;
        }

      itkExceptionMacro( << msg.str().c_str() );
      }
    }

  m_TransformIO->SetAppendMode(this->m_AppendMode);
  m_TransformIO->SetFileName(this->m_FileName);
  m_TransformIO->SetTransformList(this->m_TransformList);
  m_TransformIO->Write();
}

template<typename TParametersValueType>
void TransformFileWriterTemplate<TParametersValueType>
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FileName: " << m_FileName << std::endl;
}

} // namespace itk

#endif
