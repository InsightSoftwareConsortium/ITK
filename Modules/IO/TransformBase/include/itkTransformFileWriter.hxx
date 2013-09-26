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
#include "itkCompositeTransformIOHelper.h"

namespace itk
{

namespace
{

/*
 This helper is used to:
 Create and set a new type transform that have requested output precision type.
 */
template<typename TOutputScalar, typename TInputScalar>
struct TransformIOHelper
{
  typedef TransformBaseTemplate<TInputScalar>     InputTransformType;
  typedef TransformBaseTemplate<TOutputScalar>    OutputTransformType;
  typedef typename OutputTransformType::Pointer   OutputTransformPointer;

  /*
   This function gets the type name of the input transform and creates
   a new transform object based on the requested precision type.
   */
  static OutputTransformPointer
  CreateNewTypeTransform(std::string transformName)
  {
    // Transform name is modified to have the output precision type.
    TransformIOBaseTemplate<TOutputScalar>::CorrectTransformPrecisionType( transformName );

    OutputTransformPointer convertedTransform;
    // Instantiate the transform
    LightObject::Pointer i = ObjectFactoryBase::CreateInstance ( transformName.c_str() );
    convertedTransform = dynamic_cast< OutputTransformType * >( i.GetPointer() );
    if( convertedTransform.IsNull() )
      {
      itkGenericExceptionMacro ( << "Could not create an instance of " << transformName );
      }
    convertedTransform->UnRegister();
    return convertedTransform;
  }

  /* Converts the value type of transform parameters to the output precision type */
  static OptimizerParameters< TOutputScalar >
  ConvertParametersType(const OptimizerParameters< TInputScalar >  &sourceParams)
  {
    OptimizerParameters< TOutputScalar > outputParams;
    outputParams.SetSize( sourceParams.GetSize() );
    for( SizeValueType i = 0; i < sourceParams.GetSize(); ++i )
      {
      outputParams[i] = static_cast<TOutputScalar>( sourceParams[i] );
      }
    return outputParams;
  }

  /* Set fixed parameters and parameters of the new type created transform */
  static void SetAllParameters(const InputTransformType *transform, OutputTransformPointer& convertedTransform)
  {
    // The precision type of the input transform parameters should be converted to the requested output precision
    convertedTransform->SetFixedParameters( ConvertParametersType( transform->GetFixedParameters() ) );
    convertedTransform->SetParameters( ConvertParametersType( transform->GetParameters() ) );
  }
};

} // end TransformFileWriterHelper namespace

template<typename ScalarType>
TransformFileWriterTemplate<ScalarType>
::TransformFileWriterTemplate() :
  m_FileName(""),
  m_AppendMode(false)
{
  TransformFactoryBase::RegisterDefaultTransforms();
}

template<typename ScalarType>
TransformFileWriterTemplate<ScalarType>
::~TransformFileWriterTemplate()
{
}

/** Set the writer to append to the specified file */
template<typename ScalarType>
void TransformFileWriterTemplate<ScalarType>
::SetAppendOn()
{
  this->SetAppendMode(true);
}

/** Set the writer to overwrite the specified file - This is the
* default mode. */
template<typename ScalarType>
void TransformFileWriterTemplate<ScalarType>
::SetAppendOff()
{
  this->SetAppendMode(false);
}

/** Set the writer mode (append/overwrite). */
template<typename ScalarType>
void TransformFileWriterTemplate<ScalarType>
::SetAppendMode(bool mode)
{
  this->m_AppendMode = mode;
}

/** Get the writer mode. */
template<typename ScalarType>
bool TransformFileWriterTemplate<ScalarType>
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
template<typename ScalarType>
void TransformFileWriterTemplate<ScalarType>
::SetInput(const Object *transform)
{
  m_TransformList.clear();
  this->PushBackTransformList(transform);
}

template<typename ScalarType>
const typename TransformFileWriterTemplate<ScalarType>::TransformType *
TransformFileWriterTemplate<ScalarType>
::GetInput()
{
  ConstTransformPointer res = *(m_TransformList.begin());
  return res.GetPointer();
}

/** Add a transform to be written */
template<typename ScalarType>
void TransformFileWriterTemplate<ScalarType>
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

template<typename ScalarType>
void TransformFileWriterTemplate<ScalarType>
::Update()
{
  if ( m_FileName == "" )
    {
    itkExceptionMacro ("No file name given");
    }
  typename TransformIOBaseTemplate<ScalarType>::Pointer transformIO =
    TransformIOFactoryTemplate<ScalarType>::CreateTransformIO( m_FileName.c_str(), WriteMode );
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

template<typename ScalarType>
void TransformFileWriterTemplate<ScalarType>
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FileName: " << m_FileName << std::endl;
}

} // namespace itk

#endif
