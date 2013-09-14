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
template<class TOutputScalar, class TInputScalar>
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
    CorrectTransformPrecisionType<TOutputScalar>( transformName );

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

/* Changes the precision type of input transform to the requested precision type */
template<typename TOutputScalar, typename TInputScalar>
inline void AddToTransformList(typename TransformBaseTemplate<TInputScalar>::ConstPointer &transform, std::list< typename TransformBaseTemplate<TOutputScalar>::ConstPointer > & transformList)
  {
  /* Pushes the converted transform to the input transform list */
  typedef TransformBaseTemplate<TInputScalar>        InputTransformType;
  typedef typename InputTransformType::ConstPointer  InputTransformConstPointer;
  typedef std::list< InputTransformConstPointer >    InputConstTransformListType;
  typedef TransformBaseTemplate<TOutputScalar>        OutputTransformType;
  typedef typename OutputTransformType::Pointer       OutputTransformPointer;
  typedef typename OutputTransformType::ConstPointer  OutputTransformConstPointer;
  typedef std::list< OutputTransformPointer >         OutputTransformListType;
  typedef std::list< OutputTransformConstPointer >    OutputConstTransformListType;

  const std::string transformName = transform->GetTransformTypeAsString();
  OutputTransformPointer convertedTransform;

  typedef TransformIOHelper<TOutputScalar, TInputScalar> IOhelper;

  // Composite and DisplacementFieldTransform transforms should be treated differently.
  if( transformName.find("CompositeTransform") == std::string::npos )
    {
    convertedTransform = IOhelper::CreateNewTypeTransform( transformName );
    IOhelper::SetAllParameters(transform, convertedTransform);
    }
  else
    {
    /*
     Following steps are needed to process a composite transform:
     1) Use the compositeTransformIOHelper to get the input transforms list.
     2) Iterate through the input transform list, convert each sub transform and put them in the output transform list.
     3) Use a composite IO Helper agian to set the output transform list into the converted composite transform.
    */
    CompositeTransformIOHelperTemplate<TInputScalar> inputHelper;
    InputConstTransformListType inputTransformList = inputHelper.GetTransformList( transform );

    // create output transform list
    OutputTransformListType compositeTransformList;
    /*
     The first transform of the output transform list should be a composite transform
     we push back just an empty composite transform as it will be skipped by the outputHelper.
    */
    OutputTransformPointer outputComposite = IOhelper::CreateNewTypeTransform( transformName );
    compositeTransformList.push_back( outputComposite.GetPointer() );

    // Now we iterate through input list and convert each sub transform to a new transform with requested precision type.
    typename InputConstTransformListType::iterator it = inputTransformList.begin();
    // composite transform is the first transform of the input transform list
    ++it; // skip the composite transform
    for(; it != inputTransformList.end(); ++it)
       {
       // get the input sub transform
       const InputTransformType *inSub = dynamic_cast< const InputTransformType *>( (*it).GetPointer() );
       // convert each sub transform and push them to the output transform list
       std::string inSubName = inSub->GetTransformTypeAsString();
       OutputTransformPointer convertedSub = IOhelper::CreateNewTypeTransform( inSubName );
       IOhelper::SetAllParameters( inSub, convertedSub );
       // push back the converted sub transform to the composite transform list
       compositeTransformList.push_back( convertedSub.GetPointer() );
       }

    convertedTransform = IOhelper::CreateNewTypeTransform( transformName ); // new composite transform
    CompositeTransformIOHelperTemplate<TOutputScalar> outputHelper;
    // set the output transform list into the new composite transform
    outputHelper.SetTransformList(convertedTransform.GetPointer(), compositeTransformList);
    }

  transformList.push_back( OutputTransformConstPointer(convertedTransform.GetPointer()) );
}

/* Precision type conversion is not needed when the input transform already has the requested precision type */
template<>
inline void AddToTransformList<double,double>(TransformBaseTemplate<double>::ConstPointer &transform, std::list< TransformBaseTemplate<double>::ConstPointer > & transformList)
{
  transformList.push_back( TransformBaseTemplate<double>::ConstPointer(transform) );
}

template<>
inline void AddToTransformList<float,float>(TransformBaseTemplate<float>::ConstPointer &transform, std::list< TransformBaseTemplate<float>::ConstPointer > & transformList)
{
  transformList.push_back( TransformBaseTemplate<float>::ConstPointer(transform) );
}

} // end anonymous namespace

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

template<class ScalarType>
void TransformFileWriterTemplate<ScalarType>
::PushBackTransformList(const Object *transObj)
{
  TransformBaseTemplate<double>::ConstPointer dblptr = dynamic_cast<const TransformBaseTemplate<double> *>( transObj );
  if( dblptr.IsNotNull() )
    {
    AddToTransformList<ScalarType,double>( dblptr, m_TransformList );
    }
  else
    {
    TransformBaseTemplate<float>::ConstPointer fltptr = dynamic_cast<const TransformBaseTemplate<float> *>( transObj );
    if( fltptr.IsNotNull() )
      {
      AddToTransformList<ScalarType,float>( fltptr, m_TransformList );
      }
    else
      {
      itkExceptionMacro("The input of writer should be whether a double precision"
                        "or a single precision transform type.");
      }
    }
}

/** Set the input transform and reinitialize the list of transforms */
template<class ScalarType>
void TransformFileWriterTemplate<ScalarType>
::SetInput(const Object *transform)
{
  m_TransformList.clear();
  this->PushBackTransformList(transform);
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

template<class ScalarType>
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

template<class ScalarType>
void TransformFileWriterTemplate<ScalarType>
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FileName: " << m_FileName << std::endl;
}

} // namespace itk

#endif
