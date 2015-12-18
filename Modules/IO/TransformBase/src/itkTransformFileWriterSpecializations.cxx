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

#define ITK_TEMPLATE_EXPLICIT_TransformFileWriter
#include "itkTransformFileWriter.h"
#include "itkTransformFileWriter.hxx"
#include <string>

namespace itk
{
namespace
{

/*
 This helper is used to:
 Create and set a new type transform that have requested output precision type.
 */
template< typename TInputTransformType, typename TOutputTransformType>
struct TransformIOHelper
{
  typedef typename TOutputTransformType::Pointer                  OutputTransformPointer;
  typedef typename TInputTransformType::ParametersValueType       InputParameterValueType;
  typedef typename TInputTransformType::FixedParametersValueType  InputFixedParameterValueType;
  typedef typename TOutputTransformType::ParametersValueType      OutputParameterValueType;
  typedef typename TOutputTransformType::FixedParametersValueType OutputFixedParameterValueType;

  /*
   This function gets the type name of the input transform and creates
   a new transform object based on the requested precision type.
   */
  static OutputTransformPointer
  CreateNewTypeTransform(std::string transformName)
  {
    // Transform name is modified to have the output precision type.
    TransformIOBaseTemplate<OutputParameterValueType>::CorrectTransformPrecisionType( transformName );

    // Instantiate the transform
    LightObject::Pointer i = ObjectFactoryBase::CreateInstance ( transformName.c_str() );
    OutputTransformPointer convertedTransform = dynamic_cast< TOutputTransformType * >( i.GetPointer() );
    if( convertedTransform.IsNull() )
      {
      itkGenericExceptionMacro ( << "Could not create an instance of " << transformName );
      }
    convertedTransform->UnRegister();
    return convertedTransform;
  }

  /* Converts the value type of transform parameters to the output precision type */
  static OptimizerParameters< OutputParameterValueType >
  ConvertParametersType(const OptimizerParameters< InputParameterValueType >  &sourceParams)
  {
    OptimizerParameters< OutputParameterValueType > outputParams;
    outputParams.SetSize( sourceParams.GetSize() );
    for( SizeValueType i = 0; i < sourceParams.GetSize(); ++i )
      {
      outputParams[i] = static_cast<OutputParameterValueType>( sourceParams[i] );
      }
    return outputParams;
  }

  /* Converts the value type of transform parameters to the output precision type */
  static OptimizerParameters< OutputFixedParameterValueType >
  ConvertFixedParametersType(const OptimizerParameters< InputFixedParameterValueType >  &sourceParams)
  {
  OptimizerParameters< OutputFixedParameterValueType > outputParams;
  outputParams.SetSize( sourceParams.GetSize() );
  for( SizeValueType i = 0; i < sourceParams.GetSize(); ++i )
    {
    outputParams[i] = static_cast<OutputFixedParameterValueType>( sourceParams[i] );
    }
  return outputParams;
  }

  /* Set fixed parameters and parameters of the new type created transform */
  static void SetAllParameters(const TInputTransformType *transform, OutputTransformPointer& convertedTransform)
  {
    // The precision type of the input transform parameters should be converted to the requested output precision
    convertedTransform->SetFixedParameters( ConvertFixedParametersType( transform->GetFixedParameters() ) );
    convertedTransform->SetParameters( ConvertParametersType( transform->GetParameters() ) );
  }
};

} // end TransformFileWriterHelper namespace

namespace
{
/* Changes the precision type of input transform to the requested precision type */
template<typename TInputTransformType,
         typename TOutputTransformType>
inline void AddToTransformList(typename TInputTransformType::ConstPointer &transform,
                               std::list< typename TOutputTransformType::ConstPointer > & transformList)
  {
  typedef typename TInputTransformType::ParametersValueType       InputParameterValueType;
  typedef typename TOutputTransformType::ParametersValueType      OutputParameterValueType;

  /* Pushes the converted transform to the input transform list */
  typedef TransformBaseTemplate<InputParameterValueType>   InputTransformType;
  typedef typename InputTransformType::ConstPointer        InputTransformConstPointer;
  typedef std::list< InputTransformConstPointer >          InputConstTransformListType;

  typedef TransformBaseTemplate<OutputParameterValueType>  OutputTransformType;
  typedef typename OutputTransformType::Pointer            OutputTransformPointer;
  typedef typename OutputTransformType::ConstPointer       OutputTransformConstPointer;
  typedef std::list<OutputTransformPointer>                OutputTransformListType;

  const std::string transformName = transform->GetTransformTypeAsString();
  OutputTransformPointer convertedTransform;

  typedef TransformIOHelper<InputTransformType, OutputTransformType> IOhelper;

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
    CompositeTransformIOHelperTemplate<InputParameterValueType> inputHelper;
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
    CompositeTransformIOHelperTemplate<OutputParameterValueType> outputHelper;
    // set the output transform list into the new composite transform
    outputHelper.SetTransformList(convertedTransform.GetPointer(), compositeTransformList);
    }

  transformList.push_back( OutputTransformConstPointer(convertedTransform.GetPointer()) );
}

template<> //If types match, no conversion
void AddToTransformList<TransformBaseTemplate<double>,TransformBaseTemplate<double> >(
                      TransformBaseTemplate<double>::ConstPointer &transform,
                      std::list< TransformBaseTemplate<double>::ConstPointer > & transformList)
{
  transformList.push_back( transform );
}

template<> //If types match, no conversion
void AddToTransformList<TransformBaseTemplate<float>, TransformBaseTemplate<float> >(
                      TransformBaseTemplate<float>::ConstPointer &transform,
                      std::list< TransformBaseTemplate<float>::ConstPointer > & transformList)
{
  transformList.push_back( transform );
}


}

template<>
void TransformFileWriterTemplate<double>
::PushBackTransformList(const Object *transObj)
{
  TransformBaseTemplate<double>::ConstPointer dblptr = dynamic_cast<const TransformBaseTemplate<double> *>( transObj );
  if( dblptr.IsNotNull() )
    {
    AddToTransformList<TransformBaseTemplate<double>, TransformBaseTemplate<double> >( dblptr, m_TransformList );
    }
  else
    {
    TransformBaseTemplate<float>::ConstPointer fltptr = dynamic_cast<const TransformBaseTemplate<float> *>( transObj );
    if( fltptr.IsNotNull() )
      {
      AddToTransformList<TransformBaseTemplate<float>, TransformBaseTemplate<double> >( fltptr, m_TransformList );
      }
    else
      {
      itkExceptionMacro("The input of writer should be whether a double precision "
        "or a single precision transform type. Called from TransformFileWriterTemplate<double,double>::PushBackTransformList(...) "  );
      }
    }
}

template<>
void TransformFileWriterTemplate<float>
::PushBackTransformList(const Object *transObj)
{
  TransformBaseTemplate<double>::ConstPointer dblptr = dynamic_cast<const TransformBaseTemplate<double> *>( transObj );
  if( dblptr.IsNotNull() )
    {
    AddToTransformList<TransformBaseTemplate<double>, TransformBaseTemplate<float> >( dblptr, m_TransformList );
    }
  else
    {
    TransformBaseTemplate<float>::ConstPointer fltptr = dynamic_cast<const TransformBaseTemplate<float> *>( transObj );
    if( fltptr.IsNotNull() )
      {
      AddToTransformList<TransformBaseTemplate<float>, TransformBaseTemplate<float> >( fltptr, m_TransformList );
      }
    else
      {
      itkExceptionMacro("The input of writer should be whether a double precision "
        "or a single precision transform type. Called from TransformFileWriterTemplate<float,double>::PushBackTransformList(...) "  );
      }
    }
}

#ifdef ITK_HAS_GCC_PRAGMA_DIAG_PUSHPOP
  ITK_GCC_PRAGMA_DIAG_PUSH()
#endif
ITK_GCC_PRAGMA_DIAG(ignored "-Wattributes")

template class ITKIOTransformBase_EXPORT TransformFileWriterTemplate< double >;
template class ITKIOTransformBase_EXPORT TransformFileWriterTemplate< float >;

#ifdef ITK_HAS_GCC_PRAGMA_DIAG_PUSHPOP
  ITK_GCC_PRAGMA_DIAG_POP()
#else
  ITK_GCC_PRAGMA_DIAG(warning "-Wattributes")
#endif

}
