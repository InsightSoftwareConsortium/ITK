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
#include "itkTransformFileWriter.hxx"
namespace itk
{

namespace
{
/* Changes the precision type of input transform to the requested precision type */
template<typename TOutputScalar, typename TInputScalar>
inline void AddToTransformList(typename TransformBaseTemplate<TInputScalar>::ConstPointer &transform,
                               std::list< typename TransformBaseTemplate<TOutputScalar>::ConstPointer > & transformList)
  {
  /* Pushes the converted transform to the input transform list */
  typedef TransformBaseTemplate<TInputScalar>        InputTransformType;
  typedef typename InputTransformType::ConstPointer  InputTransformConstPointer;
  typedef std::list< InputTransformConstPointer >    InputConstTransformListType;
  typedef TransformBaseTemplate<TOutputScalar>       OutputTransformType;
  typedef typename OutputTransformType::Pointer      OutputTransformPointer;
  typedef typename OutputTransformType::ConstPointer OutputTransformConstPointer;
  typedef std::list< OutputTransformPointer >        OutputTransformListType;
  typedef std::list< OutputTransformConstPointer >   OutputConstTransformListType;

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

template<>
void AddToTransformList<double,double>(TransformBaseTemplate<double>::ConstPointer &transform,
                                        std::list< TransformBaseTemplate<double>::ConstPointer > & transformList)
{
  transformList.push_back( TransformBaseTemplate<double>::ConstPointer(transform) );
}

template<>
void AddToTransformList<float,float>(TransformBaseTemplate<float>::ConstPointer &transform,
                                      std::list< TransformBaseTemplate<float>::ConstPointer > & transformList)
{
  transformList.push_back( TransformBaseTemplate<float>::ConstPointer(transform) );
}

}

template<>
void TransformFileWriterTemplate<double>
::PushBackTransformList(const Object *transObj)
{
  TransformBaseTemplate<double>::ConstPointer dblptr = dynamic_cast<const TransformBaseTemplate<double> *>( transObj );
  if( dblptr.IsNotNull() )
    {
    AddToTransformList<double, double>( dblptr, m_TransformList );
    }
  else
    {
    TransformBaseTemplate<float>::ConstPointer fltptr = dynamic_cast<const TransformBaseTemplate<float> *>( transObj );
    if( fltptr.IsNotNull() )
      {
      AddToTransformList<double, float>( fltptr, m_TransformList );
      }
    else
      {
      itkExceptionMacro("The input of writer should be whether a double precision"
                        "or a single precision transform type.");
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
    AddToTransformList<float, double>( dblptr, m_TransformList );
    }
  else
    {
    TransformBaseTemplate<float>::ConstPointer fltptr = dynamic_cast<const TransformBaseTemplate<float> *>( transObj );
    if( fltptr.IsNotNull() )
      {
      AddToTransformList<float, float>( fltptr, m_TransformList );
      }
    else
      {
      itkExceptionMacro("The input of writer should be whether a double precision"
                        "or a single precision transform type.");
      }
    }
}

}
