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
#ifndef itkCompositeTransformIOHelper_hxx
#define itkCompositeTransformIOHelper_hxx

#include "itkCompositeTransformIOHelper.h"

namespace
{
  template <unsigned int VDimension>
  inline std::string GetTransformDimensionAsString()
  {
    std::string rval("unknown");
    return rval;
  }

#ifdef ITK_HAS_GCC_PRAGMA_DIAG_PUSHPOP
  ITK_GCC_PRAGMA_DIAG_PUSH()
#endif
ITK_GCC_PRAGMA_DIAG(ignored "-Wunused-function")
  template<>
  std::string GetTransformDimensionAsString<2>()
  {
    std::string rval("2_2");
    return rval;
  }

  template<>
  std::string GetTransformDimensionAsString<3>()
  {
    std::string rval("3_3");
    return rval;
  }

  template<>
  std::string GetTransformDimensionAsString<4>()
  {
    std::string rval("4_4");
    return rval;
  }

  template<>
  std::string GetTransformDimensionAsString<5>()
  {
    std::string rval("5_5");
    return rval;
  }

  template<>
  std::string GetTransformDimensionAsString<6>()
  {
    std::string rval("6_6");
    return rval;
  }

  template<>
  std::string GetTransformDimensionAsString<7>()
  {
    std::string rval("7_7");
    return rval;
  }

  template<>
  std::string GetTransformDimensionAsString<8>()
  {
    std::string rval("8_8");
    return rval;
  }

  template<>
  std::string GetTransformDimensionAsString<9>()
  {
    std::string rval("9_9");
    return rval;
  }
#ifdef ITK_HAS_GCC_PRAGMA_DIAG_PUSHPOP
  ITK_GCC_PRAGMA_DIAG_POP()
#else
  ITK_GCC_PRAGMA_DIAG(warning "-Wunused-function")
#endif
}

namespace itk
{
template<typename TParametersValueType>
typename CompositeTransformIOHelperTemplate<TParametersValueType>::ConstTransformListType &
CompositeTransformIOHelperTemplate<TParametersValueType>
::GetTransformList(const TransformType *transform)
{
  this->m_TransformList.clear();

  // try each CompositeTransform Type, starting with
  // most common
  if(this->BuildTransformList<3>(transform) == 0 &&
     this->BuildTransformList<2>(transform) == 0 &&
     this->BuildTransformList<4>(transform) == 0 &&
     this->BuildTransformList<5>(transform) == 0 &&
     this->BuildTransformList<6>(transform) == 0 &&
     this->BuildTransformList<7>(transform) == 0 &&
     this->BuildTransformList<8>(transform) == 0 &&
     this->BuildTransformList<9>(transform) == 0)
    {
    itkGenericExceptionMacro(<< "Unsupported Composite Transform Type "
                             << transform->GetTransformTypeAsString());
    }
  return m_TransformList;
}

template<typename TParametersValueType>
void
CompositeTransformIOHelperTemplate<TParametersValueType>
::SetTransformList(TransformType *transform,TransformListType &transformList)
{
  // try each CompositeTransform Type, starting with
  // most common
  if(this->InternalSetTransformList<3>(transform,transformList) == 0 &&
     this->InternalSetTransformList<2>(transform,transformList) == 0 &&
     this->InternalSetTransformList<4>(transform,transformList) == 0 &&
     this->InternalSetTransformList<5>(transform,transformList) == 0 &&
     this->InternalSetTransformList<6>(transform,transformList) == 0 &&
     this->InternalSetTransformList<7>(transform,transformList) == 0 &&
     this->InternalSetTransformList<8>(transform,transformList) == 0 &&
     this->InternalSetTransformList<9>(transform,transformList) == 0)
    {
    itkGenericExceptionMacro(<< "Unsupported Composite Transform Type "
                             << transform->GetTransformTypeAsString());
    }
}

template<typename TParametersValueType>
template <unsigned int VDimension>
int
CompositeTransformIOHelperTemplate<TParametersValueType>
::BuildTransformList(const TransformType *transform)
{
  //
  // see if we've found the right type
  typedef CompositeTransform<TParametersValueType, VDimension> CompositeType;

  const std::string CompositeTransformTypeName = transform->GetTransformTypeAsString();
  if(CompositeTransformTypeName.find("CompositeTransform") == std::string::npos || CompositeTransformTypeName.find(GetTransformDimensionAsString<VDimension>()) == std::string::npos)
    {
    return 0;
    }
  const CompositeType *composite = static_cast<const CompositeType *>(transform);

  //
  // push the composite on the list first, as per the convention for
  // the TransformFileReader
  this->m_TransformList.push_back(const_cast<TransformType *>(transform));

  const typename CompositeType::TransformQueueType &transforms =
  composite->GetTransformQueue();
  for(typename CompositeType::TransformQueueType::const_iterator it =
      transforms.begin(); it != transforms.end(); ++it)
    {
    const TransformType *curTransform = static_cast<const TransformType *>((*it).GetPointer());
    ConstTransformPointer curPtr = curTransform;
    this->m_TransformList.push_back(curPtr);
    }
  return 1;
}

template<typename TParametersValueType>
template <unsigned int VDimension>
int
CompositeTransformIOHelperTemplate<TParametersValueType>
::InternalSetTransformList(TransformType *transform,TransformListType &transformList)
{
  //
  // local composite transform type
  typedef itk::CompositeTransform<TParametersValueType, VDimension> CompositeType;
  typedef typename CompositeType::TransformType              ComponentTransformType;

  //
  // see if we've found the right type
  const std::string CompositeTransformTypeName = transform->GetTransformTypeAsString();
  if(CompositeTransformTypeName.find("CompositeTransform") == std::string::npos || CompositeTransformTypeName.find(GetTransformDimensionAsString<VDimension>()) == std::string::npos)
    {
    //
    // if not, we'll try then next dim down
    return 0;
    }
  CompositeType *composite = static_cast<CompositeType *>(transform);

  //
  // iterate thru transform list and assign into Composite
  typename TransformListType::iterator it = transformList.begin();
  ++it;                         // skip the composite transform
  for(; it != transformList.end(); ++it)
    {
    ComponentTransformType *component = static_cast<ComponentTransformType *>((*it).GetPointer());
    composite->AddTransform(component);
    }
  return 1;
}

} // namespace itk

#endif
