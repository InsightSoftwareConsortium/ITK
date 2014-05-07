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
#ifndef __itkCompositeTransformIOHelper_hxx
#define __itkCompositeTransformIOHelper_hxx

#include "itkCompositeTransformIOHelper.h"

namespace itk
{
template <typename TScalar>
typename CompositeTransformIOHelperTemplate<TScalar>::ConstTransformListType &
CompositeTransformIOHelperTemplate<TScalar>
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

template <typename TScalar>
void
CompositeTransformIOHelperTemplate<TScalar>
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

template <typename TScalar>
template <unsigned TDim>
int
CompositeTransformIOHelperTemplate<TScalar>
::BuildTransformList(const TransformType *transform)
{
  //
  // see if we've found the right type
  typedef CompositeTransform<TScalar,TDim> CompositeType;

  const CompositeType *composite = dynamic_cast<const CompositeType *>(transform);
  if(composite == ITK_NULLPTR)
    {
    //
    // if not, return zero
    return 0;
    }

  //
  // push the composite on the list first, as per the convention for
  // the TransformFileReader
  this->m_TransformList.push_back(const_cast<TransformType *>(transform));

  const typename CompositeType::TransformQueueType &transforms =
  composite->GetTransformQueue();
  for(typename CompositeType::TransformQueueType::const_iterator it =
      transforms.begin(); it != transforms.end(); ++it)
    {
    const TransformType *curTransform = dynamic_cast<const TransformType *>((*it).GetPointer());
    if(curTransform == ITK_NULLPTR)
      {
      itkGenericExceptionMacro(<< "Failure to convert transform of type "
                               << (*it)->GetTransformTypeAsString()
                               << " to itk::TransformBase");
      }
    ConstTransformPointer curPtr = curTransform;
    this->m_TransformList.push_back(curPtr);
    }
  return 1;
}

template <typename TScalar>
template <unsigned TDim>
int
CompositeTransformIOHelperTemplate<TScalar>
::InternalSetTransformList(TransformType *transform,TransformListType &transformList)
{
  //
  // local composite transform type
  typedef itk::CompositeTransform<TScalar,TDim>      CompositeType;
  typedef typename CompositeType::TransformType      ComponentTransformType;

  //
  // see if we've found the right type
  CompositeType *composite = dynamic_cast<CompositeType *>(transform);
  if(composite == ITK_NULLPTR)
    {
    //
    // if not, we'll try then next dim down
    return 0;
    }
  //
  // iterate thru transform list and assign into Composite
  typename TransformListType::iterator it = transformList.begin();
  ++it;                         // skip the composite transform
  for(; it != transformList.end(); ++it)
    {
    ComponentTransformType *component =
    dynamic_cast<ComponentTransformType *>((*it).GetPointer());
    if(component == ITK_NULLPTR)
      {
      itkGenericExceptionMacro(<< "Can't assign transform of type "
                               << (*it)->GetTransformTypeAsString()
                               << " to a Composite Transform of type "
                               << composite->GetTransformTypeAsString());
      }
    composite->AddTransform(component);
    }
  return 1;
}

} // namespace itk

#endif
