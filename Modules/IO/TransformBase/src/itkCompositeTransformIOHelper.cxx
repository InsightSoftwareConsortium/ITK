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

#include "itkCompositeTransformIOHelper.h"
#include "itkCompositeTransform.h"

namespace itk
{

/** build transform list from CompositeTransform */
template <typename TScalar,unsigned TDim>
int
CompositeTransformIOHelper
::BuildTransformList(const TransformType *transform)
{
  //
  // see if we've found the right type
  typedef CompositeTransform<TScalar,TDim> CompositeType;

  const CompositeType *composite = dynamic_cast<const CompositeType *>(transform);
  if(composite == 0)
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
    if(curTransform == 0)
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

/** fill transform queue for a CompositeTransform */
template <typename TScalar,unsigned TDim>
int
CompositeTransformIOHelper
::InternalSetTransformList(TransformType *transform,TransformListType &transformList)
{
  //
  // local composite transform type
  typedef CompositeTransform<TScalar,TDim>      CompositeType;
  typedef typename CompositeType::TransformType ComponentTransformType;

  //
  // see if we've found the right type
  CompositeType *composite = dynamic_cast<CompositeType *>(transform);
  if(composite == 0)
    {
    //
    // if not, we'll try then next dim down
    return 0;
    }
  //
  // iterate thru transform list and assign into Composite
  TransformListType::iterator it = transformList.begin();
  ++it;                         // skip the composite transform
  for(; it != transformList.end(); ++it)
    {
    ComponentTransformType *component =
      dynamic_cast<ComponentTransformType *>((*it).GetPointer());
    if(component == 0)
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

CompositeTransformIOHelper::ConstTransformListType &
CompositeTransformIOHelper
::GetTransformList(const TransformType *transform)
{
  this->m_TransformList.clear();
  // try each CompositeTransform Type, starting with
  // most common
  if(this->BuildTransformList<double,3>(transform) == 0 &&
     this->BuildTransformList<float,3>(transform) == 0 &&
     this->BuildTransformList<double,2>(transform) == 0 &&
     this->BuildTransformList<float,2>(transform) == 0 &&
     this->BuildTransformList<double,4>(transform) == 0 &&
     this->BuildTransformList<double,5>(transform) == 0 &&
     this->BuildTransformList<double,6>(transform) == 0 &&
     this->BuildTransformList<double,7>(transform) == 0 &&
     this->BuildTransformList<double,8>(transform) == 0 &&
     this->BuildTransformList<double,9>(transform) == 0 &&
     this->BuildTransformList<float,4>(transform) == 0 &&
     this->BuildTransformList<float,5>(transform) == 0 &&
     this->BuildTransformList<float,6>(transform) == 0 &&
     this->BuildTransformList<float,7>(transform) == 0 &&
     this->BuildTransformList<float,8>(transform) == 0 &&
     this->BuildTransformList<float,9>(transform) == 0)
    {
    itkGenericExceptionMacro(<< "Unsupported Composite Transform Type "
                             << transform->GetTransformTypeAsString());
    }
  return m_TransformList;
}

void
CompositeTransformIOHelper
::SetTransformList(TransformType *transform,TransformListType &transformList)
{
  // try each CompositeTransform Type, starting with
  // most common
  if(this->InternalSetTransformList<double,3>(transform,transformList) == 0 &&
     this->InternalSetTransformList<float,3>(transform,transformList) == 0 &&
     this->InternalSetTransformList<double,2>(transform,transformList) == 0 &&
     this->InternalSetTransformList<float,2>(transform,transformList) == 0 &&
     this->InternalSetTransformList<double,4>(transform,transformList) == 0 &&
     this->InternalSetTransformList<double,5>(transform,transformList) == 0 &&
     this->InternalSetTransformList<double,6>(transform,transformList) == 0 &&
     this->InternalSetTransformList<double,7>(transform,transformList) == 0 &&
     this->InternalSetTransformList<double,8>(transform,transformList) == 0 &&
     this->InternalSetTransformList<double,9>(transform,transformList) == 0 &&
     this->InternalSetTransformList<float,4>(transform,transformList) == 0 &&
     this->InternalSetTransformList<float,5>(transform,transformList) == 0 &&
     this->InternalSetTransformList<float,6>(transform,transformList) == 0 &&
     this->InternalSetTransformList<float,7>(transform,transformList) == 0 &&
     this->InternalSetTransformList<float,8>(transform,transformList) == 0 &&
     this->InternalSetTransformList<float,9>(transform,transformList) == 0)
    {
    itkGenericExceptionMacro(<< "Unsupported Composite Transform Type "
                             << transform->GetTransformTypeAsString());
    }
}

}
