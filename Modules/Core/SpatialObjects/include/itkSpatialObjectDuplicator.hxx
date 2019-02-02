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
#ifndef itkSpatialObjectDuplicator_hxx
#define itkSpatialObjectDuplicator_hxx

#include "itkSpatialObjectDuplicator.h"
#include "itkSpatialObjectFactoryBase.h"

namespace itk
{
template< typename TInputSpatialObject >
SpatialObjectDuplicator< TInputSpatialObject >
::SpatialObjectDuplicator()
{
  m_Input = nullptr;
  m_DuplicateSpatialObject = nullptr;
  m_InternalSpatialObjectTime = 0;
  SpatialObjectFactoryBase::RegisterDefaultSpatialObjects();
}


template<typename TInputSpatialObject>
void
SpatialObjectDuplicator<TInputSpatialObject>
::WarnAndPrintFactories(const std::string& spatialObjectType) const
{
  std::cout << "Could not create an instance of " << spatialObjectType << std::endl
            << "The usual cause of this error is not registering the "
            << "SpatialObject with SpatialFactory" << std::endl;
  std::cout << "Currently registered spatial objects: " << std::endl;
  std::list< std::string > names =
    SpatialObjectFactoryBase::GetFactory()->GetClassOverrideWithNames();
  std::list< std::string >::iterator it;
  for ( it = names.begin(); it != names.end(); it++ )
    {
    std::cout << "\t\"" << *it << "\"" << std::endl;
    }
}

template< typename TInputSpatialObject >
void
SpatialObjectDuplicator< TInputSpatialObject >
::CopyObject(const InternalSpatialObjectType *source,
             InternalSpatialObjectType *destination)
{
  // Create the new Spatial Object using the SpatialObjectFactory
  LightObject::Pointer i;
  std::string value = source->GetClassNameAndDimension();

  i = ObjectFactoryBase::CreateInstance( value.c_str() );

  using SOType = itk::SpatialObject< TInputSpatialObject::ObjectDimension >;

  auto * newSO = dynamic_cast< SOType * >( i.GetPointer() );
  if ( newSO == nullptr )
    {
    WarnAndPrintFactories(value);
    return;
    }


  // Correct for extra reference count from CreateInstance().
  newSO->UnRegister();

  // We make the copy
  newSO->CopyInformation(source);
  destination->AddSpatialObject(newSO);

  using ChildrenListType = typename TInputSpatialObject::ChildrenListType;
  ChildrenListType *children = source->GetChildren(0);
  typename ChildrenListType::const_iterator it = children->begin();
  while ( it != children->end() )
    {
    this->CopyObject(*it, newSO);
    it++;
    }
  delete children;
}

template< typename TInputSpatialObject >
void
SpatialObjectDuplicator< TInputSpatialObject >
::Update()
{
  if ( !m_Input )
    {
    itkExceptionMacro(<< "Input SpatialObject has not been connected");
    return;
    }

  // Update only if the input SpatialObject has been modified
  ModifiedTimeType t, t1, t2;
  t1 = m_Input->GetPipelineMTime();
  t2 = m_Input->GetMTime();
  t = ( t1 > t2 ? t1 : t2 );

  if ( t == m_InternalSpatialObjectTime )
    {
    return; // No need to update
    }

  // Cache the timestamp
  m_InternalSpatialObjectTime = t;

  //Copy the object first
  // Create the new Spatial Object using the SpatialObjectFactory
  LightObject::Pointer i;
  std::string value = m_Input->GetClassNameAndDimension();
  i = ObjectFactoryBase::CreateInstance( value.c_str() );

  m_DuplicateSpatialObject = dynamic_cast< SpatialObjectType * >( i.GetPointer() );
  if ( m_DuplicateSpatialObject.IsNull() )
    {
    WarnAndPrintFactories(value);
    return;
    }

  // Correct for extra reference count from CreateInstance().
  m_DuplicateSpatialObject->UnRegister();

  m_DuplicateSpatialObject->CopyInformation(m_Input);

  // Create the children
  using ChildrenListType = typename TInputSpatialObject::ChildrenListType;
  ChildrenListType *children = m_Input->GetChildren(0);
  typename ChildrenListType::const_iterator it = children->begin();
  while ( it != children->end() )
    {
    this->CopyObject(*it, m_DuplicateSpatialObject);
    it++;
    }
  delete children;
}

template< typename TInputSpatialObject >
void
SpatialObjectDuplicator< TInputSpatialObject >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Input SpatialObject: " << m_Input << std::endl;
  os << indent << "Output SpatialObject: " << m_DuplicateSpatialObject << std::endl;
  os << indent << "Internal SpatialObject Time: "
     << m_InternalSpatialObjectTime << std::endl;
}
} // end namespace itk

#endif
