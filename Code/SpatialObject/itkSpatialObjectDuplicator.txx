/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkSpatialObjectDuplicator.txx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSpatialObjectDuplicator_txx
#define __itkSpatialObjectDuplicator_txx

#include "itkSpatialObjectDuplicator.h"
#include "itkSpatialObjectFactoryBase.h"

namespace itk
{ 
    
/** Constructor */
template<class TInputSpatialObject>
SpatialObjectDuplicator<TInputSpatialObject>
::SpatialObjectDuplicator()
{
  m_Input = NULL;
  m_Output = NULL;
  m_InternalSpatialObjectTime = 0;
  SpatialObjectFactoryBase::RegisterDefaultSpatialObjects();
}

/** Recursive function to copy the objects */
template<class TInputSpatialObject>
void
SpatialObjectDuplicator<TInputSpatialObject>
::CopyObject(const InternalSpatialObjectType* source,
             InternalSpatialObjectType* destination)
{
  // Create the new Spatial Object using the SpatialObjectFactory
  LightObject::Pointer i;
  std::string value = source->GetSpatialObjectTypeAsString();
  i = ObjectFactoryBase::CreateInstance ( value.c_str() );

  typedef itk::SpatialObject<TInputSpatialObject::ObjectDimension> SOType;

  SOType* newSO = dynamic_cast<SOType*> ( i.GetPointer() );
  if ( newSO == NULL )
    {
    std::cout << "Could not create an instance of " << value << std::endl
        << "The usual cause of this error is not registering the "
        << "SpatialObject with SpatialFactory" << std::endl;
    std::cout << "Currently registered Transforms: " << std::endl;
    std::list<std::string> names = 
         SpatialObjectFactoryBase::GetFactory()->GetClassOverrideWithNames();
    std::list<std::string>::iterator it;
    for ( it = names.begin(); it != names.end(); it++ )
      {
      std::cout << "\t\"" << *it << "\"" << std::endl;
      }
    return;
    }
  
  // We make the copy
  newSO->CopyInformation(source);
  destination->AddSpatialObject(newSO);

  typedef typename TInputSpatialObject::ChildrenListType ChildrenListType;
  ChildrenListType* children = source->GetChildren(0);
  typename ChildrenListType::const_iterator it = children->begin();
  while(it != children->end())
    {
    this->CopyObject(*it,newSO);
    it++;
    }
  delete children;

}

/** Update function */
template<class TInputSpatialObject>
void
SpatialObjectDuplicator<TInputSpatialObject>
::Update(void)
{
  if(!m_Input)
    {
    itkExceptionMacro(<<"Input SpatialObject has not been connected");
    return;
    }

  // Update only if the input SpatialObject has been modified
  unsigned long t, t1, t2;
  t1 = m_Input->GetPipelineMTime();
  t2 = m_Input->GetMTime();
  t = (t1 > t2 ? t1 : t2);

  if(t == m_InternalSpatialObjectTime) 
    {
    return; // No need to update
    }

  // Cache the timestamp
  m_InternalSpatialObjectTime = t;

  //Copy the object first
  // Create the new Spatial Object using the SpatialObjectFactory
  LightObject::Pointer i;
  std::string value = m_Input->GetSpatialObjectTypeAsString();
  i = ObjectFactoryBase::CreateInstance ( value.c_str() );

  m_Output = dynamic_cast<SpatialObjectType*> ( i.GetPointer() );
  if ( m_Output.IsNull() )
    { 
    std::cout << "Could not create an instance of " << value << std::endl
        << "The usual cause of this error is not registering the "
        << "SpatialObject with SpatialFactory" << std::endl;
    std::cout << "Currently registered Transforms: " << std::endl;
    std::list<std::string> names = 
          SpatialObjectFactoryBase::GetFactory()->GetClassOverrideWithNames();
    std::list<std::string>::iterator it;
    for ( it = names.begin(); it != names.end(); it++ )
      {
      std::cout << "\t\"" << *it << "\"" << std::endl;
      }
    return;
    }

  m_Output->CopyInformation(m_Input);

  
  // Create the children
  typedef typename TInputSpatialObject::ChildrenListType ChildrenListType;
  ChildrenListType* children = m_Input->GetChildren(0);
  typename ChildrenListType::const_iterator it = children->begin();
  while(it != children->end())
    {
    this->CopyObject(*it,m_Output);
    it++;
    }
  delete children;
}

template<class TInputSpatialObject>
void
SpatialObjectDuplicator<TInputSpatialObject>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "Input SpatialObject: " << m_Input << std::endl;
  os << indent << "Output SpatialObject: " << m_Output << std::endl;
  os << indent << "Internal SpatialObject Time: " 
     << m_InternalSpatialObjectTime << std::endl;
}

} // end namespace itk

#endif
