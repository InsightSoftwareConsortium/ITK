/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScene.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkScene.h"
#include <algorithm>

namespace itk
{
  
/** Constructor */
template <unsigned int PipelineDimension>
Scene<PipelineDimension>
::Scene()
{
}

/** Destructor */
template <unsigned int PipelineDimension>
Scene<PipelineDimension>
::~Scene()
{
}

/** Add a spatial object to the scene */
template <unsigned int PipelineDimension>
void 
Scene<PipelineDimension>
::AddSpatialObject( NDimensionalSpatialObject<> * pointer )
{
  m_Children.push_back( pointer );
}

/** Remove a spatial object from the scene */
template <unsigned int PipelineDimension>
void 
Scene<PipelineDimension>
::RemoveSpatialObject( NDimensionalSpatialObject<> * pointer )
{
  ChildrenListType::iterator it;    
  it = std::find(m_Children.begin(),m_Children.end(),pointer);

  if( it != m_Children.end() )
  {
    if( *it == pointer )
    {
      m_Children.erase( it );
    }
  }
  else
  { 
    //throw an exception object to let user know that he tried to remove an object
    // which is not in the list of the children.
  }
}


/** Return the modification time of the scene */
template <unsigned int PipelineDimension>
unsigned long
Scene<PipelineDimension>
::GetMTime( void ) const
{
  ChildrenListType::const_iterator it = m_Children.begin();
  ChildrenListType::const_iterator end = m_Children.end();
 
  unsigned long latestTime = Superclass::GetMTime();
  unsigned long localTime;
  for(; it!=end; it++ )
  {
    localTime = (*it)->GetMTime();
    if( localTime > latestTime )
    {
      latestTime = localTime;
    }
  } 
  return latestTime;
}

/** Return the children list */
template <unsigned int PipelineDimension>
Scene<PipelineDimension>::ChildrenListType &
Scene<PipelineDimension>
::GetChildren( void )
{
  return m_Children;
}

/** Set the children list */
template <unsigned int PipelineDimension>
void
Scene<PipelineDimension>
::SetChildren( ChildrenListType & children )
{ 
  m_Children = children;
}

/** Return the number of objects in the scene */
template <unsigned int PipelineDimension>
unsigned int
Scene<PipelineDimension>
::GetNumberOfChildren( void )
{
  return m_Children.size();
} 

/** Print the object */
template <unsigned int PipelineDimension>
void
Scene<PipelineDimension>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  os << indent << "Number of children: " 
     << m_Children.size() << std::endl;
  os << indent << "List of children: ";

  ChildrenListType::const_iterator it = m_Children.begin();
  ChildrenListType::const_iterator end = m_Children.end();

  for(; it != end; it++ )
  {
    os << "[" << (*it) << "] ";
  }
  os << std::endl;

  Superclass::PrintSelf(os, indent);
}

/** Return a SpatialObject in the scene 
 *  given a parent ID */
template <unsigned int PipelineDimension>
NDimensionalSpatialObject<> *
Scene<PipelineDimension>
::GetObjectById(int Id)
{
  ChildrenListType::iterator it = m_Children.begin();
    
  while( it != m_Children.end() )
  {
    if( (*it)->GetId() == Id )
    { 
      return *it;
    }
    it++;
  }
  return NULL;
}


} // end of namespace itk 
