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
  template <unsigned int PipelineDimension>
  Scene<PipelineDimension>
  ::Scene()
  {
  }

  template <unsigned int PipelineDimension>
  Scene<PipelineDimension>
  ::~Scene()
  {
  }


  template <unsigned int PipelineDimension>
  void 
  Scene<PipelineDimension>
  ::AddSpatialObject( NDimensionalSpatialObject<> * pointer )
  {
    
    m_Children.push_back( pointer );
    /*ChildrenListType::iterator it;

    it = std::find(m_Children.begin(),m_Children.end(),pointer);

    if( it == m_Children.end() )
    {
      m_Children.push_back( pointer );
      pointer->SetParent( this );
    }
    else
    { 
      //throw an exception object to let user know that he tried to add an object
      // which is already in the list of the children.
    }*/
  }

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
        (*it)->SetParent(NULL);
        m_Children.erase( it );
      }
    }
    else
    { 
      //throw an exception object to let user know that he tried to remove an object
      // which is not in the list of the children.
    }
  }


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


  template <unsigned int PipelineDimension>
  Scene<PipelineDimension>::ChildrenListType &
  Scene<PipelineDimension>
  ::GetChildren( void )
  {
    return m_Children;
  }

  template <unsigned int PipelineDimension>
  void
  Scene<PipelineDimension>
  ::SetChildren( ChildrenListType & children )
  { 
    m_Children = children;

    /*ChildrenListType::const_iterator it = m_Children.begin();
    ChildrenListType::const_iterator end = m_Children.end();
    
    for(; it != end; it++ )
    {
      (*it)->SetParent( this );    
    }*/
  }

  template <unsigned int PipelineDimension>
  unsigned int
  Scene<PipelineDimension>
  ::GetNumberOfChildren( void )
  {
    return m_Children.size();
  } 

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


} // end of namespace itk 




