/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTubeNetworkSpatialObject.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
 
#include "itkTubeNetworkSpatialObject.h" 

namespace itk 
{ 
  /**
   * Constructor
   */
  TubeNetworkSpatialObject 
  ::TubeNetworkSpatialObject()
  {
    m_ParentId=-1;
  } 

  /**
   * Destructor
   */
  TubeNetworkSpatialObject 
  ::~TubeNetworkSpatialObject()  
  {
  } 
 
 
  /** 
   * call the CalcTangent() method for each Tube contained in this  
   * composite object... 
   */ 
  void  
  TubeNetworkSpatialObject 
  ::CalcTangent( void )  
  { 
    ChildrenListType::iterator it = m_Children.begin(); 
    ChildrenListType::iterator end = m_Children.end();
   
    TubeSpatialObject * tubePointer;
    TubeNetworkSpatialObject * tubeNetPointer; 

    for( ; it != end; it++ ) 
      { 
      if( (tubePointer = dynamic_cast< TubeSpatialObject * >(*it)) != 0 ) 
        {
        tubePointer->CalcTangent(); 
        }
      else if( (tubeNetPointer = dynamic_cast< TubeNetworkSpatialObject* >(*it)) != 0 )
        {
        tubeNetPointer->CalcTangent();
        }
      else
        {
        std::cout<<"unable to cast ("<< &it <<") iterator to tube !!!"<<std::endl;
        }
      } 
  } 
 
  /**
   * Return a list of tubes given a certain depth.
   * maximumDepth = 0 corresponds to an infinite depth.
   * maximumDepth = 1 returns tubes children.
   * currentDepth variable doesn't have to be changed/provided.
   */
  TubeNetworkSpatialObject::TubeListType *
  TubeNetworkSpatialObject
  ::GetTubes( unsigned int maximumDepth , unsigned int currentDepth ) const
  {
    TubeListType * tubes = new TubeListType;

    ChildrenListType::const_iterator childrenListIt = m_Children.begin();
    while(childrenListIt != m_Children.end())
    {    
      if(typeid(*this) != typeid(**childrenListIt))
      {
        tubes->push_back(dynamic_cast<TubeSpatialObject*>(*childrenListIt));
      }
      else if( (currentDepth < maximumDepth-1) || (maximumDepth == 0) )
      {
        currentDepth++;
        tubes->merge(*dynamic_cast<TubeNetworkSpatialObject*>((*childrenListIt))->GetTubes(maximumDepth,currentDepth));
      }
      childrenListIt++;
    }
    
    return tubes;
  }


} // end namespace itk
