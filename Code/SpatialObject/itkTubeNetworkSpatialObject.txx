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
#ifndef __itkTubeNetworkSpatialObject_txx
#define __itkTubeNetworkSpatialObject_txx

#include "itkTubeNetworkSpatialObject.h" 

namespace itk 
{ 

/** Constructor */
template< unsigned int TDimension, unsigned int PipelineDimension >
TubeNetworkSpatialObject< TDimension , PipelineDimension > 
::TubeNetworkSpatialObject()
{
  m_Dimension = TDimension;
  strcpy(m_TypeName,"TubeNetworkSpatialObject");
  m_ParentId=-1;
} 

/** Destructor */
template< unsigned int TDimension, unsigned int PipelineDimension >
TubeNetworkSpatialObject< TDimension , PipelineDimension >  
::~TubeNetworkSpatialObject()  
{
} 
 
/** call the CalcTangent() method for each Tube contained in this  
 *  object... */ 
template< unsigned int TDimension, unsigned int PipelineDimension > 
void  
TubeNetworkSpatialObject< TDimension , PipelineDimension >  
::CalcTangent( void )  
{ 
  typename ChildrenListType::iterator it = m_Children.begin(); 
  typename ChildrenListType::iterator end = m_Children.end();
  
  TubeSpatialObject<TDimension> * tubePointer;
  TubeNetworkSpatialObject<TDimension> * tubeNetPointer; 

  for( ; it != end; it++ ) 
  { 
    if( (tubePointer = dynamic_cast< TubeSpatialObject<TDimension> * >(*it)) != 0 ) 
    {
      tubePointer->CalcTangent(); 
    }
    else if( (tubeNetPointer = dynamic_cast< TubeNetworkSpatialObject<TDimension>* >(*it)) != 0 )
    {
      tubeNetPointer->CalcTangent();
    }
    else
    {
      std::cout<<"unable to cast ("<< &it <<") iterator to tube !!!"<<std::endl;
    }
  }
} 
 
/** Return a list of tubes given a certain depth.
 *  maximumDepth = 0 corresponds to an infinite depth.
 *  maximumDepth = 1 returns tubes children.
 *  currentDepth variable doesn't have to be changed/provided. */
template< unsigned int TDimension, unsigned int PipelineDimension >
typename TubeNetworkSpatialObject< TDimension , PipelineDimension > ::TubeListType *
TubeNetworkSpatialObject< TDimension , PipelineDimension > 
::GetTubes( unsigned int maximumDepth , unsigned int currentDepth ) const
{
  TubeListType * tubes = new TubeListType;

  typename ChildrenListType::const_iterator childrenListIt = m_Children.begin();
  while(childrenListIt != m_Children.end())
  {    
    // Check if the child is really a tube or a tube network
    if( (!strncmp(typeid(**childrenListIt).name(),"class itk::TubeSpatialObject",26))
       || (!strncmp(typeid(**childrenListIt).name(),"class itk::TubeNetworkSpatialObject",28))
      )
    {
      if(typeid(*this) != typeid(**childrenListIt))
      {
        tubes->push_back(dynamic_cast<TubeSpatialObject<TDimension>*>(*childrenListIt));
      }
      else if( (currentDepth < maximumDepth-1) || (maximumDepth == 0) )
      {
        currentDepth++;
        tubes->merge(*dynamic_cast<TubeNetworkSpatialObject<TDimension>*>((*childrenListIt))->GetTubes(maximumDepth,currentDepth));
      }
    }
    childrenListIt++;
  }
    
  return tubes;
}


} // end namespace itk

#endif

