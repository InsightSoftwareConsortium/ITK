/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTubeNetworkSpatialObject.cxx
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
  TubeNetworkSpatialObject 
  ::TubeNetworkSpatialObject() // : CompositeSpatialObject<3, AffineTransform<double,3>, bool >()  
  {} 
 
  TubeNetworkSpatialObject 
  ::~TubeNetworkSpatialObject()  
  {} 
 
 
  /* 
  * call the CalcTangent() method for each Tube contained in this  
  * composite object... 
  */ 
 
  void  
  TubeNetworkSpatialObject 
  ::CalcTangent( void )  
  { 
    ChildrenListType::iterator it = m_Children.begin(); 
    ChildrenListType::iterator end = m_Children.end();
   
    TubeSpatialObject* tubePointer;
    TubeNetworkSpatialObject* tubeNetPointer; 
 
 
    for( ; it != end; it++ ) 
      { 
      if( (tubePointer = dynamic_cast< TubeSpatialObject * >(it->GetPointer())) != 0 ) 
        {
        tubePointer->CalcTangent(); 
        }
      else if( (tubeNetPointer = dynamic_cast< TubeNetworkSpatialObject* >(it->GetPointer())) != 0 )
        {
        tubeNetPointer->CalcTangent();
        }
      else
        {
        std::cout<<"unable to cast ("<< &it <<") iterator to tube !!!"<<std::endl;
        }
      } 
  } 

} // end namespace itk
