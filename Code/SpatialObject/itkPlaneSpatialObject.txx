/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPlaneSpatialObject.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkPlaneSpatialObject_txx
#define __itkPlaneSpatialObject_txx

#include "itkPlaneSpatialObject.h" 

namespace itk 
{ 

/** Constructor */
template< unsigned int TDimension >
PlaneSpatialObject<TDimension >
::PlaneSpatialObject()
{
  this->SetTypeName("PlaneSpatialObject");
  this->SetDimension(TDimension);
  m_LowerPoint.Fill(0);
  m_UpperPoint.Fill(0);
} 

/** Destructor */
template< unsigned int TDimension >
PlaneSpatialObject<TDimension >
::~PlaneSpatialObject()  
{
  
}


/** Test whether a point is inside or outside the object 
 *  For computational speed purposes, it is faster if the method does not
 *  check the name of the class and the current depth */ 
template< unsigned int TDimension >
bool 
PlaneSpatialObject< TDimension >
::IsInside( const PointType & point) const
{
  typename TransformType::Pointer inverse = TransformType::New();
  if(!this->GetIndexToWorldTransform()->GetInverse(inverse))
    {
    return false;
    }

  PointType transformedPoint = inverse->TransformPoint(point);
    
  bool inside = true;
  for(unsigned int i=0;i<TDimension;i++)
    {
    if((transformedPoint[i] > m_UpperPoint[i] ) 
       || (transformedPoint[i] < m_LowerPoint[i] ))
      {
      inside = false;
      break;
      }
    }

  if(inside)
    {
    return true;
    } 
  return false;
}


/** Test if the given point is inside the blob */
template< unsigned int TDimension >
bool 
PlaneSpatialObject< TDimension > 
::IsInside( const PointType & point, unsigned int depth, char * name ) const
{
  itkDebugMacro( "Checking the point [" << point << "is inside the plane" );
    
  if(name == NULL)
    {
    if(IsInside(point))
      {
      return true;
      }
    }
  else if(strstr(typeid(Self).name(), name))
    {
    if(IsInside(point))
      {
      return true;
      }
    }

  return Superclass::IsInside(point, depth, name);
} 

/** Compute the bounds of the Plane */
template< unsigned int TDimension >
bool
PlaneSpatialObject<TDimension >
::ComputeLocalBoundingBox(void) const
{ 
  itkDebugMacro( "Computing tube bounding box" );

  if( this->GetBoundingBoxChildrenName().empty() 
      || strstr(typeid(Self).name(), 
                this->GetBoundingBoxChildrenName().c_str()) )
    {
    PointType pnt;
    PointType pnt2;
    pnt.Fill(0);
    pnt2.Fill(0);  
    for(unsigned int i=0; i<TDimension;i++) 
      {   
      pnt[i]=m_LowerPoint[i];
      pnt2[i]=m_UpperPoint[i];
      }
    
    pnt = this->GetIndexToWorldTransform()->TransformPoint(pnt);
    pnt2 = this->GetIndexToWorldTransform()->TransformPoint(pnt2);
         
    const_cast<BoundingBoxType *>(this->GetBounds())->SetMinimum(pnt);
    const_cast<BoundingBoxType *>(this->GetBounds())->SetMaximum(pnt2);
    }
  return true;
} 


/** Returns if the Plane os evaluable at one point */
template< unsigned int TDimension >
bool
PlaneSpatialObject<TDimension >
::IsEvaluableAt( const PointType & point, 
                 unsigned int depth, char * name ) const
{
  itkDebugMacro( "Checking if the Plane is evaluable at " << point );
  return IsInside(point, depth, name );
}

/** Returns the value at one point */
template< unsigned int TDimension >
bool
PlaneSpatialObject<TDimension >
::ValueAt( const PointType & point, double & value, unsigned int depth, 
           char * name ) const
{
  itkDebugMacro( "Getting the value of the tube at " << point );
  if( IsInside(point, 0, name) )
    {
    value = this->GetDefaultInsideValue();
    return true;
    }
  else
    {
    if( Superclass::IsEvaluableAt(point, depth, name) )
      {
      Superclass::ValueAt(point, value, depth, name);
      return true;
      }
    else
      {
      value = this->GetDefaultOutsideValue();
      return false;
      }
    }
  return false;
}

/** Print Self function */
template< unsigned int TDimension >
void 
PlaneSpatialObject< TDimension >
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os, indent);
}

} // end namespace itk

#endif
