/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkContourSpatialObject.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#ifndef __itkContourSpatialObject_txx
#define __itkContourSpatialObject_txx

#include "itkContourSpatialObject.h" 

#include <itkNumericTraits.h>

namespace itk  
{

/** Constructor */
template< unsigned int TDimension >
ContourSpatialObject< TDimension > 
::ContourSpatialObject()  
{ 
  this->SetDimension(TDimension);
  this->SetTypeName("ContourSpatialObject");
  this->GetProperty()->SetRed(1); 
  this->GetProperty()->SetGreen(0); 
  this->GetProperty()->SetBlue(0); 
  this->GetProperty()->SetAlpha(1); 
  m_InterpolationType = NO_INTERPOLATION;
  m_Closed = false;
} 

/** Destructor */ 
template< unsigned int TDimension >
ContourSpatialObject< TDimension >  
::~ContourSpatialObject()
{ 
} 
 
/** Get the list of control points */
template< unsigned int TDimension >
typename ContourSpatialObject< TDimension > ::ControlPointListType &  
ContourSpatialObject< TDimension > 
::GetControlPoints() 
{ 
  itkDebugMacro( "Getting control Point list" );
  return m_ControlPoints;
} 

/** Get the list of control points*/
template< unsigned int TDimension >
const typename ContourSpatialObject< TDimension > ::ControlPointListType &  
ContourSpatialObject< TDimension > 
::GetControlPoints() const
{ 
  itkDebugMacro( "Getting ContourPoint list" );
  return m_ControlPoints;
} 

/** Set the control points which are defining the contour */
template< unsigned int TDimension >
void  
ContourSpatialObject< TDimension >  
::SetControlPoints( ControlPointListType & points )  
{
  m_ControlPoints.clear();
        
  typename PointListType::iterator it,end;
  it = points.begin();    
  end = points.end();
  while(it != end)
    {
    m_ControlPoints.push_back(*it);
    it++;
    }
  this->Modified();
} 

/** Get the list of interpolated points */
template< unsigned int TDimension >
typename ContourSpatialObject< TDimension > ::InterpolatedPointListType &  
ContourSpatialObject< TDimension > 
::GetInterpolatedPoints() 
{ 
  itkDebugMacro( "Getting interpolated Point list" );
  return m_InterpolatedPoints;
} 

/** Get the list of interpolated points*/
template< unsigned int TDimension >
const typename ContourSpatialObject< TDimension > ::InterpolatedPointListType &  
ContourSpatialObject< TDimension > 
::GetInterpolatedPoints() const
{ 
  itkDebugMacro( "Getting interpolated list" );
  return m_InterpolatedPoints;
} 

/** Set the interpolated points which are defining the contour */
template< unsigned int TDimension >
void  
ContourSpatialObject< TDimension >  
::SetInterpolatedPoints( InterpolatedPointListType & points )  
{
  m_InterpolatedPoints.clear();
        
  typename PointListType::iterator it,end;
  it = points.begin();    
  end = points.end();
  while(it != end)
    {
    m_InterpolatedPoints.push_back(*it);
    it++;
    }
  this->Modified();
}

/** Print the contour spatial object */
template< unsigned int TDimension >
void  
ContourSpatialObject< TDimension >  
::PrintSelf( std::ostream& os, Indent indent ) const 
{ 
  os << indent << "ContourSpatialObject(" << this << ")" << std::endl; 
  os << indent << "ID: " << this->GetId() << std::endl; 
  os << indent << "# Control Points: " 
     << static_cast< unsigned long>( m_ControlPoints.size() ) << std::endl;
  os << indent << "Interpolation type: " << m_InterpolationType << std::endl;
  os << indent << "Contour closed: " << m_Closed << std::endl;
  Superclass::PrintSelf( os, indent ); 
} 
  
/** Compute the bounds of the blob */ 
template< unsigned int TDimension >
bool 
ContourSpatialObject< TDimension >  
::ComputeLocalBoundingBox() const
{ 
  itkDebugMacro( "Computing blob bounding box" );
 
  if( this->GetBoundingBoxChildrenName().empty() 
     || strstr(typeid(Self).name(), 
     this->GetBoundingBoxChildrenName().c_str()) )
    {
    typename ControlPointListType::const_iterator it  = m_ControlPoints.begin();
    typename ControlPointListType::const_iterator end = m_ControlPoints.end();
  
    if(it == end)
      {
      return false;
      }
    else
      {     
      PointType pt = this->GetIndexToWorldTransform()->TransformPoint((*it).GetPosition());
      const_cast<BoundingBoxType *>(this->GetBounds())->SetMinimum(pt);
      const_cast<BoundingBoxType *>(this->GetBounds())->SetMaximum(pt);
      it++;

      while(it!= end)
        {  
        pt = this->GetIndexToWorldTransform()->TransformPoint((*it).GetPosition());
        const_cast<BoundingBoxType *>(this->GetBounds())->ConsiderPoint(pt);
        it++;
        }

      // Add the interpolated points (if any)
      typename InterpolatedPointListType::const_iterator itI  = m_InterpolatedPoints.begin();
      while(itI != m_InterpolatedPoints.end()) 
        {  
        pt = this->GetIndexToWorldTransform()->TransformPoint((*itI).GetPosition());
        const_cast<BoundingBoxType *>(this->GetBounds())->ConsiderPoint(pt);
        itI++;
        }
      }
    }

  return true;
} 


/** Test whether a point is inside or outside the object 
 *  For computational speed purposes, it is faster if the method does not
 *  check the name of the class and the current depth */ 
template< unsigned int TDimension >
bool 
ContourSpatialObject< TDimension >
::IsInside( const PointType & point) const
{
  /*typename ControlPointListType::const_iterator it = m_ControlPoints.begin();
  typename ControlPointListType::const_iterator itEnd = m_ControlPoints.end();
    
  if(!this->GetIndexToWorldTransform()->GetInverse(const_cast<TransformType *>(this->GetInternalInverseTransform())))
    {
    return false;
    }

  PointType transformedPoint = this->GetInternalInverseTransform()->TransformPoint(point);

  if( this->GetBounds()->IsInside(transformedPoint) )
    {
    while(it != itEnd)
      {
      if((*it).GetPosition() == transformedPoint)
        {
        return true;
        }
      it++;
      }
    }*/
  return false;
}

/** Test if the given point is inside the blob
 *  Note: ComputeBoundingBox should be called before. */
template< unsigned int TDimension >
bool 
ContourSpatialObject< TDimension >  
::IsInside( const PointType & point, unsigned int depth, char * name ) const
{
  itkDebugMacro( "Checking the point [" << point << "] is inside the blob" );
 
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

/** Return true if the blob is evaluable at a given point 
 *  i.e if the point is defined in the points list        */
template< unsigned int TDimension >
bool
ContourSpatialObject< TDimension > 
::IsEvaluableAt( const PointType & point, unsigned int depth, char * name ) const
{
  itkDebugMacro( "Checking if the blob is evaluable at " << point );
  return IsInside(point, depth, name);
}


/** Return 1 if the point is in the points list */
template< unsigned int TDimension >
bool
ContourSpatialObject< TDimension > 
::ValueAt( const PointType & point, double & value, unsigned int depth,
           char * name ) const
{
  itkDebugMacro( "Getting the value of the blob at " << point );
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
}

} // end namespace itk 

#endif
