/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLandmarkSpatialObject.txx
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

#ifndef __itkLandmarkSpatialObject_txx
#define __itkLandmarkSpatialObject_txx

#include "itkLandmarkSpatialObject.h" 

#include <itkNumericTraits.h>

namespace itk  
{

/** Constructor */
template< unsigned int TDimension >
LandmarkSpatialObject< TDimension > 
::LandmarkSpatialObject()  
{ 
  m_Dimension = TDimension;
  m_TypeName = "LandmarkSpatialObject";
  m_Property->SetRed(1); 
  m_Property->SetGreen(0); 
  m_Property->SetBlue(0); 
  m_Property->SetAlpha(1); 
} 

/** Destructor */ 
template< unsigned int TDimension >
LandmarkSpatialObject< TDimension >  
::~LandmarkSpatialObject()
{ 
} 
 
/** Get the list of points which are defining the blob */
template< unsigned int TDimension >
typename LandmarkSpatialObject< TDimension > ::PointListType &  
LandmarkSpatialObject< TDimension > 
::GetPoints() 
{ 
  itkDebugMacro( "Getting LandmarkPoint list" );
  return m_Points;
} 

/** Get the list of points which are defining the blob */
template< unsigned int TDimension >
const typename LandmarkSpatialObject< TDimension > ::PointListType &  
LandmarkSpatialObject< TDimension > 
::GetPoints() const
{ 
  itkDebugMacro( "Getting LandmarkPoint list" );
  return m_Points;
} 

/** Set the points which are defining the Landmark structure */
template< unsigned int TDimension >
void  
LandmarkSpatialObject< TDimension >  
::SetPoints( PointListType & points )  
{
  // in this function, passing a null pointer as argument will
  // just clear the list...
  m_Points.clear();
        
  typename PointListType::iterator it,end;
  it = points.begin();    
  end = points.end();
  while(it != end)
    {
    m_Points.push_back(*it);
    it++;
    }
  
  this->Modified();
} 
 
/** Print the blob spatial object */
template< unsigned int TDimension >
void  
LandmarkSpatialObject< TDimension >  
::PrintSelf( std::ostream& os, Indent indent ) const 
{ 
  os << indent << "LandmarkSpatialObject(" << this << ")" << std::endl; 
  os << indent << "ID: " << m_Id << std::endl; 
  os << indent << "nb of points: "<< static_cast< unsigned long>( m_Points.size() ) << std::endl;
  Superclass::PrintSelf( os, indent ); 
} 
  
/** Compute the bounds of the blob */ 
template< unsigned int TDimension >
bool 
LandmarkSpatialObject< TDimension >  
::ComputeLocalBoundingBox() const
{ 
  itkDebugMacro( "Computing blob bounding box" );
 
  if( m_BoundingBoxChildrenName.empty() || strstr(typeid(Self).name(), m_BoundingBoxChildrenName.c_str()) )
    {
    typename PointListType::const_iterator it  = m_Points.begin();
    typename PointListType::const_iterator end = m_Points.end();
  
    if(it == end)
      {
      return false;
      }
    else
      {     
      PointType pt = this->GetIndexToWorldTransform()->TransformPoint((*it).GetPosition());
      m_Bounds->SetMinimum(pt);
      m_Bounds->SetMaximum(pt);
      it++;

      while(it!= end) 
        {  
        PointType pt = this->GetIndexToWorldTransform()->TransformPoint((*it).GetPosition());
        m_Bounds->ConsiderPoint(pt);
        it++;
        }
      }
    }

  return true;
} 

/** Test if the given point is inside the blob
 *  Note: ComputeBoundingBox should be called before. */
template< unsigned int TDimension >
bool 
LandmarkSpatialObject< TDimension >  
::IsInside( const PointType & point, unsigned int depth, char * name ) const
{
  itkDebugMacro( "Checking the point [" << point << "] is inside the blob" );

  if( name == NULL || strstr(typeid(Self).name(), name) )
    {
    typename PointListType::const_iterator it = m_Points.begin();
    typename PointListType::const_iterator itEnd = m_Points.end();
    
    TransformType::Pointer inverse = TransformType::New();
    if(!GetIndexToWorldTransform()->GetInverse(inverse))
      {
      return false;
      }

    PointType transformedPoint = inverse->TransformPoint(point);

    if( m_Bounds->IsInside(transformedPoint) )
      {
      while(it != itEnd)
        {
        if((*it).GetPosition() == transformedPoint)
          {
          return true;
          }
        it++;
        }
      }
    }

  return Superclass::IsInside(point, depth, name);
} 

/** Return true if the blob is evaluable at a given point 
 *  i.e if the point is defined in the points list        */
template< unsigned int TDimension >
bool
LandmarkSpatialObject< TDimension > 
::IsEvaluableAt( const PointType & point, unsigned int depth, char * name ) const
{
  itkDebugMacro( "Checking if the blob is evaluable at " << point );
  return IsInside(point, depth, name);
}


/** Return 1 if the point is in the points list */
template< unsigned int TDimension >
bool
LandmarkSpatialObject< TDimension > 
::ValueAt( const PointType & point, double & value, unsigned int depth,
           char * name ) const
{
  itkDebugMacro( "Getting the value of the blob at " << point );
  if( IsInside(point, 0, name) )
    {
    value = 1;
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
      value = 0;
      return false;
      }
    }
}

} // end namespace itk 

#endif
