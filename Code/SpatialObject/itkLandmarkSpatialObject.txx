/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLandmarkSpatialObject.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
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
template< unsigned int TDimension , unsigned int SpaceDimension >
LandmarkSpatialObject< TDimension, SpaceDimension > 
::LandmarkSpatialObject()  
{ 
  m_Dimension = TDimension;
  strcpy(m_TypeName,"LandmarkSpatialObject");
  m_Property->SetRed(1); 
  m_Property->SetGreen(0); 
  m_Property->SetBlue(0); 
  m_Property->SetAlpha(1); 
} 

/** Destructor */ 
template< unsigned int TDimension , unsigned int SpaceDimension >
LandmarkSpatialObject< TDimension, SpaceDimension >  
::~LandmarkSpatialObject()
{ 
} 
 
/** Get the list of points which are defining the blob */
template< unsigned int TDimension , unsigned int SpaceDimension >
typename LandmarkSpatialObject< TDimension, SpaceDimension > ::PointListType &  
LandmarkSpatialObject< TDimension, SpaceDimension > 
::GetPoints() 
{ 
  itkDebugMacro( "Getting LandmarkPoint list" );
  return m_Points;
} 

/** Get the list of points which are defining the blob */
template< unsigned int TDimension , unsigned int SpaceDimension >
const typename LandmarkSpatialObject< TDimension, SpaceDimension > ::PointListType &  
LandmarkSpatialObject< TDimension, SpaceDimension > 
::GetPoints() const
{ 
  itkDebugMacro( "Getting LandmarkPoint list" );
  return m_Points;
} 

/** Set the points which are defining the Landmark structure */
template< unsigned int TDimension , unsigned int SpaceDimension >
void  
LandmarkSpatialObject< TDimension, SpaceDimension >  
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
template< unsigned int TDimension , unsigned int SpaceDimension >
void  
LandmarkSpatialObject< TDimension, SpaceDimension >  
::PrintSelf( std::ostream& os, Indent indent ) const 
{ 
  os << indent << "LandmarkSpatialObject(" << this << ")" << std::endl; 
  os << indent << "ID: " << m_Id << std::endl; 
  os << indent << "nb of points: "<< m_Points.size() << std::endl;
  Superclass::PrintSelf( os, indent ); 
} 
  
/** Compute the bounds of the blob */ 
template< unsigned int TDimension , unsigned int SpaceDimension >
bool 
LandmarkSpatialObject< TDimension, SpaceDimension >  
::ComputeBoundingBox( unsigned int depth, char * name )
{ 
  itkDebugMacro( "Computing blob bounding box" );
  bool ret = false;

  if( this->GetMTime() > m_BoundsMTime )
    {
    ret = Superclass::ComputeBoundingBox(depth, name);

    if( name == NULL || strstr(typeid(Self).name(), name) )
      {
      typename PointListType::iterator it  = m_Points.begin();
      typename PointListType::iterator end = m_Points.end();
  
      if(it == end)
        {
        return ret;
        }
      else
        {
        if(!ret)
          {
          m_Bounds->SetMinimum((*it).GetPosition());
          m_Bounds->SetMaximum((*it).GetPosition());
          it++;
          }
        while(it!= end) 
          {  
          m_Bounds->ConsiderPoint((*it).GetPosition());
          it++;
          }
        ret = true;
        }
      }

    m_BoundsMTime = this->GetMTime();
    }

  return ret;
} 

/** Test if the given point is inside the blob
 *  Note: ComputeBoundingBox should be called before. */
template< unsigned int TDimension , unsigned int SpaceDimension >
bool 
LandmarkSpatialObject< TDimension, SpaceDimension >  
::IsInside( const PointType & point, unsigned int depth, char * name ) const
{
  itkDebugMacro( "Checking the point [" << point << "] is inside the blob" );

  if( name == NULL || strstr(typeid(Self).name(), name) )
    {
    typename PointListType::const_iterator it = m_Points.begin();
    typename PointListType::const_iterator itEnd = m_Points.end();
    
    PointType transformedPoint = point;
    TransformPointToLocalCoordinate(transformedPoint);
  
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
template< unsigned int TDimension , unsigned int SpaceDimension >
bool
LandmarkSpatialObject< TDimension, SpaceDimension > 
::IsEvaluableAt( const PointType & point, unsigned int depth, char * name ) const
{
   itkDebugMacro( "Checking if the blob is evaluable at " << point );
   return IsInside(point, depth, name);
}


/** Return 1 if the point is in the points list */
template< unsigned int TDimension , unsigned int SpaceDimension >
bool
LandmarkSpatialObject< TDimension, SpaceDimension > 
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
/*      itk::ExceptionObject e("LandmarkSpatialObject.txx");
      e.SetLocation("LandmarkSpatialObject::ValueAt( const PointType & )");
      e.SetDescription("this object cannot provide a value at the point");
      throw e;
 */     }
    }
}

} // end namespace itk 

#endif
