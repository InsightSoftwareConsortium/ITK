/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkEllipseSpatialObject.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __EllipseSpatialObject_txx
#define __EllipseSpatialObject_txx

#include "itkEllipseSpatialObject.h" 

namespace itk 
{ 

/** Constructor */
template< unsigned int TDimension >
EllipseSpatialObject< TDimension >
::EllipseSpatialObject()
{
  m_TypeName = "EllipseSpatialObject";
  m_Radius.Fill(1.0);
  m_Dimension = TDimension;
} 

/** Destructor */
template< unsigned int TDimension >
EllipseSpatialObject< TDimension >
::~EllipseSpatialObject()  
{
  
}

/** Set all radii to the same radius value */
template< unsigned int TDimension >
void
EllipseSpatialObject< TDimension >
::SetRadius(double radius)
{
  for(unsigned int i=0;i<NumberOfDimension;i++)
  {
    m_Radius[i]=radius;
  }
}

/** Test if the given point is inside the ellipse */
template< unsigned int TDimension >
bool 
EllipseSpatialObject< TDimension > 
::IsInside( const PointType & point, unsigned int depth, char * name ) const 
{
  itkDebugMacro( "Checking the point [" << point << "is inside the tube" );
    
  if(name == NULL || strstr(typeid(Self).name(), name) )
    {
    const TransformType * giT = GetWorldToIndexTransform();
    PointType transformedPoint = giT->TransformPoint(point);
  
    double r = 0;
    for(unsigned int i=0;i<TDimension;i++)
      {
      r += (transformedPoint[i]*transformedPoint[i])/(m_Radius[i]*m_Radius[i]);
      }
  
    if(r<1)
      {
      return true;
      }
    }

   return Superclass::IsInside(point, depth, name);
} 

/** Compute the bounds of the ellipse */
template< unsigned int TDimension >
bool
EllipseSpatialObject< TDimension >
::ComputeBoundingBox() const
{ 
  itkDebugMacro( "Computing tube bounding box" );

  if( this->GetMTime() > m_BoundsMTime )
    { 
    bool ret = Superclass::ComputeBoundingBox();

     if( m_BoundingBoxChildrenName.empty() 
       || strstr(typeid(Self).name(), m_BoundingBoxChildrenName.c_str()) )
      {
      PointType pnt;
      PointType pnt2;
      for(unsigned int i=0; i<TDimension;i++) 
        {   
        if(m_Radius[i]>0)
          {
          pnt[i]=m_Radius[i];
          pnt2[i]=-m_Radius[i];
          }
        else
          {
          pnt[i]=-m_Radius[i];
          pnt2[i]=m_Radius[i];
          }
        } 
    
      if(!ret)
        {
        m_Bounds->SetMinimum(pnt);
        m_Bounds->SetMaximum(pnt2);
        }
      else
        {
        m_Bounds->ConsiderPoint(pnt);
        m_Bounds->ConsiderPoint(pnt2);
        }
      }

    m_BoundsMTime = this->GetMTime();
    }

  return true;
} 


/** Returns if the ellipse os evaluable at one point */
template< unsigned int TDimension >
bool
EllipseSpatialObject< TDimension >
::IsEvaluableAt( const PointType & point, unsigned int depth, char * name ) const
{
  itkDebugMacro( "Checking if the ellipse is evaluable at " << point );
  return IsInside(point, depth, name);
}

/** Returns the value at one point */
template< unsigned int TDimension >
bool
EllipseSpatialObject< TDimension >
::ValueAt( const PointType & point, double & value, unsigned int depth,
           char * name ) const
{
  itkDebugMacro( "Getting the value of the ellipse at " << point );
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
  return false;
}

/** Print Self function */
template< unsigned int TDimension >
void 
EllipseSpatialObject< TDimension >
::PrintSelf( std::ostream& os, Indent indent ) const
{

  Superclass::PrintSelf(os, indent);
  os << "Radius: " << m_Radius << std::endl;

}

} // end namespace itk

#endif
