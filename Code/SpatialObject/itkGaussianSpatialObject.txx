/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGaussianSpatialObject.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __GaussianSpatialObject_txx
#define __GaussianSpatialObject_txx

#include <math.h>

#include "itkGaussianSpatialObject.h" 

namespace itk 
{ 

/** Constructor */
template< unsigned int TDimension >
GaussianSpatialObject< TDimension >
::GaussianSpatialObject()
{
  m_TypeName = "GaussianSpatialObject";
  m_Dimension = TDimension;
  m_Radius = 1.0;
  m_Maximum = 1.0;
} 

/** Destructor */
template< unsigned int TDimension >
GaussianSpatialObject< TDimension >
::~GaussianSpatialObject()  
{
  
}

/** The z-score is the root mean square of the z-scores along
 *  each principal axis.   */
template< unsigned int TDimension >
typename GaussianSpatialObject< TDimension >::ScalarType
GaussianSpatialObject< TDimension > 
::SquaredZScore( const PointType& point ) const
{
  const TransformType * giT = GetWorldToIndexTransform();
  PointType transformedPoint = giT->TransformPoint(point);
  
  ScalarType r = 0;
  for( unsigned int i=0; i<TDimension; i++ )
    {
    r += transformedPoint[i] * transformedPoint[i];
    }
  return r;
}

/** Test if the given point is inside the boundary of the spatial
 * object */
template< unsigned int TDimension >
bool 
GaussianSpatialObject< TDimension > 
::IsInside( const PointType & point, unsigned int depth, char * name ) const 
{
  itkDebugMacro( "Checking whether the point ["
                 << point << "] is inside the Gaussian" );

  if(name == NULL || strstr(typeid(Self).name(), name) )
    {
    if ( SquaredZScore( point ) < m_Radius * m_Radius )
      {
      return true;
      }
    }

  return Superclass::IsInside(point, depth, name);
} 

/** Compute the bounds of the Gaussian (as determined by the
 *  specified radius).  */
template< unsigned int TDimension >
bool
GaussianSpatialObject< TDimension >
::ComputeBoundingBox() const
{ 
  itkDebugMacro( "Computing Gaussian bounding box" );

  if( this->GetMTime() > m_BoundsMTime )
    { 
    bool ret = Superclass::ComputeBoundingBox();

    if( m_BoundingBoxChildrenName.empty() 
        || strstr(typeid(Self).name(), m_BoundingBoxChildrenName.c_str()) )
      {
      PointType pnt;
      PointType pnt2;
      for( unsigned int i=0; i<TDimension; i++ ) 
        {   
        pnt[i] = -m_Radius;
        pnt2[i] = m_Radius;
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


/** Returns the value at one point */
template< unsigned int TDimension >
bool
GaussianSpatialObject< TDimension >
::ValueAt( const PointType & point, ScalarType & value, unsigned int depth,
           char * name ) const
{
  itkDebugMacro( "Getting the value of a Gaussian at " << point );
  if(name == NULL || strstr(typeid(Self).name(), name) )
    {
    double zsq = this->SquaredZScore(point);
    value = m_Maximum * (ScalarType)exp( -zsq / 2.0 );
    return true;
    }
  return Superclass::ValueAt( point, value, depth,  name );
}


/** Returns the sigma=m_Radius level set of the Gaussian function, as an
 * EllipseSpatialObject.  */
template< unsigned int TDimension >
typename EllipseSpatialObject< TDimension >::Pointer 
GaussianSpatialObject< TDimension >
::GetEllipsoid() const
{
  typedef itk::EllipseSpatialObject< TDimension > EllipseType;
  typename EllipseType::Pointer ellipse = EllipseType::New();

  ellipse->SetRadius( m_Radius );
  
  ellipse->GetIndexToObjectTransform()->SetParameters( 
    this->GetIndexToObjectTransform()->GetParameters() );
  
  ellipse->GetObjectToWorldTransform()->SetParameters(
    this->GetObjectToWorldTransform()->GetParameters() );
  
  ellipse->GetIndexToWorldTransform()->SetParameters(
    this->GetIndexToWorldTransform()->GetParameters() );
  
  ellipse->GetWorldToIndexTransform()->SetParameters(
    this->GetWorldToIndexTransform()->GetParameters() );
  
  return ellipse;
}

/** Print Self function */
template< unsigned int TDimension >
void 
GaussianSpatialObject< TDimension >
::PrintSelf( std::ostream& os, Indent indent ) const
{

  Superclass::PrintSelf(os, indent);
  os << "Maximum: " << m_Maximum << std::endl;
  os << "Radius: " << m_Radius << std::endl;

}

} // end namespace itk

#endif
