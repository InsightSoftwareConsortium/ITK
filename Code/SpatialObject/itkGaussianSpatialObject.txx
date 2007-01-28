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
#ifndef __itkGaussianSpatialObject_txx
#define __itkGaussianSpatialObject_txx

#include <math.h>
#include "itkGaussianSpatialObject.h" 

namespace itk 
{ 

/** Constructor */
template< unsigned int TDimension >
GaussianSpatialObject< TDimension >
::GaussianSpatialObject()
{
  this->SetTypeName("GaussianSpatialObject");
  this->SetDimension(TDimension);
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
  if(!this->GetIndexToWorldTransform()->GetInverse(
          const_cast<TransformType *>(this->GetInternalInverseTransform())))
    {
    return 0;
    }

  PointType transformedPoint = 
                  this->GetInternalInverseTransform()->TransformPoint(point);  
   
  ScalarType r = 0;
  for( unsigned int i=0; i<TDimension; i++ )
    {
    r += transformedPoint[i] * transformedPoint[i];
    }
  return r;
}

/** Test whether a point is inside or outside the object 
 *  For computational speed purposes, it is faster if the method does not
 *  check the name of the class and the current depth */ 
template< unsigned int TDimension >
bool 
GaussianSpatialObject< TDimension >
::IsInside( const PointType & point) const
{
  this->ComputeLocalBoundingBox();
  if( !this->GetBounds()->IsInside(point) )
    {
    return false;
    }
    
  if(!this->GetIndexToWorldTransform()->GetInverse(
           const_cast<TransformType *>(this->GetInternalInverseTransform())))
    {
    return false;
    }

  PointType transformedPoint = 
                    this->GetInternalInverseTransform()->TransformPoint(point);
  double r = 0;
  for(unsigned int i=0;i<TDimension;i++)
    {
    if(m_Radius!=0.0)
      {
      r += (transformedPoint[i]*transformedPoint[i])/(m_Radius*m_Radius);
      }
    else if(transformedPoint[i]>0.0)  // Degenerate ellipse
      {
      r = 2; // Keeps function from returning true here 
      break;
      }
    }
  
  if(r<1)
    {
    return true;
    }
  return false;
}


/** Test if the given point is inside the boundary of the spatial
 * object */
template< unsigned int TDimension >
bool 
GaussianSpatialObject< TDimension > 
::IsInside( const PointType & point, unsigned int depth, char * name ) const 
{
  itkDebugMacro( "Checking the point [" << point 
                 << "] is inside the GaussianSpatialObject" );
    
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

/** Compute the bounds of the Gaussian (as determined by the
 *  specified radius).  */
template< unsigned int TDimension >
bool
GaussianSpatialObject< TDimension >
::ComputeLocalBoundingBox() const
{ 
  if( this->GetBoundingBoxChildrenName().empty() 
      || strstr(typeid(Self).name(), 
                this->GetBoundingBoxChildrenName().c_str()) )
    {
    // we need to set the minimum and maximum of the bounding box
    // the center is always inside the bounding box.  
    PointType center;
    center.Fill(0);
    center = this->GetIndexToWorldTransform()->TransformPoint(center);
    const_cast<BoundingBoxType *>(this->GetBounds())->SetMinimum(center);
    const_cast<BoundingBoxType *>(this->GetBounds())->SetMaximum(center);

    // First we compute the bounding box in the index space
    typename BoundingBoxType::Pointer bb = BoundingBoxType::New();

    PointType pntMin;
    PointType pntMax;
    unsigned int i;
    for(i=0; i<TDimension;i++)
      {
      pntMin[i]=-m_Radius;
      pntMax[i]=m_Radius;
      }
    
    bb->SetMinimum(pntMin);
    bb->SetMaximum(pntMax);

    bb->ComputeBoundingBox();

    typedef typename BoundingBoxType::PointsContainer PointsContainer;
    const PointsContainer * corners = bb->GetCorners();
    typename BoundingBoxType::PointsContainer::const_iterator 
                                                     it = corners->begin();
    while(it != corners->end())
      {
      PointType pnt = this->GetIndexToWorldTransform()->TransformPoint(*it);
      const_cast<BoundingBoxType *>(this->GetBounds())->ConsiderPoint(pnt);
      ++it;
      }
    }
  return true;
} 

/** Returns if the ellipse os evaluable at one point */
template< unsigned int TDimension >
bool
GaussianSpatialObject< TDimension >
::IsEvaluableAt( const PointType & point, 
                 unsigned int depth, char * name ) const
{
  itkDebugMacro( "Checking if the ellipse is evaluable at " << point );
  return IsInside(point, depth, name);
}

/** Returns the value at one point */
template< unsigned int TDimension >
bool
GaussianSpatialObject< TDimension >
::ValueAt( const PointType & point, ScalarType & value, unsigned int depth,
           char * name ) const
{
  itkDebugMacro( "Getting the value of the ellipse at " << point );
  if( IsInside(point, 0, name) )
    {
    double zsq = this->SquaredZScore(point);
    value = m_Maximum * (ScalarType)vcl_exp(-zsq / 2.0 );
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
  
  ellipse->GetIndexToObjectTransform()->SetCenter( 
    this->GetIndexToObjectTransform()->GetCenter() );
  ellipse->GetIndexToObjectTransform()->SetMatrix( 
    this->GetIndexToObjectTransform()->GetMatrix() );
  ellipse->GetIndexToObjectTransform()->SetOffset( 
    this->GetIndexToObjectTransform()->GetOffset() );
  
  ellipse->GetObjectToWorldTransform()->SetCenter(
    this->GetObjectToWorldTransform()->GetCenter() );
  ellipse->GetObjectToWorldTransform()->SetMatrix(
    this->GetObjectToWorldTransform()->GetMatrix() );
  ellipse->GetObjectToWorldTransform()->SetOffset(
    this->GetObjectToWorldTransform()->GetOffset() );
  
  ellipse->GetIndexToWorldTransform()->SetCenter(
    this->GetIndexToWorldTransform()->GetCenter() );
  ellipse->GetIndexToWorldTransform()->SetMatrix(
    this->GetIndexToWorldTransform()->GetMatrix() );
  ellipse->GetIndexToWorldTransform()->SetOffset(
    this->GetIndexToWorldTransform()->GetOffset() );
  
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
