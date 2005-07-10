/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkArrowSpatialObject.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkArrowSpatialObject_txx
#define __itkArrowSpatialObject_txx

#include "itkArrowSpatialObject.h" 
#include <itkEuler3DTransform.h>

namespace itk  
{ 

/** Constructor */
template< unsigned int TDimension >
ArrowSpatialObject< TDimension > 
::ArrowSpatialObject()  
{ 
  this->SetDimension(TDimension);
  this->SetTypeName ("ArrowSpatialObject");
  this->GetProperty()->SetRed(1); 
  this->GetProperty()->SetGreen(0); 
  this->GetProperty()->SetBlue(0); 
  this->GetProperty()->SetAlpha(1); 
  this->ComputeBoundingBox();
  
  m_Direction.Fill(0);
  m_Direction[0] = 1; // along the x direction by default
  m_Position.Fill(0);
  m_Length = 1;
} 
 
/** Destructor */
template< unsigned int TDimension >
ArrowSpatialObject< TDimension >  
::~ArrowSpatialObject()
{ 
}
 

/** Set the length of the arrow */
template< unsigned int TDimension >
void
ArrowSpatialObject< TDimension >  
::SetLength(double length)
{
  m_Length = length;
  double spacing[TDimension];
  spacing[0] = m_Length;
  
  for(unsigned int i=1;i<TDimension;i++)
    {
    spacing[i] = 1;
    }
  this->SetSpacing(spacing);
  this->Modified();  
}


/** Compute the bounding box */ 
template< unsigned int TDimension >
bool  
ArrowSpatialObject< TDimension >
::ComputeLocalBoundingBox() const
{
  itkDebugMacro( "Computing Rectangle bounding box" );
  
  if( this->GetBoundingBoxChildrenName().empty() 
    || strstr(typeid(Self).name(), this->GetBoundingBoxChildrenName().c_str()) )
    {
    PointType pnt = this->GetPosition();
    PointType pnt2;
    for(unsigned int i=0; i<TDimension;i++) 
      {   
      pnt2[i]=pnt[i]+m_Length*m_Direction[i];
      }
      
    pnt = this->GetIndexToWorldTransform()->TransformPoint(pnt);
    pnt2 = this->GetIndexToWorldTransform()->TransformPoint(pnt2);
      
    const_cast<typename Superclass::BoundingBoxType*>(this->GetBounds())->SetMinimum(pnt);
    const_cast<typename Superclass::BoundingBoxType*>(this->GetBounds())->SetMaximum(pnt2);
    }
  return true;
}

/** Check if a given point is on the arrow */
template< unsigned int TDimension >
bool 
ArrowSpatialObject< TDimension >  
::IsInside( const PointType & point, unsigned int depth, char * name ) const
{
  itkDebugMacro( "Checking the point [" << point << "] is on the Line" );
 
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


/** Test whether a point is inside or outside the object 
 *  For computational speed purposes, it is faster if the method does not
 *  check the name of the class and the current depth */ 
template< unsigned int TDimension >
bool 
ArrowSpatialObject< TDimension >
::IsInside( const PointType & point) const
{  
  

  if(!this->GetIndexToWorldTransform()->GetInverse(const_cast<TransformType *>(this->GetInternalInverseTransform())))
    {
    return false;
    }

  PointType transformedPoint = this->GetInternalInverseTransform()->TransformPoint(point);

  this->ComputeLocalBoundingBox();
  
  if( this->GetBounds()->IsInside(transformedPoint) )
    {
    // If the transformedPoint lies on the line between the two points
    PointType pnt = this->GetPosition();
    PointType pnt2;
    for(unsigned int i=0; i<TDimension;i++) 
      {   
      pnt2[i]=pnt[i]+m_Length*m_Direction[i];
      }

    VectorType v = pnt2-pnt;
    VectorType v2 = transformedPoint-pnt;

    v.Normalize();
    v2.Normalize();

    if(dot_product(v.GetVnlVector(),v2.GetVnlVector()) == 1)
      {
      return true;
      }
    }

  return false;
}



/** Update the local transform from the position and the direction */ 
template< unsigned int TDimension >
void  
ArrowSpatialObject< TDimension >  
::UpdateTransform()
{
  VectorType offset;
  for(unsigned int i=0;i<TDimension;i++)
    {
    offset[i] = m_Position[i];
    }
  this->GetObjectToParentTransform()->SetOffset(offset);

  // If the given direction is not normalized we set the length of the vector
  // as the length of the arrow
  m_Length = m_Direction.GetSquaredNorm();

  if(m_Length != 0.0)
    {
    m_Length = sqrt(m_Length);
    }
  else
  {
  this->Modified();
  return;
  }

  m_Direction.Normalize();

  if(TDimension == 3)
    {
    typedef itk::Euler3DTransform<double> EulerTransformType;
    EulerTransformType::Pointer euler = EulerTransformType::New();

    double angley;
    double anglez = 0;
    
    #ifndef PI    
     const double PI = 4.0 * atan( 1.0 );
    #endif

    if(m_Direction[0] == 0.0)
      {
      if(m_Direction[1]>0.0)
        {
        anglez=PI/2;
        }
      else if(m_Direction[1]<0.0)
        {
        anglez=-PI/2;
        }
      }
    else
      {
      if(m_Direction[0]<0.0)
        {
        anglez = PI+atan(m_Direction[1]/m_Direction[0]);
        }
      else
        {
        anglez = atan(m_Direction[1]/m_Direction[0]);
        }
      }
    angley = -asin(m_Direction[2]);
    euler->SetRotation(0,angley,anglez);
    this->GetObjectToParentTransform()->SetMatrix(euler->GetRotationMatrix());
    }

  this->Modified();
}

/** Print the object */ 
template< unsigned int TDimension >
void  
ArrowSpatialObject< TDimension >  
::PrintSelf( std::ostream& os, Indent indent ) const 
{ 
  os << indent << "ArrowSpatialObject(" << this << ")" << std::endl; 
  Superclass::PrintSelf( os, indent );
  os << indent << "Position = " << m_Position << std::endl;
  os << indent << "Direction = " << m_Direction << std::endl;
  os << indent << "Length = " << m_Length << std::endl;
} 
 
} // end namespace itk 

#endif // end __itkArrowSpatialObject_txx
