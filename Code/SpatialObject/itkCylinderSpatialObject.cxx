/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCylinderSpatialObject.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkCylinderSpatialObject.h" 

namespace itk 
{ 

/** Constructor */
CylinderSpatialObject::CylinderSpatialObject()
{
  this->SetTypeName("CylinderSpatialObject");
  this->SetDimension(3);
  m_Radius = 1.0;
  m_Height = 1.0;
} 

/** Destructor */
CylinderSpatialObject ::~CylinderSpatialObject()  
{
  
}

/** Test whether a point is inside or outside the object 
 *  For computational speed purposes, it is faster if the method does not
 *  check the name of the class and the current depth */ 
bool CylinderSpatialObject
::IsInside( const PointType & point) const
{  
  if(!this->GetIndexToWorldTransform()->GetInverse(const_cast<TransformType *>(this->GetInternalInverseTransform())))
    {
    return false;
    }

  PointType transformedPoint = this->GetInternalInverseTransform()->TransformPoint(point);
       
  this->ComputeLocalBoundingBox();

  if( this->GetBounds()->IsInside(point) )
    {
    // Check if the point is on the normal plane
    PointType a,b;
    a[0] = 0;
    a[1] = -m_Height/2;
    a[2] = 0;
      
    b[0] = 0;
    b[1] = m_Height/2;
    b[2] = 0; 
   
    double A = 0;
    double B = 0;

    for(unsigned int i = 0;i<3;i++)
      {
      A += (b[i]-a[i])*(transformedPoint[i]-a[i]);
      B += (b[i]-a[i])*(b[i]-a[i]);
      }

    double lambda = A/B;

    if( (
         (lambda>-(m_Radius/(2*sqrt(B))))
          && (lambda<0))
          || ((lambda <= 1.0) && (lambda >= 0.0))       
        )
      {
      PointType p;

      for(unsigned int i = 0;i<3;i++)
        {
        p[i] = a[i]+lambda*(b[i]-a[i]);
        }

      double tempSquareDist=transformedPoint.EuclideanDistanceTo(p);

      double R =  m_Radius;

      if(tempSquareDist <= R)
        {
        return true;
        }
      }
    }
  return false;
}



/** Test if the given point is inside the Cylinder */
bool CylinderSpatialObject 
::IsInside( const PointType & point, unsigned int depth, char * name ) const 
{
  itkDebugMacro( "Checking the point [" << point << "] is inside the Cylinder" );
    
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

/** Compute the bounds of the Cylinder */
bool CylinderSpatialObject
::ComputeLocalBoundingBox() const
{ 
  itkDebugMacro( "Computing tube bounding box" );

  if( this->GetBoundingBoxChildrenName().empty() 
    || strstr(typeid(Self).name(), this->GetBoundingBoxChildrenName().c_str()) )
    {
      // First point
      PointType ptMin,ptMax;
      ptMin[0] = -m_Radius;
      ptMin[1] = -m_Height/2;
      ptMin[2] = -m_Radius;
      ptMin = this->GetIndexToWorldTransform()->TransformPoint(ptMin);     
      ptMax[0] = +m_Radius;
      ptMax[1] = -m_Height/2;
      ptMax[2] = +m_Radius;     
      ptMax = this->GetIndexToWorldTransform()->TransformPoint(ptMax);
      const_cast<BoundingBoxType *>(this->GetBounds())->SetMinimum(ptMin);
      const_cast<BoundingBoxType *>(this->GetBounds())->SetMaximum(ptMax);    
      ptMin[0] = -m_Radius;
      ptMin[1] = +m_Height/2;
      ptMin[2] = -m_Radius;
      ptMin = this->GetIndexToWorldTransform()->TransformPoint(ptMin);     
      ptMax[0] = +m_Radius;
      ptMax[1] = +m_Height/2;
      ptMax[2] = +m_Radius;           
      ptMax = this->GetIndexToWorldTransform()->TransformPoint(ptMax);
      const_cast<BoundingBoxType *>(this->GetBounds())->ConsiderPoint(ptMin);
      const_cast<BoundingBoxType *>(this->GetBounds())->ConsiderPoint(ptMax);
    }
  return true;
} 


/** Returns if the Cylinder os evaluable at one point */
bool CylinderSpatialObject
::IsEvaluableAt( const PointType & point, unsigned int depth, char * name ) const
{
  itkDebugMacro( "Checking if the Cylinder is evaluable at " << point );
  return IsInside(point, depth, name);
}

/** Returns the value at one point */
bool CylinderSpatialObject
::ValueAt( const PointType & point, double & value, unsigned int depth,
           char * name ) const
{
  itkDebugMacro( "Getting the value of the Cylinder at " << point );
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
void CylinderSpatialObject
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os, indent);
  os << "Radius: " << m_Radius << std::endl;
  os << "Height: " << m_Height << std::endl;
}

} // end namespace itk

