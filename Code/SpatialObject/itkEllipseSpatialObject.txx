/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkEllipseSpatialObject.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkEllipseSpatialObject_txx
#define __itkEllipseSpatialObject_txx

#include "itkEllipseSpatialObject.h" 

namespace itk 
{ 

/** Constructor */
template< unsigned int TDimension >
EllipseSpatialObject< TDimension >
::EllipseSpatialObject()
{
  this->SetTypeName("EllipseSpatialObject");
  m_Radius.Fill(1.0);
  this->SetDimension(TDimension);
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

/** Test whether a point is inside or outside the object 
 *  For computational speed purposes, it is faster if the method does not
 *  check the name of the class and the current depth */ 
template< unsigned int TDimension >
bool 
EllipseSpatialObject< TDimension >
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
    if(m_Radius[i]!=0.0)
      {
      r += (transformedPoint[i]*transformedPoint[i])/(m_Radius[i]*m_Radius[i]);
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

/** Test if the given point is inside the ellipse */
template< unsigned int TDimension >
bool 
EllipseSpatialObject< TDimension > 
::IsInside( const PointType & point, unsigned int depth, char * name ) const 
{
  itkDebugMacro( "Checking the point [" << point << "] is inside the Ellipse" );
    
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

/** Compute the bounds of the ellipse */
template< unsigned int TDimension >
bool
EllipseSpatialObject< TDimension >
::ComputeLocalBoundingBox() const
{ 
  itkDebugMacro( "Computing ellipse bounding box" );

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
      pntMin[i]=-m_Radius[i];
      pntMax[i]=m_Radius[i];
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
EllipseSpatialObject< TDimension >
::IsEvaluableAt( const PointType & point, 
                 unsigned int depth, char * name ) const
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
EllipseSpatialObject< TDimension >
::PrintSelf( std::ostream& os, Indent indent ) const
{

  Superclass::PrintSelf(os, indent);
  os << "Radius: " << m_Radius << std::endl;

}

/** Copy the information from another spatial object */
template< unsigned int TDimension >
void  EllipseSpatialObject< TDimension >
::CopyInformation(const DataObject *data)
{
  // check if we are the same type
  const Self* source = dynamic_cast<const Self*>(data);
  if(!source)
    {
    std::cout << "CopyInformation: objects are not of the same type" 
              << std::endl;
    return;
    }

  // copy the properties
  Superclass::CopyInformation(data);

  // copy the internal info
  this->SetRadius(source->GetRadius());
}


} // end namespace itk

#endif
