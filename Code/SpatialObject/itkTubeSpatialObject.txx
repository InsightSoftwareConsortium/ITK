/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTubeSpatialObject.txx
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

#ifndef __itkTubeSpatialObject_txx
#define __itkTubeSpatialObject_txx

#include "itkTubeSpatialObject.h" 

namespace itk  
{ 

/** Constructor */
template< unsigned int TDimension , unsigned int PipelineDimension >
TubeSpatialObject< TDimension, PipelineDimension > 
::TubeSpatialObject()  
{ 
  m_Dimension = TDimension;
  strcpy(m_TypeName,"TubeSpatialObject");
  m_Property->SetRed(1); 
  m_Property->SetGreen(0); 
  m_Property->SetBlue(0); 
  m_Property->SetAlpha(1); 
  //m_Points = new PointListType();
  ComputeBounds();
} 
 
/** Destructor */
template< unsigned int TDimension , unsigned int PipelineDimension >
TubeSpatialObject< TDimension, PipelineDimension >  
::~TubeSpatialObject()
{ 
  //delete m_Points;
} 
 

/** Get the list of points composing the tube */
template< unsigned int TDimension , unsigned int PipelineDimension >
typename TubeSpatialObject< TDimension, PipelineDimension >::PointListType &  
TubeSpatialObject< TDimension, PipelineDimension > 
::GetPoints() 
{ 
  itkDebugMacro( "Getting TubePoint list" );
  return m_Points;
} 


/** Get the list of points composing the tube */
template< unsigned int TDimension , unsigned int PipelineDimension >
const typename TubeSpatialObject< TDimension, PipelineDimension >::PointListType &  
TubeSpatialObject< TDimension, PipelineDimension > 
::GetPoints() const
{ 
  itkDebugMacro( "Getting TubePoint list" );
  return m_Points;
} 

/** Set the list of points composing the tube */
template< unsigned int TDimension , unsigned int PipelineDimension >
void  
TubeSpatialObject< TDimension, PipelineDimension >  
::SetPoints( PointListType & points )  
{
  // in this function, passing a null pointer as argument will
  // just clear the list...
  m_Points.clear();
         
  typename PointListType::iterator it,end;
  it = points.begin();    
  end = points.end();
  for(; it != end; it++ )
  {
    m_Points.push_back(*it);
  }
      
  this->Modified();
}

/** Remove the list of tube points */
template< unsigned int TDimension , unsigned int PipelineDimension >
void  
TubeSpatialObject< TDimension, PipelineDimension >  
::Clear(void)
{
  m_Points.clear();
}

/** Print the object */ 
template< unsigned int TDimension , unsigned int PipelineDimension >
void  
TubeSpatialObject< TDimension, PipelineDimension >  
::PrintSelf( std::ostream& os, Indent indent ) const 
{ 
  os << indent << "TubeSpatialObject(" << this << ")" << std::endl; 
  os << indent << "ID: " << m_Id << std::endl; 
  os << indent << "nb of points: "<< m_Points.size() << std::endl;
  Superclass::PrintSelf( os, indent ); 
} 
 
/** Compute the bounds of the tube */  
template< unsigned int TDimension , unsigned int PipelineDimension >
void 
TubeSpatialObject< TDimension, PipelineDimension >  
::ComputeBounds( void ) 
{ 
  itkDebugMacro( "Computing tube bounding box" );
  if( this->GetMTime() > m_BoundsMTime )
  {
    PointType pointLow, pointHigh; 
    PointType tempPointLow, tempPointHigh;
    typename PointListType::iterator it  = m_Points.begin();
    typename PointListType::iterator end = m_Points.end();

    PointContainerPointer points = PointContainerType::New();
    points->Initialize();

    for(unsigned int i=0; it!= end; it++, i++ ) 
    {     
      points->InsertElement(i,(*it).GetPosition());
    } 

    m_Bounds->SetPoints(points);
    m_Bounds->ComputeBoundingBox();
    m_BoundsMTime.Modified();
  }
} 

/** Return true if the given point is inside the tube */
template< unsigned int TDimension , unsigned int PipelineDimension >
bool 
TubeSpatialObject< TDimension, PipelineDimension >  
::IsInside( const PointType & point ) const
{
  itkDebugMacro( "Checking the point [" << point << "is inside the tube" );

  // find the closest point, and get the radius at that point...
  // if the distance is shorter than the radius, then the point is
  // inside, else it is outside :)))
  // think about using an interpolation between the closest point and
  // its next, and previous neighbor, in order to be more accurate during 
  // the selection process.

  double minSquareDist=999999.0;
  double tempSquareDist;
  typename PointListType::const_iterator it = m_Points.begin();
  typename PointListType::const_iterator end = m_Points.end(); 
  typename PointListType::const_iterator min;  
  PointType transformedPoint = point;
  TransformPointToLocalCoordinate(transformedPoint);

  bool inside;
  if( !m_Bounds->IsInside(transformedPoint) )
  {
    inside = false;
  }
  else
  {
    for(unsigned int i=0; it!= end; it++,i++)
    {  
      if( (tempSquareDist=transformedPoint.SquaredEuclideanDistanceTo((*it).GetPosition())) < minSquareDist)
      {
        minSquareDist = tempSquareDist;
        min = it; 
      }
    }
    double dist = sqrt(minSquareDist);
    if( dist <= ((*min).GetRadius()) )
    {
      return true;
    }
    else
    {
      inside = false;
    }
  }

  if(inside)
  {
    return true;
  }
  else
  {
    return Superclass::IsInside(transformedPoint);
  }
} 

/** Compute the tangent of the centerline of the tube */ 
template< unsigned int TDimension , unsigned int PipelineDimension >
bool  
TubeSpatialObject< TDimension, PipelineDimension >  
::CalcTangent( void ) 
{ 
  itkDebugMacro( "Computing the tangent vectors of the tube" );

  if( m_Points->size() == 0 )
  {
    return false; 
  }    
  
  PointType x1, x3; 
  vnl_vector< double > t(2);
  double l; 
  t.fill(0.0);
 
  if( m_Points->size() == 1 ) 
  { 
    ( * m_Points->begin() )->SetTangent(t); 
    return true; 
  } 
     
  PointListType::iterator it1,it2,it3; 
  it1 = m_Points->begin(); 
  it2 = m_Points->begin();
  it2++;
  it3 = m_Points->begin();
  it3++;
  it3++; 
   
  while(it3 !=m_Points->end())
  {
    x1 = (*it1)->GetPosition();
    x3 = (*it3)->GetPosition();
    l=0;
    for(int i=0; i<TDimension; i++)
    {
      t[i] = (x3[i] - x1[i])/2.0;
      l = l + t[i]*t[i];
    }
    
    l = sqrt(l); 
    for(int i=0; i<TDimension; i++)
    {
      t[i] /= l;
    }

    (*it2)->SetTangent(t);
    it1++;
    it2++;
    it3++;
  }

  it1 = m_Points->begin();
  it2 = it1;
  it2++;
  t = *(*it2)->GetTangent();
  (*it1)->SetTangent(t);
  it1 = m_Points->end();
  it1--;
  it2 = it1;
  it2--;
  t = *(*it2)->GetTangent();
  (*it1)->SetTangent(t);

  return true; 
} 

/** Compute the normal of the tangent of the centerline of the tube */ 
template< unsigned int TDimension , unsigned int PipelineDimension >
bool  
TubeSpatialObject< TDimension, PipelineDimension >  
::CalcNormal( void ) 
{ 
  itkDebugMacro( "Computing the normal vectors of the tube" );

  if( m_Points->size() == 0 )
  {
    return false; 
  }

  vnl_vector< double > t(2);
  vnl_vector< double > n1(2);
  vnl_vector< double > n2(2); 
    
  PointListType::iterator it1,it2; 
  it1 = m_Points->begin(); 

  while(it1 != m_Points->end())
  {
    t = *(*it1)->GetTangent(); 

    if (TDimension == 2)
    { 
      t = *(*it1)->GetTangent(); 
      n1(0) = -t(1);
      n1(1) = t(0);
      (*it1)->SetV1(n1); 
    }

    it1++;
  }

  it1 = m_Points->begin();
  it2 = it1;
  it2++;
  n1 = *(*it2)->GetV1();
  (*it1)->SetV1(n1);
   
  if (TDimension == 3)
  {
    n2 = *(*it2)->GetV2();
    (*it1)->SetV2(n2);
  }
   
  it1 = m_Points->end();
  it1--;
  it2 = it1;
  it2--;
  n1 = *(*it2)->GetV1();
  (*it1)->SetV1(n1);
   
  if (TDimension == 3)
  {
    n2 = *(*it2)->GetV2();
    (*it1)->SetV2(n2);  
  }

  return true; 
} 

/** Return true if the tube is evaluable at a given point */
template< unsigned int TDimension , unsigned int PipelineDimension >
bool
TubeSpatialObject< TDimension, PipelineDimension > 
::IsEvaluableAt( const PointType & point )
{
  itkDebugMacro( "Checking if the tube is evaluable at " << point );
  return IsInside(point);
}

/** Return the value of the tube at a specified point */
template< unsigned int TDimension , unsigned int PipelineDimension >
void
TubeSpatialObject< TDimension, PipelineDimension > 
::ValueAt( const PointType & point, double & value )
{
  itkDebugMacro( "Getting the value of the tube at " << point );
  if( !IsEvaluableAt(point) )
  {
    value = 0;
    itk::ExceptionObject e("TubeSpatialObject.txx");
    e.SetLocation("TubeSpatialObject::ValueAt( const PointType & )");
    e.SetDescription("this object cannot provide a value at the requested point");
    throw e;
  }

  value = 1;
}

/** Get the modification time */
template< unsigned int TDimension , unsigned int PipelineDimension >
unsigned long
TubeSpatialObject< TDimension, PipelineDimension > 
::GetMTime( void ) const
{
  unsigned long latestMTime = Object::GetMTime();
  unsigned long boundsMTime;

  if( (boundsMTime = m_Bounds->GetMTime()) > latestMTime )
  {
    latestMTime = boundsMTime;
  }
  return latestMTime;
}

} // end namespace itk 

#endif // end __itkTubeSpatialObject_txx
