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
template< unsigned int TDimension >
TubeSpatialObject< TDimension > 
::TubeSpatialObject()  
{ 
  m_ParentPoint = -1;
  m_Dimension = TDimension;
  m_TypeName = "TubeSpatialObject";
  m_Property->SetRed(1); 
  m_Property->SetGreen(0); 
  m_Property->SetBlue(0); 
  m_Property->SetAlpha(1); 
} 
 
/** Destructor */
template< unsigned int TDimension >
TubeSpatialObject< TDimension >  
::~TubeSpatialObject()
{ 
} 
 

/** Get the list of points composing the tube */
template< unsigned int TDimension >
typename TubeSpatialObject< TDimension >::PointListType &  
TubeSpatialObject< TDimension > 
::GetPoints() 
{ 
  itkDebugMacro( "Getting TubePoint list" );
  return m_Points;
} 


/** Get the list of points composing the tube */
template< unsigned int TDimension >
const typename TubeSpatialObject< TDimension >::PointListType &  
TubeSpatialObject< TDimension > 
::GetPoints() const
{ 
  itkDebugMacro( "Getting TubePoint list" );
  return m_Points;
} 

/** Set the list of points composing the tube */
template< unsigned int TDimension >
void  
TubeSpatialObject< TDimension >  
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

/** Remove the list of tube points */
template< unsigned int TDimension >
void  
TubeSpatialObject< TDimension >  
::Clear(void)
{
  m_Points.clear();
}

/** Print the object */ 
template< unsigned int TDimension >
void  
TubeSpatialObject< TDimension >  
::PrintSelf( std::ostream& os, Indent indent ) const 
{ 
  os << indent << "TubeSpatialObject(" << this << ")" << std::endl; 
  os << indent << "ID: " << m_Id << std::endl; 
  os << indent << "nb of points: "<< static_cast<unsigned long>( m_Points.size() )<< std::endl;
  os << indent << "Parent Point : " << m_ParentPoint << std::endl; 
  Superclass::PrintSelf( os, indent ); 
} 
 
/** Compute the bounds of the tube */  
template< unsigned int TDimension >
bool 
TubeSpatialObject< TDimension >  
::ComputeBoundingBox() const
{ 
  itkDebugMacro( "Computing tube bounding box" );
  bool ret = false;

  if( (this->GetMTime() > m_BoundsMTime) || (m_BoundsMTime==0) )
    {
    ret = Superclass::ComputeBoundingBox();

    if( m_BoundingBoxChildrenName.empty() 
        || strstr(typeid(Self).name(), m_BoundingBoxChildrenName.c_str()) )
      {
      typename PointListType::const_iterator it  = m_Points.begin();
      typename PointListType::const_iterator end = m_Points.end();

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

/** Return true if the given point is inside the tube */
template< unsigned int TDimension >
bool 
TubeSpatialObject< TDimension >  
::IsInside( const PointType & point, unsigned int depth, char * name) const
{
  itkDebugMacro( "Checking the point [" << point << "] is inside the tube" );

  // find the closest point, and get the radius at that point...
  // if the distance is shorter than the radius, then the point is
  // inside, else it is outside :)))
  // think about using an interpolation between the closest point and
  // its next, and previous neighbor, in order to be more accurate during 
  // the selection process.

  if(name == NULL || strstr(typeid(Self).name(), name) )
    {
    double minSquareDist=999999.0;
    double tempSquareDist;
    typename PointListType::const_iterator it = m_Points.begin();
    typename PointListType::const_iterator end = m_Points.end(); 
    typename PointListType::const_iterator min;  
  
    const TransformType * giT = GetWorldToIndexTransform();
    PointType transformedPoint = giT->TransformPoint(point);
  
    if( m_Bounds->IsInside(transformedPoint) )
      {
      while(it!= end)
        {  
        tempSquareDist=transformedPoint.SquaredEuclideanDistanceTo(
          (*it).GetPosition());
        if(tempSquareDist <= minSquareDist)
          {
          minSquareDist = tempSquareDist;
          min = it; 
          }
        it++;
        }
  
      double dist = sqrt(minSquareDist);
      if( dist <= ((*min).GetRadius()) )
        {
        return true;
        }
      }
    }
  
  return Superclass::IsInside(point, depth, name);

} 

/** Compute the tangent of the centerline of the tube */ 
template< unsigned int TDimension >
bool  
TubeSpatialObject< TDimension >  
::ComputeTangentAndNormals( void ) 
{ 
  itkDebugMacro( "Computing the tangent vectors of the tube" );
 
  if( m_Points.size() == 0 )
    {
    return false; 
    }    
  
  PointType x1, x3; 
  VectorType t;
  double l; 
  t.Fill(0.0);
 
  if( m_Points.size() == 1 ) 
    { 
    ( *(m_Points.begin()) ).SetTangent(t); 
    return true; 
    } 
     
  typename PointListType::iterator it1,it2,it3; 
  it1 = m_Points.begin(); 
  it2 = m_Points.begin();
  it2++;
  it3 = m_Points.begin();
  it3++;
  it3++; 
   
  while(it3 !=m_Points.end())
    {
    x1 = (*it1).GetPosition();
    x3 = (*it3).GetPosition();
    l=0;
    for(unsigned int i=0; i<TDimension; i++)
      {
      t[i] = (x3[i] - x1[i])/2.0;
      l = l + t[i]*t[i];
      }
    
    l = sqrt(l); 
    for(unsigned int i=0; i<TDimension; i++)
      {
      t[i] /= l;
      }
 
    (*it2).SetTangent(t);
    it1++;
    it2++;
    it3++;
    }
 
  it1 = m_Points.begin();
  it2 = it1;
  it2++;
  t = (*it2).GetTangent();
  (*it1).SetTangent(t);
  it1 = m_Points.end();
  it1--;
  it2 = it1;
  it2--;
  t = (*it2).GetTangent();
  (*it1).SetTangent(t);
 

  // Compute the normal
  CovariantVectorType n1;
  CovariantVectorType n2; 
    
  it1 = m_Points.begin(); 
 
  while(it1 != m_Points.end())
    {
    t = (*it1).GetTangent(); 
 
    if (TDimension == 2)
      { 
      t = (*it1).GetTangent(); 
      n1[0] = -t[1];
      n1[1] = t[0];
      (*it1).SetNormal1(n1); 
      }
    else if (TDimension == 3)
      {
      n1[0] = -t[1];
      n1[1] = t[0];
      n1[2] = 0;
 
      n2[0] = t[2]*t[0]; 
      n2[1] = -t[2]*t[1];
      n2[2] = t[0]*t[0] - t[1]*t[1];
 
      (*it1).SetNormal1(n1);
      (*it1).SetNormal2(n2);
      }

 
    it1++;
    }
 
  it1 = m_Points.begin();
  it2 = it1;
  it2++;
  n1 = (*it2).GetNormal1();
  (*it1).SetNormal1(n1);
   
  if (TDimension == 3)
    {
    n2 = (*it2).GetNormal2();
    (*it1).SetNormal2(n2);
    }
   
  it1 = m_Points.end();
  it1--;
  it2 = it1;
  it2--;
  n1 = (*it2).GetNormal1();
  (*it1).SetNormal1(n1);
   
  if (TDimension == 3)
    {
    n2 = (*it2).GetNormal2();
    (*it1).SetNormal2(n2);  
    }


  return true; 
} 

/** Return true if the tube is evaluable at a given point */
template< unsigned int TDimension >
bool
TubeSpatialObject< TDimension > 
::IsEvaluableAt( const PointType & point, unsigned int depth, char * name ) const
{
  itkDebugMacro( "Checking if the tube is evaluable at " << point );
  return IsInside(point, depth, name);
}

/** Return the value of the tube at a specified point */
template< unsigned int TDimension >
bool
TubeSpatialObject< TDimension > 
::ValueAt( const PointType & point, double & value, unsigned int depth,
           char * name ) const
{
  itkDebugMacro( "Getting the value of the tube at " << point );
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

} // end namespace itk 

#endif // end __itkTubeSpatialObject_txx
