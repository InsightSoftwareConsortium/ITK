/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTubeSpatialObject.txx
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

#ifndef __itkTubeSpatialObject_txx
#define __itkTubeSpatialObject_txx

#include "itkTubeSpatialObject.h" 

namespace itk  
{ 

/** Constructor */
template< unsigned int TDimension, typename TTubePointType >
TubeSpatialObject< TDimension, TTubePointType >
::TubeSpatialObject()  
{ 
  m_ParentPoint = -1;
  this->SetDimension(TDimension);
  this->SetTypeName("TubeSpatialObject");
  this->GetProperty()->SetRed(1); 
  this->GetProperty()->SetGreen(0); 
  this->GetProperty()->SetBlue(0); 
  this->GetProperty()->SetAlpha(1);
  m_OldMTime = 0;
  m_IndexToWorldTransformMTime = 0;
  m_EndType = 0; // default end-type is flat
} 
 
/** Destructor */
template< unsigned int TDimension, typename TTubePointType >
TubeSpatialObject< TDimension, TTubePointType >  
::~TubeSpatialObject()
{ 
} 
 

/** Get the list of points composing the tube */
template< unsigned int TDimension, typename TTubePointType >
typename TubeSpatialObject< TDimension, TTubePointType >::PointListType &  
TubeSpatialObject< TDimension, TTubePointType > 
::GetPoints() 
{ 
  itkDebugMacro( "Getting TubePoint list" );
  return m_Points;
} 


/** Get the list of points composing the tube */
template< unsigned int TDimension, typename TTubePointType >
const typename TubeSpatialObject< TDimension, TTubePointType >::PointListType &  
TubeSpatialObject< TDimension, TTubePointType > 
::GetPoints() const
{ 
  itkDebugMacro( "Getting TubePoint list" );
  return m_Points;
} 

/** Set the list of points composing the tube */
template< unsigned int TDimension, typename TTubePointType >
void  
TubeSpatialObject< TDimension, TTubePointType >  
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
template< unsigned int TDimension, typename TTubePointType >
void  
TubeSpatialObject< TDimension, TTubePointType >  
::Clear(void)
{
  m_Points.clear();
}

/** Print the object */ 
template< unsigned int TDimension, typename TTubePointType >
void  
TubeSpatialObject< TDimension, TTubePointType >  
::PrintSelf( std::ostream& os, Indent indent ) const 
{ 
  os << indent << "TubeSpatialObject(" << this << ")" << std::endl; 
  os << indent << "ID: " << this->GetId() << std::endl; 
  os << indent << "nb of points: "<< static_cast<unsigned long>( m_Points.size() )<< std::endl;
  os << indent << "End Type : " << m_EndType << std::endl;
  os << indent << "Parent Point : " << m_ParentPoint << std::endl; 
  Superclass::PrintSelf( os, indent ); 
} 
 
/** Compute the bounds of the tube */  
template< unsigned int TDimension, typename TTubePointType >
bool 
TubeSpatialObject< TDimension, TTubePointType >  
::ComputeLocalBoundingBox() const
{ 
  itkDebugMacro( "Computing tube bounding box" );

  // Check if the IndexToWorldTransform or the object itself has been modified
  if( (this->GetMTime() == m_OldMTime)
     && (m_IndexToWorldTransformMTime == this->GetIndexToWorldTransform()->GetMTime())
    )
    {
    return true; // if not modified we return
    }
 
  m_OldMTime = this->GetMTime();
  m_IndexToWorldTransformMTime = this->GetIndexToWorldTransform()->GetMTime();
   
  if( this->GetBoundingBoxChildrenName().empty() 
    || strstr(typeid(Self).name(), this->GetBoundingBoxChildrenName().c_str()) )
    {
    typename PointListType::const_iterator it  = m_Points.begin();
    typename PointListType::const_iterator end = m_Points.end();

    if(it == end)
      {
      return false;
      }
    else
      {
      // First we compute the bounding box in the index space
      typename BoundingBoxType::Pointer bb = BoundingBoxType::New();
      PointType ptMin = (*it).GetPosition()-(*it).GetRadius();
      PointType ptMax = (*it).GetPosition()+(*it).GetRadius();
      bb->SetMinimum(ptMin);
      bb->SetMaximum(ptMax);

      ptMin = this->GetIndexToWorldTransform()->TransformPoint(ptMin);
      const_cast<BoundingBoxType *>(this->GetBounds())->SetMinimum(ptMin);
      ptMax = this->GetIndexToWorldTransform()->TransformPoint(ptMax);
      const_cast<BoundingBoxType *>(this->GetBounds())->SetMaximum(ptMax);

      it++;
      while(it!= end) 
       {
        PointType ptMin = (*it).GetPosition()-(*it).GetRadius();
        PointType ptMax = (*it).GetPosition()+(*it).GetRadius();
        bb->ConsiderPoint(ptMin);
        bb->ConsiderPoint(ptMax);
        it++;
        }

      typedef typename BoundingBoxType::PointsContainer PointsContainer;
      const PointsContainer * corners = bb->GetCorners();
      typename BoundingBoxType::PointsContainer::const_iterator it = corners->begin();
      while(it != corners->end())
        {
        PointType pnt = this->GetIndexToWorldTransform()->TransformPoint(*it);
        const_cast<BoundingBoxType *>(this->GetBounds())->ConsiderPoint(pnt);       
        ++it;
        }
      }
    }
  return true;
} 

/** Test whether a point is inside or outside the object 
 *  For computational speed purposes, it is faster if the method does not
 *  check the name of the class and the current depth */ 
template< unsigned int TDimension, typename TTubePointType >
bool 
TubeSpatialObject< TDimension, TTubePointType >
::IsInside( const PointType & point) const
{
  this->ComputeLocalBoundingBox();
  if( !this->GetBounds()->IsInside(point) )
    {
    return false;
    }

  double minSquareDist=999999.0;
  double tempSquareDist;
  typename PointListType::const_iterator it = m_Points.begin();
  typename PointListType::const_iterator it2 = m_Points.begin();
  typename PointListType::const_iterator end = m_Points.end(); 
  typename PointListType::const_iterator min;
  
  if(!this->GetIndexToWorldTransform()->GetInverse(const_cast<TransformType *>(this->GetInternalInverseTransform())))
    {
    return false;
    }

  PointType transformedPoint = this->GetInternalInverseTransform()->TransformPoint(point);
       
  if(m_EndType == 0) // flat end-type
    {
    it2++; // next point 
    while(it2!= end)
      {
      // Check if the point is on the normal plane
      PointType a = (*it).GetPosition();
      PointType b = (*it2).GetPosition();
      
      double A = 0;
      double B = 0;

      for(unsigned int i = 0;i<TDimension;i++)
        {
        A += (b[i]-a[i])*(transformedPoint[i]-a[i]);
        B += (b[i]-a[i])*(b[i]-a[i]);
        }

      double lambda = A/B;

      if( ((it != m_Points.begin()) && 
          (lambda>-((*it).GetRadius()/(2*sqrt(B))))
          && (lambda<0))
          || ((lambda <= 1.0) && (lambda >= 0.0))       
        )
        {
        PointType p;

        for(unsigned int i = 0;i<TDimension;i++)
          {
          p[i] = a[i]+lambda*(b[i]-a[i]);
          }
        tempSquareDist=transformedPoint.EuclideanDistanceTo(p);
        double R =  (*it).GetRadius()+lambda*((*it2).GetRadius()-(*it).GetRadius());

        if(tempSquareDist <= R)
          {
          return true;
          }
        }
        it++;
        it2++;
      }
    }
  else if(m_EndType == 1) // rounded end-type
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
  return false;
}


/** Return true if the given point is inside the tube */
template< unsigned int TDimension, typename TTubePointType >
bool 
TubeSpatialObject< TDimension, TTubePointType >  
::IsInside( const PointType & point, unsigned int depth, char * name) const
{
  itkDebugMacro( "Checking the point [" << point << "] is inside the tube" );
 
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

/** Remove duplicate points */
template< unsigned int TDimension, typename TTubePointType >
unsigned int  
TubeSpatialObject< TDimension, TTubePointType > 
::RemoveDuplicatePoints(unsigned int step)
{
  unsigned int nPoints = 0;
  typename PointListType::iterator it, previt; 
  it = m_Points.begin(); 
  
  previt = it;
  unsigned long size = m_Points.size();

  if(size<=step)
    {
    return 0;
    }

  for(unsigned int i=0;i<step;i++)
    {
    it++;
    }
  
  for(unsigned int i=0;i<size-step;i++)
    {
    if((*previt).GetPosition() == (*it).GetPosition())
      {
      it=m_Points.erase(it);
      nPoints++;
      }
    else
      {
      previt++;
      it++;
      }
    
    }
  return nPoints;
}


/** Compute the tangent of the centerline of the tube */ 
template< unsigned int TDimension, typename TTubePointType >
bool  
TubeSpatialObject< TDimension, TTubePointType >  
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
    if(l == 0)
      {
      std::cout << "TubeSpatialObject::ComputeTangentAndNormals() : "; 
      std::cout << "length between two consecutive points is 0";
      std::cout << " (use RemoveDuplicatePoints())" << std::endl;
      return false;
      }
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

      if(n1[0]+n1[1]+n1[2] == 0.0) // if the normal is null
        {
          n1[0] = 0;
          n1[1] = -t[2];
          n1[2] = t[1];
        }
      
      n2[0] = t[1]*n1[2]-t[2]*n1[1];
      n2[1] = t[2]*n1[0]-t[0]*n1[2];
      n2[2] = t[0]*n1[1]-t[1]*n1[0];
     
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
template< unsigned int TDimension, typename TTubePointType >
bool
TubeSpatialObject< TDimension, TTubePointType > 
::IsEvaluableAt( const PointType & point, unsigned int depth, char * name ) const
{
  itkDebugMacro( "Checking if the tube is evaluable at " << point );
  return IsInside(point, depth, name);
}

/** Return the value of the tube at a specified point */
template< unsigned int TDimension, typename TTubePointType >
bool
TubeSpatialObject< TDimension, TTubePointType > 
::ValueAt( const PointType & point, double & value, unsigned int depth,
           char * name ) const
{
  itkDebugMacro( "Getting the value of the tube at " << point );
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

} // end namespace itk 

#endif // end __itkTubeSpatialObject_txx
