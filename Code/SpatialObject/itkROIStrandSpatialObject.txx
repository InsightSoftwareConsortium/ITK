/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkROIStrandSpatialObject.txx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) 2002 Insight Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkROIStrandSpatialObject.h"
#include "itkExceptionObject.h"

#include <limits>
namespace itk
{


  template <unsigned int TDimension >
  ROIOrientation
  ROIStrandSpatialObject<TDimension>
  ::Plane()
  {
    ROIOrientation plane;
    // local typedef to shut up the compiler...

    PointListType &points = this->GetPoints();
    typename PointListType::iterator it = points.begin();
    typename PointListType::iterator itend = points.end();
    double min[3],max[3];       // x, y, z
    int i;
    for(i = 0; i < 3; i++)
      {
        min[i] = -std::numeric_limits<double>::max();
        max[i] = std::numeric_limits<double>::max();
      }
    while (it != itend)
      {
        PointType &curpoint = dynamic_cast<PointType &>((*it));
        for(i = 0; i < 3; i++) 
          {
            if(min[i] > curpoint[i]) min[i] = curpoint[i];
            if(max[i] < curpoint[i]) max[i] = curpoint[i];
          }
        it++;
      }
    if(min[0] == max[0] && min[1] != max[1] && min[2] != max[2])
      {
        plane = Sagittal;
      }
    else if(min[0] != max[0] && min[1] == max[1] && min[2] != max[2])
      {
        plane = Axial;
      }
    else if(min[0] != max[0] && min[1] != max[1] && min[2] == max[2])
      {
        plane = Coronal;
      }
    return plane;
  }

  template <unsigned int TDimension >
  bool 
  ROIStrandSpatialObject<TDimension>
  ::IsClosed()
  {
    PointListType &points = this->GetPoints();
    typename PointListType::iterator it = points.begin();
    typename PointListType::iterator itend = points.end();
    itend--;
    return (*it).GetPosition() == (*itend).GetPosition();
    return false;
  }

  template <unsigned int TDimension >
  unsigned int
  ROIStrandSpatialObject<TDimension>
  ::NumberOfPoints() const
  {
    return (this->GetPoints()).size();
  }

  template <unsigned int TDimension >
  typename ROIStrandSpatialObject<TDimension>::PointType
  ROIStrandSpatialObject<TDimension>
  ::ClosestPoint(PointType &curPoint)
  {


    PointListType &points = this->GetPoints();
    typename PointListType::iterator it = points.begin();
    typename PointListType::iterator itend = points.end();
    double distance = std::numeric_limits<double>::max();

    if(it == itend)
      { 
        ExceptionObject exception(__FILE__, __LINE__);
        exception.SetDescription("File cannot be read");
        throw exception;
      }
    
    PointType closestPoint;
    while (it != itend)
      {
        typename SpatialObjectPoint<TDimension>::PointType curpos 
        = (*it).GetPosition();
        double curdistance = curpos.EuclideanDistanceTo(curPoint);
        if(curdistance < distance)
          {
            closestPoint = (*it).GetPosition();
          }
        it++;
      }
    return closestPoint;
  }

  template <unsigned int TDimension >
  double
  ROIStrandSpatialObject<TDimension>
  ::MeasureArea()
  {
    // int i;
    // double area = 0.0;
    // if (n < 3)
    // return 0.0;
    // for (i = 0; i < n; i++)
    // area += (double)(p[i].x * p[j].y - p[i].y * p[j].x);
    // area *= 0.5;
    // return area < 0.0 ? -area : area;
    //To find the area of a planar polygon not in the x-y plane, use:
    //2 A(P) = abs(N . (sum_{i=0}^{n-1} (v_i x v_{i+1})))
    //where N is a unit vector normal to the plane. The `.' represents the
    //dot product operator, the `x' represents the cross product operator,
    //        and abs() is the absolute value function.
    double area = 0.0;
    int numpoints = this->NumberOfPoints();
    int X, Y;
    if(numpoints < 3)
      {
        return 0;
      }
    switch(this->Plane())
      {
      case Sagittal:
        X = 1; Y = 2;
        break;
      case Axial:
        X = 0; Y = 2;
        break;
      case Coronal:
        X = 0; Y = 1;
      default:
        ExceptionObject exception(__FILE__, __LINE__);
        exception.SetDescription("File cannot be read");
        throw exception;
      }
    PointListType &points = this->GetPoints();
    typename PointListType::iterator it = points.begin();
    PointType start = (*it).GetPosition();
    for(int i = 0; i < numpoints; i++)
      {
        PointType a = (*it).GetPosition();
        PointType b;
        it++;
        if(i == numpoints - 1)
          b = start;
        else
          b = (*it).GetPosition();
        //
        // closed ROI has first and last points the same
        if(a == b)
          continue;
        area += a[X] * b[Y] - a[Y] * b[X];
      }
    area *= 0.5;
    return area < 0.0 ? -area : area;
  }

  template <unsigned int TDimension >
  double 
  ROIStrandSpatialObject<TDimension>
  ::MeasureVolume()
  {
    return m_thickness * this->MeasureArea;
  }

  template <unsigned int TDimension >
  double 
  ROIStrandSpatialObject<TDimension>
  ::MeasurePerimeter()
  {
    double perimeter = 0.0;
    int numpoints = this->NumberOfPoints();
    int X, Y;
    if(numpoints < 3)
      {
        return 0;
      }
    switch(this->Plane())
      {
      case Sagittal:
        X = 1; Y = 2;
        break;
      case Axial:
        X = 0; Y = 2;
        break;
      case Coronal:
        X = 0; Y = 1;
      default:
        ExceptionObject exception(__FILE__, __LINE__);
        exception.SetDescription("File cannot be read");
        throw exception;
      }
    PointListType &points = this->GetPoints();
    typename PointListType::iterator it = points.begin();
    PointType start = (*it).GetPosition();
    for(int i = 0; i < numpoints; i++)
      {
        PointType a = (*it).GetPosition();
        PointType b;
        it++;
        if(i == numpoints - 1)
          b = start;
        else
          b = (*it).GetPosition();
        //
        // closed ROI has first and last points the same
        if(a == b)
          continue;
        double curdistance = a.EuclideanDistanceTo(b);
        perimeter += curdistance;
      }
    return perimeter;
  }

  template <unsigned int TDimension >
  bool 
  ROIStrandSpatialObject<TDimension>
  ::DeletePoint(PointType &pointToDelete)
  {
    
    PointListType &points = this->GetPoints();
    typename PointListType::iterator it = points.begin();
    typename PointListType::iterator itend = points.end();
    if(it == itend)
      { 
        return false;
      }
    
    while (it != itend)
      {
        BlobPointType &curPoint = (*it);
        typename SpatialObjectPoint<TDimension>::PointType curpos 
        = curPoint.GetPosition();
        if(curpos == pointToDelete)
          {
            points.remove(curPoint);
            return true;
          }
        it++;
      }
    return false;
  }

  template <unsigned int TDimension >
  bool 
  ROIStrandSpatialObject<TDimension>
  ::AddPoint(PointType &pointToAdd)
  {
    BlobPointType newPoint;
    newPoint.SetPosition(pointToAdd);
    this->GetPoints.push_back(newPoint);
    return true;
  }

  template <unsigned int TDimension >
  bool 
  ROIStrandSpatialObject<TDimension>
  ::InsertPoint(PointType &point1, PointType &pointToAdd)
  {
    
    PointListType &points = this->GetPoints();
    typename PointListType::iterator it = points.begin();
    typename PointListType::iterator itend = points.end();
    if(it == itend)
      { 
        this->AddPoint(pointToAdd);
        return true;
      }
    
    while (it != itend)
      {
        BlobPointType &curPoint = (*it);
        typename SpatialObjectPoint<TDimension>::PointType curpos 
        = curPoint.GetPosition();
        if(curpos == point1)
          {
            typename PointListType::iterator after = it;
            after++;
            BlobPointType newPoint;
            newPoint.SetPosition(pointToAdd);
            points.insert(after,1,newPoint);
            return true;
          }
        it++;
      }
    return false;
  }

  template <unsigned int TDimension >
  bool 
  ROIStrandSpatialObject<TDimension>
  ::ReplacePoint(PointType &oldpoint, PointType &newPoint)
  {
    PointListType &points = this->GetPoints();
    typename PointListType::iterator it = points.begin();
    typename PointListType::iterator itend = points.end();
    if(it == itend)
      { 
        this->AddPoint(pointToAdd);
        return true;
      }
    
    while(it != itend)
      {
        BlobPointType &curPoint = (*it);
        typename SpatialObjectPoint<TDimension>::PointType curpos 
        = curPoint.GetPosition();
        if(curpos == oldpoint)
          {
            typename PointListType::iterator after = it;
            after++;
            BlobPointType newPoint;
            newPoint.SetPosition(pointToAdd);
            points.insert(after,1,newPoint);
            points.erase(it);
            return true;
          }
        it++;
      }
    return false;
  }
  template <unsigned int TDimension >
  bool 
  ROIStrandSpatialObject<TDimension>
  ::RemoveSegment(PointType &startpoint, PointType &midpoint,PointType &endPoint)
  {
    PointListType &points = this->GetPoints();
    typename PointListType::iterator it = points.begin();
    typename PointListType::iterator itend = points.end();
    typename PointListType::iterator first;
    typename PointListType::iterator last;
    if(it == itend)
      { 
        return false;
      }
    int foundcount = 0;
    while(it != itend)
      {
        BlobPointType &curPoint = (*it);
        typename SpatialObjectPoint<TDimension>::PointType curpos 
        = curPoint.GetPosition();
        if(curpos == startpoint) 
          {
            first = it;
            foundcount++;
          } 
        else
          {
            last = it;
            foundcount++;
          }
        if(foundcount == 2)
          {
            break;
          }
        it++;
      }
    if(foundcount != 2)
      return false;
    points.erase(first,last);
    
  }

// Determine if a point is inside a polygon
// Globals which should be set before calling this function:
//
// int    polySides  =  how many corners the polygon has
// float  polyX[]    =  horizontal coordinates of corners
// float  polyY[]    =  vertical coordinates of corners
// float  x, y       =  point to be tested
//
// (Globals are used in this example for purposes of speed.
// Change as desired.)
//
// The function will return TRUE if the point x,y is inside the
// polygon, or FALSE if it is not. If the point x,y is exactly on
// the edge of the polygon, then the function may return TRUE or
// FALSE.
//
// Note that division by zero is avoided because the division is
// protected by the "if" clause which surrounds it.
//
//   boolean pointInPolygon() 
//   {
//     int      i, j=0         ;
//     boolean  oddNODES=FALSE ;
//
//     for (i=0; i<polySides; i++) 
//       {
//      j++; if (j==polySides) j=0;
//      if (polyY[i]<y && polyY[j]>=y
//          ||  polyY[j]<y && polyY[i]>=y) 
//        {
//          if (polyX[i]+(y-polyY[i])/(polyY[j]-polyY[i])*
//              (polyX[j]-polyX[i])<x) 
//            {
//              oddNODES=!oddNODES; 
//            }
//        }
//       }
//
//     return oddNODES; 
//   }


  template <unsigned int TDimension >
  bool 
  ROIStrandSpatialObject<TDimension>
  ::IsInside( const PointType & point,unsigned int depth,char * name) const
  {
    int numpoints = this->NumberOfPoints();
    int X, Y;
    if(numpoints < 3)
      {
        return false;
      }
    switch(const_cast<Self *>(this)->Plane())
      {
      case Sagittal:
        X = 1; Y = 2;
        break;
      case Axial:
        X = 0; Y = 2;
        break;
      case Coronal:
        X = 0; Y = 1;
      default:
        ExceptionObject exception(__FILE__, __LINE__);
        exception.SetDescription("File cannot be read");
        throw exception;
      }

    PointListType &points = const_cast<Self *>(this)->GetPoints();
    typename PointListType::iterator it = points.begin();
    typename PointListType::iterator itend = points.end();
    PointType start = (*it).GetPosition();
    PointType last = (*itend).GetPosition();
    //
    // if last point same as first, don't bother with it.
    if(start == last)
      numpoints--;

    bool oddNodes = false;

    for(int i = 0; i < numpoints; i++)
      {
        PointType start = (*it).GetPosition();
        it++;
        PointType end;
        if(i == numpoints - 1)
          {
            end = start;
          }
        else
          {
            end = (*it).GetPosition();
          }
        double x = point[X]; double y = point[Y];
        if((start[Y] < y && end[Y] >= y) ||
           (end[Y] < y && start[Y] >= y))
          {
            if( (start[X] + (y - start[Y]))/
                ((end[Y] - start[Y]) * (end[X] - start[X])) < x )
              {
                oddNodes = !oddNodes;
              }
          }
      }
    return oddNodes;
  }
}
