/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPolygonSpatialObject.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkPolygonSpatialObject_h
#define __itkPolygonSpatialObject_h
#include "itkBlobSpatialObject.h"
#include "itkPolygonGroupOrientation.h"

namespace itk
{
template <unsigned int TDimension = 3> class 
  PolygonSpatialObject
  :public BlobSpatialObject<TDimension>
  {
    public:
    typedef PolygonSpatialObject<TDimension>         Self;
    typedef BlobSpatialObject< TDimension >            Superclass;
    typedef SmartPointer < Self >                      Pointer;
    typedef SmartPointer < const Self >                ConstPointer;
    typedef typename Superclass::PointType             PointType;
    typedef typename Superclass::PointListType         PointListType;
    typedef typename Superclass::BlobPointType         BlobPointType;
    /** Method for creation through the object factory. */
    itkNewMacro( Self );

    /** Method for creation through the object factory. */
    itkTypeMacro( Self, Superclass );
  
    /** Method returning plane alignment of strand */
    PolygonGroupOrientation Plane();
    /** Method sets the thickness of the current strand */
    itkSetMacro(Thickness,double);
    /** Method gets the thickness of the current strand */
    itkGetMacro(Thickness,double);
    bool IsClosed();
    unsigned int NumberOfPoints() const;
    /** Method returns the Point closest to the given point */
    PolygonSpatialObject<TDimension>::PointType 
    ClosestPoint(PolygonSpatialObject<TDimension>::PointType &curPoint);
    /** Method returns area of polygon described by points */
    double MeasureArea();
    /** Method returns the volume of the strand */
    double MeasureVolume();
    /** Method returns the length of the perimeter */
    double MeasurePerimeter();
    /** Method deletes a point from the strand */
    bool DeletePoint(PointType &pointToDelete);
    /** Method adds a point to the end of the strand */
    bool AddPoint(PointType &pointToAdd);
    /** Method inserts point after point1 */
    bool InsertPoint(PointType &point1, PointType &pointToAdd);
    /** Method replaces a point */
    bool ReplacePoint(PointType &oldpoint, PointType &newPoint);
    /** Method removes the series of points between startpoint and endpoint */
    bool RemoveSegment(PointType &startpoint,PointType &endPoint);
    /** Test whether a point is inside or outside the object. */ 
    virtual bool IsInside( const PointType & point,
                           unsigned int depth=0,
                           char * name = NULL) const;
    private:
    PolygonGroupOrientation m_Orientation;
    double m_Thickness;
    PolygonSpatialObject() 
    { 
      m_Orientation = Unknown; 
      m_Thickness = 0.0;
    }
  };
}
#ifndef ITK_MANUAL_INSTANTIATION 
#include "itkPolygonSpatialObject.txx" 
#endif 

#endif  // __itkPolygonSpatialObject_h
