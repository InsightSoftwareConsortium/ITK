/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkROIStrandSpatialObject.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkROIStrandSpatialObject_h
#define __itkROIStrandSpatialObject_h
#include "itkBlobSpatialObject.h"
#include "itkROIOrientation.h"

namespace itk
{
  template <unsigned int TDimension = 3> class 
    ROIStrandSpatialObject
    :public BlobSpatialObject<TDimension>
    {
      public:
      typedef ROIStrandSpatialObject<TDimension>         Self;
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
  
      /** Method returning plane equation of strand 
       * Proper return type? ABCD plane eqn?
       */
      ROIOrientation Plane();
      itkSetMacro(thickness,double);
      itkGetMacro(thickness,double);
      bool IsClosed();
      unsigned int NumberOfPoints() const;
      ROIStrandSpatialObject<TDimension>::PointType 
      ClosestPoint(ROIStrandSpatialObject<TDimension>::PointType &curPoint);
      double MeasureArea();
      double MeasureVolume();
      double MeasurePerimeter();
      bool DeletePoint(PointType &pointToDelete);
      bool AddPoint(PointType &pointToAdd);
      bool InsertPoint(PointType &point1, PointType &pointToAdd);
      bool ReplacePoint(PointType &oldpoint, PointType &newPoint);
      bool RemoveSegment(PointType &startpoint, PointType &midpoint,
                         PointType &endPoint);
      /** Test whether a point is inside or outside the object. */ 
      virtual bool IsInside( const PointType & point,
                             unsigned int depth=0,
                             char * name = NULL) const;
      private:
      ROIOrientation m_orientation;
      double m_thickness;
      ROIStrandSpatialObject() 
      { 
        m_orientation = Unknown; 
        m_thickness = 0.0;
      }
    };
}
#ifndef ITK_MANUAL_INSTANTIATION 
#include "itkROIStrandSpatialObject.txx" 
#endif 

#endif
