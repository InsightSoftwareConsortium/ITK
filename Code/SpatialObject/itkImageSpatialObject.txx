/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageSpatialObject.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __ImageSpatialObject_txx
#define __ImageSpatialObject_txx


#include "itkImageSpatialObject.h"
#include "itkSize.h"

namespace itk
{
  template< unsigned int NDimensions, class TransformType, class PixelType >
  ImageSpatialObject< NDimensions, TransformType, PixelType >
  ::ImageSpatialObject()
  {
    m_Image = ImageType::New();
    ComputeBounds();
  }

  template< unsigned int NDimensions, class TransformType, class PixelType >
  ImageSpatialObject< NDimensions, TransformType, PixelType >
  ::~ImageSpatialObject()
  {
    m_Image->Delete();
  }

  template< unsigned int NDimensions, class TransformType, class PixelType >
  bool
  ImageSpatialObject< NDimensions, TransformType, PixelType >
  ::IsEvaluableAt( const PointType & point )
  {
    if( !IsInside( point ) )
      {
      return false;
      }
    return true; 
  }

  template< unsigned int NDimensions, class TransformType, class PixelType >
  bool
  ImageSpatialObject< NDimensions, TransformType, PixelType >
  ::IsInside( const PointType & point )
  {
    PointType p = TransformPointToLocalCoordinate( point );
    return m_Bounds->IsInside( p );
  }

  template< unsigned int NDimensions, class TransformType, class PixelType >
  void 
  ImageSpatialObject< NDimensions, TransformType, PixelType >
  ::ValueAt( const PointType & point, PixelType & value )
  {
    IndexType index;

    if( !IsEvaluableAt( point ) )
      {
      ExceptionObject e;
      e.SetLocation("ImageSpatialObject< NDimensions, TransformType, PixelType >::ValueAt( const PointType & )");
      e.SetDescription("the image value cannot be evaluated at the requested point");
      throw e;
      }

    PointType p = TransformPointToLocalCoordinate(point);

    if( m_Image->TransformPhysicalPointToIndex( p, index ) )
      {
      value = m_Image->GetPixel(index);
      }
  }

  template< unsigned int NDimensions, class TransformType, class PixelType >
  void
  ImageSpatialObject< NDimensions, TransformType, PixelType >
  ::ComputeBounds( void )
  {
    if( this->GetMTime() > m_BoundsMTime )
      { 
      ImageType::RegionType region = m_Image->GetLargestPossibleRegion();
      itk::Size<NDimensions> size = region.GetSize();
      VectorContainer< unsigned long int, Point< ScalarType, NDimensions> >::Pointer points;
      PointType pointLow,pointHigh;

      if( !(points = m_Bounds->GetPoints()) )
        {
        points = VectorContainerType::New();
        m_Bounds->SetPoints(points);
        }

      points->Initialize(); 

      for( unsigned int i=0; i<NDimensions; i++ )
        {
        pointLow[i] = 0;
        pointHigh[i] = size[i];
        }
   
      points->InsertElement(0,pointLow);
      points->InsertElement(1,pointHigh);
      m_Bounds->ComputeBoundingBox();
      m_BoundsMTime.Modified();
      }
  }

  template< unsigned int NDimensions, class TransformType, class PixelType >
  void
  ImageSpatialObject< NDimensions, TransformType, PixelType >
  ::SetImage( ImageSpatialObject< NDimensions, TransformType, PixelType >::ImagePointer image )
  {
    m_Image = image;
    m_Image->Modified();
    ComputeBounds();
  }

  template< unsigned int NDimensions, class TransformType, class PixelType >
  ImageSpatialObject< NDimensions, TransformType, PixelType >::ImagePointer
  ImageSpatialObject< NDimensions, TransformType, PixelType >
  ::GetImage( void )
  {
    return m_Image;
  }

  template< unsigned int NDimensions, class TransformType, class PixelType >
  void
  ImageSpatialObject< NDimensions, TransformType, PixelType >
  ::PrintSelf( std::ostream& os, Indent indent ) const
  {
    Superclass::PrintSelf(os,indent);
    os << "Image: " << std::endl;
    os << indent << m_Image << std::endl;
  }

  template< unsigned int NDimensions, class TransformType, class PixelType >
  unsigned long 
  ImageSpatialObject< NDimensions, TransformType, PixelType >
  ::GetMTime( void ) const
  {
    unsigned long latestMTime = Object::GetMTime();
    unsigned long boundsMTime,imageMTime;
    
    if( (boundsMTime = m_Bounds->GetMTime()) > latestMTime )
      { 
      latestMTime = boundsMTime;
      }

    if( (imageMTime = m_Image->GetMTime()) > latestMTime )
      {
      latestMTime = imageMTime;
      }

    return latestMTime; 
  }
}

#endif //__ImageSpatialObject_txx
