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

/** Constructor */
template< unsigned int NDimensions, class PixelType, unsigned int PipelineDimension >
ImageSpatialObject< NDimensions,  PixelType, PipelineDimension >
::ImageSpatialObject()
{
  strcpy(m_TypeName,"ImageSpatialObject");
  m_Image = ImageType::New();
  m_SlicePosition = new int[NDimensions];
  for(unsigned int i=0;i<NDimensions;i++)
  {
    m_SlicePosition[i]=0;
  }

  ComputeBoundingBox();
}

/** Destructor */
template< unsigned int NDimensions, class PixelType, unsigned int PipelineDimension >
ImageSpatialObject< NDimensions,  PixelType, PipelineDimension >
::~ImageSpatialObject()
{
  delete m_SlicePosition;
}

/** Return true if the given point is inside the image */
template< unsigned int NDimensions, class PixelType, unsigned int PipelineDimension >
bool
ImageSpatialObject< NDimensions,  PixelType, PipelineDimension >
::IsEvaluableAt( const PointType & point, bool includeChildren )
{
  return IsInside(point, includeChildren);
}

/** Return true if the given point is inside the image */
template< unsigned int NDimensions, class PixelType, unsigned int PipelineDimension >
bool
ImageSpatialObject< NDimensions,  PixelType, PipelineDimension >
::IsInside( const PointType & point, bool includeChildren ) const
{
  PointType p = point;
  TransformPointToLocalCoordinate( p );
  if(m_Bounds->IsInside( p))
    {
    return true;
    }
  else
    {
    return Superclass::IsInside(p, includeChildren);
    }
}

/** Return the value of the image at a specified point 
 *  The value returned is always of type double */
template< unsigned int NDimensions, class PixelType, unsigned int PipelineDimension >
void 
ImageSpatialObject< NDimensions,  PixelType, PipelineDimension >
::ValueAt( const PointType & point, double & value, bool includeChildren )
{
  if( IsEvaluableAt( point, false ) )
    {
    PointType p = point;
    TransformPointToLocalCoordinate(p);
    IndexType index;
    for(int i=0; i<NDimensions; i++)
      {
      index[i] = (int)p[i];
      }
    value = m_Image->GetPixel(index);
    return;
    }
  else
    {
    if( Superclass::IsEvaluableAt(point, includeChildren) )
      {
      Superclass::ValueAt(point, value, includeChildren);
      return;
      }
    else
      {
      value = 0;
      ExceptionObject e;
      e.SetLocation("ImageSpatialObject< NDimensions,  PixelType, \
                     PipelineDimension >::ValueAt( const PointType & )");
      e.SetDescription("the image value cannot be evaluated at the point");
      throw e;
      }
    }
}

/** Compute the bounds of the image */
template< unsigned int NDimensions, class PixelType, unsigned int PipelineDimension >
bool
ImageSpatialObject< NDimensions,  PixelType, PipelineDimension >
::ComputeBoundingBox( bool includeChildren )
{

  if( this->GetMTime() > m_BoundsMTime )
    { 
    bool ret = Superclass::ComputeBoundingBox(includeChildren);

    typename ImageType::RegionType region = m_Image->GetLargestPossibleRegion();
    itk::Size<NDimensions> size = region.GetSize();
    PointType pointLow,pointHigh;

    for( unsigned int i=0; i<NDimensions; i++ )
      {
      pointLow[i] = 0;
      pointHigh[i] = size[i];
      }
   
    if(!ret)
      {
      m_Bounds->SetMinimum(pointLow);
      m_Bounds->SetMaximum(pointHigh);
      }
    else
      {
      m_Bounds->ConsiderPoint(pointLow);
      m_Bounds->ConsiderPoint(pointHigh);
      }

    m_BoundsMTime = this->GetMTime();

    return true;
    }

  return false;
}

/** Set the image in the spatial object */
template< unsigned int NDimensions, class PixelType, unsigned int PipelineDimension >
void
ImageSpatialObject< NDimensions,  PixelType, PipelineDimension >
::SetImage( ImageSpatialObject< NDimensions,  PixelType, PipelineDimension >::ImageType * image )
{
  m_Image = image;
  m_Image->Modified();
  ComputeBoundingBox();
}

/** Get the image inside the spatial object */
template< unsigned int NDimensions, class PixelType, unsigned int PipelineDimension >
typename ImageSpatialObject< NDimensions,  PixelType, PipelineDimension >::ImageType *
ImageSpatialObject< NDimensions,  PixelType, PipelineDimension >
::GetImage( void )
{
  return m_Image.GetPointer();
}

/** Print the object */
template< unsigned int NDimensions, class PixelType, unsigned int PipelineDimension >
void
ImageSpatialObject< NDimensions,  PixelType, PipelineDimension >
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os,indent);
  os << "Image: " << std::endl;
  os << indent << m_Image << std::endl;
}

/** Get the modification time */
template< unsigned int NDimensions, class PixelType, unsigned int PipelineDimension >
unsigned long 
ImageSpatialObject< NDimensions,  PixelType, PipelineDimension >
::GetMTime( void ) const
{
  unsigned long latestMTime = Superclass::GetMTime();
  unsigned long imageMTime = m_Image->GetMTime();
    
  if( imageMTime > latestMTime )
  {
    latestMTime = imageMTime;
  }

  return latestMTime; 
}


/** Set the slice position */
template< unsigned int NDimensions, class PixelType, unsigned int PipelineDimension >
void
ImageSpatialObject< NDimensions,  PixelType, PipelineDimension >
::SetSlicePosition(unsigned int dimension, int position) 
{
  m_SlicePosition[dimension]=position;
  this->Modified();
}


} // end namespace itk

#endif //__ImageSpatialObject_txx
