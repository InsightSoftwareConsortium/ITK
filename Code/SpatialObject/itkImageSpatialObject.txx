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
template< unsigned int NDimensions, class PixelType, 
        unsigned int SpaceDimension >
ImageSpatialObject< NDimensions,  PixelType, SpaceDimension >
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
template< unsigned int NDimensions, class PixelType, 
          unsigned int SpaceDimension >
ImageSpatialObject< NDimensions,  PixelType, SpaceDimension >
::~ImageSpatialObject()
{
  delete m_SlicePosition;
}

/** Return true if the given point is inside the image */
template< unsigned int NDimensions, class PixelType, 
          unsigned int SpaceDimension >
bool
ImageSpatialObject< NDimensions,  PixelType, SpaceDimension >
::IsEvaluableAt( const PointType & point, unsigned int depth, char * name ) const
{
  return IsInside(point, depth, name);
}

/** Return true if the given point is inside the image */
template< unsigned int NDimensions, class PixelType, 
          unsigned int SpaceDimension >
bool
ImageSpatialObject< NDimensions,  PixelType, SpaceDimension >
::IsInside( const PointType & point, unsigned int depth, char * name ) const
{
  if( name == NULL || strstr(typeid(Self).name(), name) )
    {
    PointType p = point;
    TransformPointToLocalCoordinate( p );
    if(m_Bounds->IsInside( p))
      {
      return true;
      }
    }

  return Superclass::IsInside(point, depth, name);
}

/** Return the value of the image at a specified point 
 *  The value returned is always of type double */
template< unsigned int NDimensions, class PixelType, 
          unsigned int SpaceDimension >
bool 
ImageSpatialObject< NDimensions,  PixelType, SpaceDimension >
::ValueAt( const PointType & point, double & value, unsigned int depth,
           char * name ) const
{
  if( IsEvaluableAt( point, 0, name ) )
    {
    PointType p = point;
    TransformPointToLocalCoordinate(p);
    IndexType index;
    for(unsigned int i=0; i<NDimensions; i++)
      {
      index[i] = (int)p[i];
      }
    value = m_Image->GetPixel(index);
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

/** Compute the bounds of the image */
template< unsigned int NDimensions, class PixelType, 
          unsigned int SpaceDimension >
bool
ImageSpatialObject< NDimensions,  PixelType, SpaceDimension >
::ComputeBoundingBox( unsigned int depth, char * name )
{

  if( this->GetMTime() > m_BoundsMTime )
    { 
    bool ret = Superclass::ComputeBoundingBox(depth, name);

    if(name == NULL || strstr(typeid(Self).name(), name) )
      {
      typename ImageType::RegionType region =
               m_Image->GetLargestPossibleRegion();
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

      return true;
      }
  
    m_BoundsMTime = this->GetMTime();

    return ret;
    }

  return false;
}

/** Set the image in the spatial object */
template< unsigned int NDimensions, class PixelType, 
          unsigned int SpaceDimension >
void
ImageSpatialObject< NDimensions,  PixelType, SpaceDimension >
::SetImage( ImageSpatialObject< NDimensions,  PixelType,
                                SpaceDimension >::ImageType * image )
{
  m_Image = image;
  m_Image->Modified();
  ComputeBoundingBox();
}

/** Get the image inside the spatial object */
template< unsigned int NDimensions, class PixelType, 
          unsigned int SpaceDimension >
typename ImageSpatialObject< NDimensions,  PixelType, SpaceDimension >::ImageType *
ImageSpatialObject< NDimensions,  PixelType, SpaceDimension >
::GetImage( void )
{
  return m_Image.GetPointer();
}

/** Print the object */
template< unsigned int NDimensions, class PixelType, 
          unsigned int SpaceDimension >
void
ImageSpatialObject< NDimensions,  PixelType, SpaceDimension >
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os,indent);
  os << "Image: " << std::endl;
  os << indent << m_Image << std::endl;
}

/** Get the modification time */
template< unsigned int NDimensions, class PixelType, 
          unsigned int SpaceDimension >
unsigned long 
ImageSpatialObject< NDimensions,  PixelType, SpaceDimension >
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
template< unsigned int NDimensions, class PixelType, 
          unsigned int SpaceDimension >
void
ImageSpatialObject< NDimensions,  PixelType, SpaceDimension >
::SetSlicePosition(unsigned int dimension, int position) 
{
  m_SlicePosition[dimension]=position;
  this->Modified();
}


} // end namespace itk

#endif //__ImageSpatialObject_txx
