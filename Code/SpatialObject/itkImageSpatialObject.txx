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
template< unsigned int TDimension, class PixelType >
ImageSpatialObject< TDimension,  PixelType >
::ImageSpatialObject()
{
  m_TypeName = "ImageSpatialObject";
  m_Image = ImageType::New();
  m_SlicePosition = new int[TDimension];
  for(unsigned int i=0;i<TDimension;i++)
  {
    m_SlicePosition[i]=0;
  }

  ComputeBoundingBox();
}

/** Destructor */
template< unsigned int TDimension, class PixelType >
ImageSpatialObject< TDimension,  PixelType >
::~ImageSpatialObject()
{
  delete m_SlicePosition;
}

/** Return true if the given point is inside the image */
template< unsigned int TDimension, class PixelType >
bool
ImageSpatialObject< TDimension,  PixelType >
::IsEvaluableAt( const PointType & point, unsigned int depth, char * name ) const
{
  return IsInside(point, depth, name);
}

/** Return true if the given point is inside the image */
template< unsigned int TDimension, class PixelType >
bool
ImageSpatialObject< TDimension,  PixelType >
::IsInside( const PointType & point, unsigned int depth, char * name ) const
{
  if( name == NULL || strstr(typeid(Self).name(), name) )
    {
    const TransformType * giT = GetWorldToIndexTransform();
    PointType p = giT->TransformPoint(point);
    if(m_Bounds->IsInside( p))
      {
      return true;
      }
    }

  return Superclass::IsInside(point, depth, name);
}

/** Return the value of the image at a specified point 
 *  The value returned is always of type double */
template< unsigned int TDimension, class PixelType >
bool 
ImageSpatialObject< TDimension,  PixelType >
::ValueAt( const PointType & point, double & value, unsigned int depth,
           char * name ) const
{
  if( IsEvaluableAt( point, 0, name ) )
    {
    const TransformType * giT = GetWorldToIndexTransform();
    PointType p = giT->TransformPoint(point);

    IndexType index;
    for(unsigned int i=0; i<TDimension; i++)
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
template< unsigned int TDimension, class PixelType >
bool
ImageSpatialObject< TDimension,  PixelType >
::ComputeBoundingBox() const
{

  if( this->GetMTime() > m_BoundsMTime )
    { 
    bool ret = Superclass::ComputeBoundingBox();

    if( m_BoundingBoxChildrenName.empty() 
        || strstr(typeid(Self).name(), m_BoundingBoxChildrenName.c_str()) )
      {
      typename ImageType::RegionType region =
               m_Image->GetLargestPossibleRegion();
      itk::Size<TDimension> size = region.GetSize();
      PointType pointLow,pointHigh;
  
      for( unsigned int i=0; i<TDimension; i++ )
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
template< unsigned int TDimension, class PixelType >
void
ImageSpatialObject< TDimension,  PixelType >
::SetImage( ImageSpatialObject< TDimension,  PixelType >::ImageType * image )
{
  m_Image = image;
  m_Image->Modified();
  ComputeBoundingBox();
}

/** Get the image inside the spatial object */
template< unsigned int TDimension, class PixelType >
typename ImageSpatialObject< TDimension,  PixelType >::ImageType *
ImageSpatialObject< TDimension,  PixelType >
::GetImage( void )
{
  return m_Image.GetPointer();
}

/** Print the object */
template< unsigned int TDimension, class PixelType >
void
ImageSpatialObject< TDimension,  PixelType >
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os,indent);
  os << "Image: " << std::endl;
  os << indent << m_Image << std::endl;
}

/** Get the modification time */
template< unsigned int TDimension, class PixelType >
unsigned long 
ImageSpatialObject< TDimension,  PixelType >
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
template< unsigned int TDimension, class PixelType >
void
ImageSpatialObject< TDimension,  PixelType >
::SetSlicePosition(unsigned int dimension, int position) 
{
  m_SlicePosition[dimension]=position;
  this->Modified();
}


} // end namespace itk

#endif //__ImageSpatialObject_txx
