/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageMaskSpatialObject.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __ImageMaskSpatialObject_txx
#define __ImageMaskSpatialObject_txx


#include "itkImageMaskSpatialObject.h"

namespace itk
{

/** Constructor */
template< unsigned int TDimension>
ImageMaskSpatialObject< TDimension>
::ImageMaskSpatialObject()
{
  this->SetTypeName("ImageMaskSpatialObject");
  this->ComputeBoundingBox();
}

/** Destructor */
template< unsigned int TDimension>
ImageMaskSpatialObject< TDimension>
::~ImageMaskSpatialObject()
{
}


/** Test whether a point is inside or outside the object 
 *  For computational speed purposes, it is faster if the method does not
 *  check the name of the class and the current depth */ 
template< unsigned int TDimension >
bool 
ImageMaskSpatialObject< TDimension >
::IsInside( const PointType & point) const
{
  if(this->GetBounds()->IsInside(point))
    {
    if(!this->GetIndexToWorldTransform()->GetInverse(const_cast<TransformType *>(this->GetInternalInverseTransform())))
      {
      return false;
      }
    PointType p = this->GetInternalInverseTransform()->TransformPoint(point);

    IndexType index;
    for(unsigned int i=0; i<TDimension; i++)
      {
      index[i] = static_cast<int>( p[i] );
      }
    bool inside = ( this->GetImage()->GetPixel(index) != NumericTraits<PixelType>::Zero );
    return inside;
    }

  return false;
}


/** Return true if the given point is inside the image */
template< unsigned int TDimension>
bool
ImageMaskSpatialObject< TDimension>
::IsInside( const PointType & point, unsigned int depth, char * name ) const
{
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
  return SpatialObject<TDimension>::IsInside(point, depth, name);
}

/** Print the object */
template< unsigned int TDimension >
void
ImageMaskSpatialObject< TDimension >
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf(os,indent);
}


} // end namespace itk

#endif //__ImageMaskSpatialObject_txx

