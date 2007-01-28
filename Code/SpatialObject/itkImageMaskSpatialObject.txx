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
#ifndef __itkImageMaskSpatialObject_txx
#define __itkImageMaskSpatialObject_txx


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
    if(!this->GetIndexToWorldTransform()->GetInverse(
           const_cast<TransformType *>(this->GetInternalInverseTransform())))
      {
      return false;
      }
    PointType p = this->GetInternalInverseTransform()->TransformPoint(point);

    IndexType index;
    for(unsigned int i=0; i<TDimension; i++)
      {
      index[i] = static_cast<int>( p[i] );
      }

    const bool insideBuffer = 
            this->GetImage()->GetBufferedRegion().IsInside( index );
      
    if( !insideBuffer )
      {
      return false;
      }

    const bool insideMask = 
      (this->GetImage()->GetPixel(index) != NumericTraits<PixelType>::Zero);

    return insideMask;
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


template< unsigned int  TDimension >
typename ImageMaskSpatialObject< TDimension >::RegionType
ImageMaskSpatialObject< TDimension >
::GetAxisAlignedBoundingBoxRegion() const
{
  // We will use a slice iterator to iterate through slices orthogonal
  // to each of the axis of the image to find the bounding box. Each
  // slice iterator iterates from the outermost slice towards the image
  // center till it finds a mask pixel. For a 3D image, there will be six
  // slice iterators, iterating from the periphery inwards till the bounds
  // along each axes are found. The slice iterators save time and avoid 
  // having to walk the whole image. Since we are using slice iterators,
  // we will implement this only for 3D images.

  PixelType outsideValue = NumericTraits< PixelType >::Zero;
  RegionType region;
  
  ImagePointer image = this->GetImage();
  
  if( ImageType::ImageDimension == 3)
    {
    IndexType index;
    typename RegionType::SizeType  size;
    
    for( unsigned int axis = 0; axis < ImageType::ImageDimension; axis++ )
      {
      // Two slice iterators along each axis...
      // Find the orthogonal planes for the slices
      unsigned int i, j;
      unsigned int direction[2];
      for (i = 0, j = 0; i < 3; ++i )
        {
        if (i != axis )
          {
          direction[j] = i;
          j++;
          }
        }
      

      // Create the forward iterator to find lower bound
      SliceIteratorType  fit(  image,  image->GetRequestedRegion() );
      fit.SetFirstDirection(  direction[1] );
      fit.SetSecondDirection( direction[0] );

      fit.GoToBegin();
      while( !fit.IsAtEnd() )
        {
        while( !fit.IsAtEndOfSlice() )
          {
          while( !fit.IsAtEndOfLine() )
            {
            if( fit.Get() != outsideValue )
              {
              index[axis] = fit.GetIndex()[axis];
              fit.GoToReverseBegin(); // skip to the end
              break;
              }
            ++fit;
            }
          fit.NextLine();
          }
        fit.NextSlice();
        }


      // Create the reverse iterator to find upper bound
      SliceIteratorType  rit(  image,  image->GetRequestedRegion() );
      rit.SetFirstDirection(  direction[1] );
      rit.SetSecondDirection( direction[0] );

      rit.GoToReverseBegin();
      while( !rit.IsAtReverseEnd() )
        {
        while( !rit.IsAtReverseEndOfSlice() )
          {
          while( !rit.IsAtReverseEndOfLine() )
            {
            if( rit.Get() != outsideValue )
              {
              size[axis] = rit.GetIndex()[axis] - index[axis] + 1;
              rit.GoToBegin(); //Skip to reverse end
              break;
              }
            --rit;
            }
          rit.PreviousLine();
          }
        rit.PreviousSlice();
        }
      }

    region.SetIndex( index );
    region.SetSize( size );
    }
  else
    {
    itkExceptionMacro( << "ImageDimension must be 3!" );
    }
  
  return region;
   
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
