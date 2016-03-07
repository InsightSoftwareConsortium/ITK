/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkImageMaskSpatialObject_hxx
#define itkImageMaskSpatialObject_hxx

#include "itkMath.h"
#include "itkImageMaskSpatialObject.h"
#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk
{
/** Constructor */
template< unsigned int TDimension >
ImageMaskSpatialObject< TDimension >
::ImageMaskSpatialObject()
{
  this->SetTypeName("ImageMaskSpatialObject");
  this->ComputeBoundingBox();
}

/** Destructor */
template< unsigned int TDimension >
ImageMaskSpatialObject< TDimension >
::~ImageMaskSpatialObject()
{}

/** Test whether a point is inside or outside the object
 *  For computational speed purposes, it is faster if the method does not
 *  check the name of the class and the current depth */
template< unsigned int TDimension >
bool
ImageMaskSpatialObject< TDimension >
::IsInside(const PointType & point) const
{
  if ( !this->GetBounds()->IsInside(point) )
    {
    return false;
    }

  if ( !this->SetInternalInverseTransformToWorldToIndexTransform() )
    {
    return false;
    }

  PointType p = this->GetInternalInverseTransform()->TransformPoint(point);

  typename Superclass::InterpolatorType::ContinuousIndexType index;
  for ( unsigned int i = 0; i < TDimension; i++ )
    {
    index[i] = p[i];
    }

  const bool insideBuffer =
    this->GetImage()->GetBufferedRegion().IsInside(index);

  if ( !insideBuffer )
    {
    return false;
    }

  typedef typename InterpolatorType::OutputType InterpolatorOutputType;
  const bool insideMask = (
    Math::NotExactlyEquals( DefaultConvertPixelTraits<InterpolatorOutputType>::GetScalarValue(this->m_Interpolator->EvaluateAtContinuousIndex(index)),
    NumericTraits<PixelType>::ZeroValue() ) );
  return insideMask;
}

/** Return true if the given point is inside the image */
template< unsigned int TDimension >
bool
ImageMaskSpatialObject< TDimension >
::IsInside(const PointType & point, unsigned int depth, char *name) const
{
  if ( name == ITK_NULLPTR )
    {
    if ( IsInside(point) )
      {
      return true;
      }
    }
  else if ( strstr(typeid( Self ).name(), name) )
    {
    if ( IsInside(point) )
      {
      return true;
      }
    }
  return SpatialObject< TDimension >::IsInside(point, depth, name);
}

template< unsigned int TDimension >
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

  PixelType  outsideValue = NumericTraits< PixelType >::ZeroValue();
  RegionType region;

  ImagePointer image = this->GetImage();

  IndexType index;
  SizeType  size;

  for(unsigned int i(0); i < ImageType::ImageDimension; i++)
    {
    index[i] = 0;
    size[i] = 0;
    }

  if ( ImageType::ImageDimension == 3 )
    {
    for ( unsigned int axis = 0; axis < ImageType::ImageDimension; axis++ )
      {
      // Two slice iterators along each axis...
      // Find the orthogonal planes for the slices
      unsigned int i, j;
      unsigned int direction[2];
      for ( i = 0, j = 0; i < 3; ++i )
        {
        if ( i != axis )
          {
          direction[j] = i;
          j++;
          }
        }

      // Create the forward iterator to find lower bound
      SliceIteratorType fit( image,  image->GetRequestedRegion() );
      fit.SetFirstDirection(direction[1]);
      fit.SetSecondDirection(direction[0]);

      fit.GoToBegin();
      while ( !fit.IsAtEnd() )
        {
        while ( !fit.IsAtEndOfSlice() )
          {
          while ( !fit.IsAtEndOfLine() )
            {
            if ( fit.Get() != outsideValue )
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
      SliceIteratorType rit( image,  image->GetRequestedRegion() );
      rit.SetFirstDirection(direction[1]);
      rit.SetSecondDirection(direction[0]);

      rit.GoToReverseBegin();
      while ( !rit.IsAtReverseEnd() )
        {
        while ( !rit.IsAtReverseEndOfSlice() )
          {
          while ( !rit.IsAtReverseEndOfLine() )
            {
            if ( rit.Get() != outsideValue )
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

    region.SetIndex(index);
    region.SetSize(size);
    }
  else
    {
    //itkExceptionMacro( << "ImageDimension must be 3!" );
    typedef ImageRegionConstIteratorWithIndex< ImageType > IteratorType;
    IteratorType it( image, image->GetRequestedRegion() );
    it.GoToBegin();

    for ( unsigned int i = 0; i < ImageType::ImageDimension; ++i )
      {
      index[i] = image->GetRequestedRegion().GetSize(i);
      size[i]  = image->GetRequestedRegion().GetIndex(i);
      }

    while ( !it.IsAtEnd() )
      {
      if ( it.Get() != outsideValue )
        {
        IndexType tmpIndex = it.GetIndex();
        for ( unsigned int i = 0; i < ImageType::ImageDimension; ++i )
          {
          if ( index[i] > tmpIndex[i] )
            {
            index[i] = tmpIndex[i];
            }

          const SizeValueType tmpSize = static_cast< SizeValueType >( tmpIndex[i] );

          if ( size[i]  < tmpSize )
            {
            size[i]  = tmpSize;
            }
          }
        }
      ++it;
      }

    for ( unsigned int i = 0; i < ImageType::ImageDimension; ++i )
      {
      size[i] = size[i] - index[i] + 1;
      }
    region.SetIndex(index);
    region.SetSize(size);
    } // end else

  return region;
}

template< unsigned int TDimension >
bool
ImageMaskSpatialObject< TDimension >
::ComputeLocalBoundingBox() const
{
  itkDebugMacro("Computing ImageMaskSpatialObject bounding box");

  if ( this->GetBoundingBoxChildrenName().empty()
       || strstr( typeid( Self ).name(),
                  this->GetBoundingBoxChildrenName().c_str() ) )
    {

    // First get the region bounding box...
    RegionType boundingRegion = GetAxisAlignedBoundingBoxRegion();
    const typename RegionType::IndexType index = boundingRegion.GetIndex();
    const typename RegionType::SizeType  size = boundingRegion.GetSize();

    //Now find the corners (by index)
    typedef VectorContainer< unsigned int, typename RegionType::IndexType >
                                                         IndexContainerType;

    typename IndexContainerType::Pointer cornerInds = IndexContainerType::New();

    unsigned int c = 0;
    cornerInds->InsertElement(c++, index);
    for ( unsigned int i = 0; i < ImageType::ImageDimension; ++i )
      {
      unsigned int curSize = cornerInds->Size();
      for ( unsigned int ii = 0; ii < curSize; ++ii)
        {
        IndexType tmpIndex = cornerInds->ElementAt(ii);
        tmpIndex[i] += size[i];
        cornerInds->InsertElement(c++,tmpIndex);
        }
      }

    // Next Transform the corners of the bounding box
    typedef typename BoundingBoxType::PointsContainer PointsContainer;
    typename PointsContainer::Pointer transformedCorners = PointsContainer::New();
    transformedCorners->Reserve(static_cast<typename PointsContainer::ElementIdentifier>( cornerInds->size() ) );

    typename IndexContainerType::const_iterator it = cornerInds->begin();
    typename PointsContainer::iterator itTrans = transformedCorners->begin();
    while ( it != cornerInds->end() )
      {
      PointType origPnt;
      for ( unsigned int i = 0; i < ImageType::ImageDimension; ++i )
        {
        origPnt[i] = static_cast< typename PointType::CoordRepType>( (*it)[i]);
        }
      PointType pnt = this->GetIndexToWorldTransform()->TransformPoint(origPnt);
      *itTrans = pnt;
      ++it;
      ++itTrans;
      }

    // refresh the bounding box with the transformed corners
    const_cast< BoundingBoxType * >( this->GetBounds() )->SetPoints(transformedCorners);
    this->GetBounds()->ComputeBoundingBox();
    }
  return true;
}


/** Print the object */
template< unsigned int TDimension >
void
ImageMaskSpatialObject< TDimension >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}
} // end namespace itk

#endif //__ImageMaskSpatialObject_hxx
