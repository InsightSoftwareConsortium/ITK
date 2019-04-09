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
template< unsigned int TDimension, typename TPixel >
ImageMaskSpatialObject< TDimension, TPixel >
::ImageMaskSpatialObject()
{
  this->SetTypeName("ImageMaskSpatialObject");
}

/** Test whether a point is inside or outside the object
 *  For computational speed purposes, it is faster if the method does not
 *  check the name of the class and the current depth */
template< unsigned int TDimension, typename TPixel >
bool
ImageMaskSpatialObject< TDimension, TPixel >
::IsInsideInObjectSpace(const PointType & point, unsigned int depth,
  const std::string & name ) const
{
  if( this->GetTypeName().find( name ) != std::string::npos )
    {
    if( this->GetMyBoundingBoxInObjectSpace()->IsInside(point) )
      {
      typename Superclass::InterpolatorType::ContinuousIndexType index;
      if( this->GetImage()->TransformPhysicalPointToContinuousIndex( point,
          index ) )
        {
        using InterpolatorOutputType = typename InterpolatorType::OutputType;
        bool insideMask = (
          Math::NotExactlyEquals(
            DefaultConvertPixelTraits<InterpolatorOutputType>::GetScalarValue(
              this->GetInterpolator()->EvaluateAtContinuousIndex(index)),
            NumericTraits<PixelType>::ZeroValue() ) );
        if( insideMask )
          {
          return true;
          }
        }
      }
    }
  if( depth > 0 )
    {
    return Superclass::IsInsideChildrenInObjectSpace( point, depth, name );
    }

  return false;
}

template< unsigned int TDimension, typename TPixel >
void
ImageMaskSpatialObject< TDimension, TPixel >
::ComputeMyBoundingBox()
{
  using IteratorType = ImageRegionConstIteratorWithIndex< ImageType >;
  IteratorType it( this->GetImage(),
    this->GetImage()->GetLargestPossibleRegion() );
  IteratorType prevIt( this->GetImage(),
    this->GetImage()->GetLargestPossibleRegion() );
  it.GoToBegin();
  prevIt = it;

  bool first = true;
  PixelType outsideValue = NumericTraits< PixelType >::ZeroValue();
  PixelType value = outsideValue;
  PixelType prevValue = outsideValue;
  IndexType tmpIndex;
  PointType tmpPoint;
  int count = 0;
  int rowSize
    = this->GetImage()->GetLargestPossibleRegion().GetSize()[0];
  while ( !it.IsAtEnd() )
    {
    value = it.Get();
    if ( value != prevValue || ( count == rowSize-1 && value != outsideValue ) )
      {
      prevValue = value;
      if( value == outsideValue )
        {
        tmpIndex = prevIt.GetIndex();
        }
      else
        {
        tmpIndex = it.GetIndex();
        }
      this->GetImage()->TransformIndexToPhysicalPoint( tmpIndex, tmpPoint );
      if( first )
        {
        first = false;
        this->GetModifiableMyBoundingBoxInObjectSpace()->SetMinimum( tmpPoint );
        this->GetModifiableMyBoundingBoxInObjectSpace()->SetMaximum( tmpPoint );
        }
      else
        {
        this->GetModifiableMyBoundingBoxInObjectSpace()->ConsiderPoint( tmpPoint );
        }
      }
    prevIt = it;
    ++it;
    ++count;
    if( count == rowSize )
      {
      count = 0;
      prevValue = outsideValue;
      }
    }

  if( first )
    {
    tmpPoint.Fill(
      NumericTraits< typename BoundingBoxType::PointType::ValueType >::ZeroValue() );

    this->GetModifiableMyBoundingBoxInObjectSpace()->SetMinimum( tmpPoint );
    this->GetModifiableMyBoundingBoxInObjectSpace()->SetMaximum( tmpPoint );

    // NOT AN EXCEPTION!!!, used to return false, but never checked
    // itkExceptionMacro(<< "ImageMask bounding box computation failed.")
    }
}

/** InternalClone */
template< unsigned int TDimension, typename TPixel >
typename LightObject::Pointer
ImageMaskSpatialObject< TDimension, TPixel >
::InternalClone() const
{
  // Default implementation just copies the parameters from
  // this to new transform.
  typename LightObject::Pointer loPtr = Superclass::InternalClone();

  typename Self::Pointer rval =
    dynamic_cast<Self *>(loPtr.GetPointer());
  if(rval.IsNull())
    {
    itkExceptionMacro(<< "downcast to type "
                      << this->GetNameOfClass()
                      << " failed.");
    }

  return loPtr;
}

/** Print the object */
template< unsigned int TDimension, typename TPixel >
void
ImageMaskSpatialObject< TDimension, TPixel >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

#if ! defined ( ITK_LEGACY_REMOVE )
template< unsigned int TDimension, typename TPixel >
typename ImageMaskSpatialObject< TDimension, TPixel >::RegionType
ImageMaskSpatialObject< TDimension, TPixel >
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
    using IteratorType = ImageRegionConstIteratorWithIndex< ImageType >;
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

          const auto tmpSize = static_cast< SizeValueType >( tmpIndex[i] );

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
#endif //ITK_LEGACY_REMOVE
} // end namespace itk

#endif //__ImageMaskSpatialObject_hxx
