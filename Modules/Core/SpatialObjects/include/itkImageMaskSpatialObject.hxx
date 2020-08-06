/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#include "itkImageRegionRange.h"

#include <cstdint> // For uintmax_t.

namespace itk
{
/** Constructor */
template <unsigned int TDimension, typename TPixel>
ImageMaskSpatialObject<TDimension, TPixel>::ImageMaskSpatialObject()
{
  this->SetTypeName("ImageMaskSpatialObject");
}

/** Test whether a point is inside or outside the object
 *  For computational speed purposes, it is faster if the method does not
 *  check the name of the class and the current depth */
template <unsigned int TDimension, typename TPixel>
bool
ImageMaskSpatialObject<TDimension, TPixel>::IsInsideInObjectSpace(const PointType & point) const
{
  const ImageType * const image = this->GetImage();

  const IndexType index = image->TransformPhysicalPointToIndex(point);

  return image->GetBufferedRegion().IsInside(index) &&
         Math::NotExactlyEquals(image->GetPixel(index), NumericTraits<PixelType>::ZeroValue());
}


template <unsigned int TDimension, typename TPixel>
void
ImageMaskSpatialObject<TDimension, TPixel>::ComputeMyBoundingBox()
{
  const ImageType * const image = this->GetImage();
  itkAssertOrThrowMacro(image != nullptr, "Ensure that SetImage has been called!");

  const RegionType boundingBoxInIndexSpace{ this->ComputeMyBoundingBoxInIndexSpace() };

  BoundingBoxType * const boundingBoxInObjectSpace = this->GetModifiableMyBoundingBoxInObjectSpace();

  // Assert should never fail as SpatialObject takes care of creating the BoundingBox.
  assert(boundingBoxInObjectSpace != nullptr);

  if (boundingBoxInIndexSpace.GetNumberOfPixels() == 0)
  {
    boundingBoxInObjectSpace->SetMinimum(PointType());
    boundingBoxInObjectSpace->SetMaximum(PointType());
  }
  else
  {
    const IndexType minIndex = boundingBoxInIndexSpace.GetIndex();

    typename Superclass::ContinuousIndexType minContinuousIndex{ minIndex };
    typename Superclass::ContinuousIndexType maxContinuousIndex{ minIndex + boundingBoxInIndexSpace.GetSize() };

    // Allow a margin of half a pixel in each direction.
    const typename SpatialObject<TDimension>::VectorType half_pixel_size{ 0.5 };
    minContinuousIndex -= half_pixel_size;
    maxContinuousIndex -= half_pixel_size;

    // Initially set the corner point corresponding to the minimum index as
    // both the minimum and maximum of the bounding box (in object space).
    // Afterwards, all other corners are considered.
    PointType firstPoint;
    image->TransformContinuousIndexToPhysicalPoint(minContinuousIndex, firstPoint);
    boundingBoxInObjectSpace->SetMinimum(firstPoint);
    boundingBoxInObjectSpace->SetMaximum(firstPoint);

    // The total number of corner points of the bounding box.
    constexpr auto numberOfCorners = std::uintmax_t{ 1 } << TDimension;

    for (std::uintmax_t cornerNumber{ 1 }; cornerNumber < numberOfCorners; ++cornerNumber)
    {
      // For each corner, estimate the n-dimensional index.

      auto continuousIndex = minContinuousIndex;

      for (unsigned dim{}; dim < TDimension; ++dim)
      {
        const std::uintmax_t bitMask{ std::uintmax_t{ 1 } << dim };

        if ((cornerNumber & bitMask) != 0)
        {
          continuousIndex[dim] = maxContinuousIndex[dim];
        }
      }

      // Consider the corner point that corresponds to this n-dimensional index.
      PointType cornerPoint;
      image->TransformContinuousIndexToPhysicalPoint(continuousIndex, cornerPoint);
      boundingBoxInObjectSpace->ConsiderPoint(cornerPoint);
    }
  }
}


/** InternalClone */
template <unsigned int TDimension, typename TPixel>
typename LightObject::Pointer
ImageMaskSpatialObject<TDimension, TPixel>::InternalClone() const
{
  // Default implementation just copies the parameters from
  // this to new transform.
  typename LightObject::Pointer loPtr = Superclass::InternalClone();

  typename Self::Pointer rval = dynamic_cast<Self *>(loPtr.GetPointer());
  if (rval.IsNull())
  {
    itkExceptionMacro(<< "downcast to type " << this->GetNameOfClass() << " failed.");
  }

  return loPtr;
}

/** Print the object */
template <unsigned int TDimension, typename TPixel>
void
ImageMaskSpatialObject<TDimension, TPixel>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}


template <unsigned int TDimension, typename TPixel>
typename ImageMaskSpatialObject<TDimension, TPixel>::RegionType
ImageMaskSpatialObject<TDimension, TPixel>::ComputeMyBoundingBoxInIndexSpace() const
{
  const ImagePointer imagePointer = this->GetImage();

  if (imagePointer == nullptr)
  {
    return {};
  }

  const ImageType & image = *imagePointer;

  const auto HasForegroundPixels = [&image](const RegionType & region) {
    for (const PixelType pixelValue : Experimental::ImageRegionRange<const ImageType>{ image, region })
    {
      constexpr auto zeroValue = NumericTraits<PixelType>::ZeroValue();

      if (pixelValue != zeroValue)
      {
        return true;
      }
    }
    return false;
  };

  const auto CreateRegion = [](const IndexType & minIndex, const IndexType & maxIndex) {
    SizeType regionSize;

    for (unsigned dim = 0; dim < SizeType::Dimension; ++dim)
    {
      regionSize[dim] = static_cast<SizeValueType>(maxIndex[dim] + 1 - minIndex[dim]);
    }
    return RegionType{ minIndex, regionSize };
  };

  const RegionType requestedRegion = image.GetRequestedRegion();

  if (requestedRegion.GetNumberOfPixels() == 0)
  {
    return {};
  }

  const SizeType imageSize = requestedRegion.GetSize();

  IndexType minIndex = requestedRegion.GetIndex();
  IndexType maxIndex = minIndex + imageSize;

  for (auto & maxIndexValue : maxIndex)
  {
    --maxIndexValue;
  }

  // Iterate from high to low (for significant performance reasons).
  for (int dim = TDimension - 1; dim >= 0; --dim)
  {
    auto subregion = CreateRegion(minIndex, maxIndex);
    subregion.SetSize(dim, 1);
    const auto initialMaxIndexValue = maxIndex[dim];

    // Estimate minIndex[dim]
    while (!HasForegroundPixels(subregion))
    {
      const auto indexValue = subregion.GetIndex(dim) + 1;

      if (indexValue > initialMaxIndexValue)
      {
        // The requested image region has only zero-valued pixels.
        return {};
      }
      subregion.SetIndex(dim, indexValue);
    }
    minIndex[dim] = subregion.GetIndex(dim);

    // Estimate maxIndex[dim]
    subregion.SetIndex(dim, initialMaxIndexValue);
    while (!HasForegroundPixels(subregion))
    {
      subregion.SetIndex(dim, subregion.GetIndex(dim) - 1);
    }
    maxIndex[dim] = subregion.GetIndex(dim);
  }
  return CreateRegion(minIndex, maxIndex);
}


#if !defined(ITK_LEGACY_REMOVE)
template <unsigned int TDimension, typename TPixel>
typename ImageMaskSpatialObject<TDimension, TPixel>::RegionType
ImageMaskSpatialObject<TDimension, TPixel>::GetAxisAlignedBoundingBoxRegion() const
{
  return ComputeMyBoundingBoxInIndexSpace();
}
#endif // ITK_LEGACY_REMOVE
} // end namespace itk

#endif //__ImageMaskSpatialObject_hxx
