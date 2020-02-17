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
#ifndef itkImageSpatialObject_hxx
#define itkImageSpatialObject_hxx

#include "itkImageSpatialObject.h"
#include "itkSize.h"
#include "itkDefaultConvertPixelTraits.h"

namespace itk
{
/** Constructor */
template <unsigned int TDimension, typename PixelType>
ImageSpatialObject<TDimension, PixelType>::ImageSpatialObject()
{
  this->SetTypeName("ImageSpatialObject");

  this->Clear();

  this->Update();
}

/** Destructor */
template <unsigned int TDimension, typename PixelType>
ImageSpatialObject<TDimension, PixelType>::~ImageSpatialObject() = default;

template <unsigned int TDimension, typename PixelType>
void
ImageSpatialObject<TDimension, PixelType>::Clear()
{
  Superclass::Clear();

  m_Image = ImageType::New();
  m_SliceNumber.Fill(0);

#if !defined(ITK_LEGACY_REMOVE)
  this->SetPixelTypeName(static_cast<const PixelType *>(nullptr));
#endif

  m_Interpolator = NNInterpolatorType::New();

  this->Modified();
}

/** Set the interpolator */
template <unsigned int TDimension, typename PixelType>
void
ImageSpatialObject<TDimension, PixelType>::SetInterpolator(InterpolatorType * interpolator)
{
  if (m_Interpolator != interpolator)
  {
    m_Interpolator = interpolator;
    if (m_Image && m_Interpolator)
    {
      m_Interpolator->SetInputImage(m_Image);
    }
    this->Modified();
  }
}


/** Return true if the given point is inside the image */
template <unsigned int TDimension, typename PixelType>
bool
ImageSpatialObject<TDimension, PixelType>::IsInsideInObjectSpace(const PointType & point) const
{
  IndexType index;
  return m_Image->TransformPhysicalPointToIndex(point, index);
}

/** Return the value of the image at a specified point
 *  The value returned is always of type double
 *  For RGB Images the value returned is the value of the first channel.
 */
template <unsigned int TDimension, typename PixelType>
bool
ImageSpatialObject<TDimension, PixelType>::ValueAtInObjectSpace(const PointType &   point,
                                                                double &            value,
                                                                unsigned int        depth,
                                                                const std::string & name) const
{
  if (this->GetTypeName().find(name) != std::string::npos)
  {
    if (this->IsEvaluableAtInObjectSpace(point, 0, name))
    {
      ContinuousIndexType cIndex;
      bool                isInside = m_Image->TransformPhysicalPointToContinuousIndex(point, cIndex);

      if (isInside)
      {
        using InterpolatorOutputType = typename InterpolatorType::OutputType;
        value = static_cast<double>(DefaultConvertPixelTraits<InterpolatorOutputType>::GetScalarValue(
          m_Interpolator->EvaluateAtContinuousIndex(cIndex)));

        return true;
      }
    }
  }

  if (depth > 0)
  {
    return Superclass::ValueAtChildrenInObjectSpace(point, value, depth - 1, name);
  }

  return false;
}

/** Compute the bounds of the image */
template <unsigned int TDimension, typename PixelType>
void
ImageSpatialObject<TDimension, PixelType>::ComputeMyBoundingBox()
{
  itkDebugMacro("Computing ImageSpatialObject bounding box");

  IndexType                    index = m_Image->GetLargestPossibleRegion().GetIndex();
  typename ImageType::SizeType size = m_Image->GetLargestPossibleRegion().GetSize();

  IndexType index2;
  for (unsigned int i = 0; i < TDimension; ++i)
  {
    index2[i] = index[i] + size[i];
  }
  PointType pnt1;
  PointType pnt2;
  m_Image->TransformIndexToPhysicalPoint(index, pnt1);
  m_Image->TransformIndexToPhysicalPoint(index2, pnt2);

  this->GetModifiableMyBoundingBoxInObjectSpace()->SetMinimum(pnt1);
  this->GetModifiableMyBoundingBoxInObjectSpace()->SetMaximum(pnt1);
  this->GetModifiableMyBoundingBoxInObjectSpace()->ConsiderPoint(pnt2);
  this->GetModifiableMyBoundingBoxInObjectSpace()->ComputeBoundingBox();
}


/** Set the image in the spatial object */
template <unsigned int TDimension, typename PixelType>
void
ImageSpatialObject<TDimension, PixelType>::SetImage(const ImageType * image)
{
  if (m_Image != image)
  {
    if (!image)
    {
      itkDebugMacro("Image passed to ImageSpatialObject was null");
      return;
    }

    m_Image = image;
    if (m_Interpolator)
    {
      m_Interpolator->SetInputImage(m_Image);
    }

    this->Modified();
  }
}

/** Get the image inside the spatial object */
template <unsigned int TDimension, typename PixelType>
const typename ImageSpatialObject<TDimension, PixelType>::ImageType *
ImageSpatialObject<TDimension, PixelType>::GetImage() const
{
  return m_Image.GetPointer();
}

/** InternalClone */
template <unsigned int TDimension, typename PixelType>
typename LightObject::Pointer
ImageSpatialObject<TDimension, PixelType>::InternalClone() const
{
  // Default implementation just copies the parameters from
  // this to new transform.
  typename LightObject::Pointer loPtr = Superclass::InternalClone();

  typename Self::Pointer rval = dynamic_cast<Self *>(loPtr.GetPointer());
  if (rval.IsNull())
  {
    itkExceptionMacro(<< "downcast to type " << this->GetNameOfClass() << " failed.");
  }
  rval->SetImage(this->GetImage()->Clone());
  rval->SetSliceNumber(this->GetSliceNumber());
  rval->SetInterpolator(this->GetInterpolator());

  return loPtr;
}

/** Print the object */
template <unsigned int TDimension, typename PixelType>
void
ImageSpatialObject<TDimension, PixelType>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Image: " << std::endl;
  os << indent << m_Image << std::endl;
  os << indent << "Interpolator: " << std::endl;
  os << indent << m_Interpolator << std::endl;
  os << indent << "SliceNumber: " << m_SliceNumber << std::endl;
#if !defined(ITK_LEGACY_REMOVE)
  os << indent << "PixelType: " << m_PixelType << std::endl;
#endif
}

/** Get the modification time */
template <unsigned int TDimension, typename PixelType>
ModifiedTimeType
ImageSpatialObject<TDimension, PixelType>::GetMTime() const
{
  ModifiedTimeType       latestMTime = Superclass::GetMTime();
  const ModifiedTimeType imageMTime = m_Image->GetMTime();

  if (imageMTime > latestMTime)
  {
    latestMTime = imageMTime;
  }

  return latestMTime;
}

/** Set the slice position */
template <unsigned int TDimension, typename PixelType>
void
ImageSpatialObject<TDimension, PixelType>::SetSliceNumber(unsigned int dimension, int position)
{
  if (dimension < ObjectDimension && m_SliceNumber[dimension] != position)
  {
    m_SliceNumber[dimension] = position;
    this->Modified();
  }
}
} // end namespace itk

#endif //__ImageSpatialObject_hxx
