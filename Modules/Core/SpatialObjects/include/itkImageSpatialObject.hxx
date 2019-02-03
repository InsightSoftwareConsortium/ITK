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
#ifndef itkImageSpatialObject_hxx
#define itkImageSpatialObject_hxx

#include "itkImageSpatialObject.h"
#include "itkSize.h"
#include "itkDefaultConvertPixelTraits.h"

namespace itk
{
/** Constructor */
template< unsigned int TDimension, typename PixelType >
ImageSpatialObject< TDimension,  PixelType >
::ImageSpatialObject()
{
  this->SetTypeName("ImageSpatialObject");
  m_Image = ImageType::New();
  m_SlicePosition = new int[TDimension];
  for ( unsigned int i = 0; i < TDimension; i++ )
    {
    m_SlicePosition[i] = 0;
    }

  this->InternalSetPixelType(static_cast<const PixelType *>(nullptr));
  m_Interpolator = NNInterpolatorType::New();
}

/** Destructor */
template< unsigned int TDimension, typename PixelType >
ImageSpatialObject< TDimension,  PixelType >
::~ImageSpatialObject()
{
  delete[] m_SlicePosition;
}

/** Set the interpolator */
template< unsigned int TDimension, typename PixelType >
void
ImageSpatialObject< TDimension,  PixelType >
::SetInterpolator(InterpolatorType *interpolator)
{
  m_Interpolator = interpolator;
  if ( m_Image )
    {
    m_Interpolator->SetInputImage(m_Image);
    }
}


/** Return true if the given point is inside the image */
template< unsigned int TDimension, typename PixelType >
bool
ImageSpatialObject< TDimension,  PixelType >
::IsInside(const PointType & point, unsigned int depth, char *name) const
{
{
  PointType transformedPoint =
    this->GetInternalInverseTransform()->TransformPoint(point);

  bool isInside = true;
  typename ImageType::RegionType region = m_Image->GetLargestPossibleRegion();
  itk::Size< TDimension > size = region.GetSize();

  for ( unsigned int i = 0; i < TDimension; i++ )
    {
    if ( size[i] )
      {
      if ( ( transformedPoint[i] > size[i] ) || ( transformedPoint[i] < 0 ) )
        {
        isInside = false;
        break;
        }
      }
    else
      {
      itkExceptionMacro(<< "Size of the ImageSpatialObject must be non-zero!");
      }
    }

  return isInside;
}
  if ( name == nullptr )
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

  return Superclass::IsInside(point, depth, name);
}

/** Return the value of the image at a specified point
 *  The value returned is always of type double
 *  For RGB Images the value returned is the value of the first channel.
 */
template< unsigned int TDimension, typename PixelType >
bool
ImageSpatialObject< TDimension,  PixelType >
::ValueAt(const PointType & point, double & value, unsigned int depth,
          char *name) const
{
  bool returnValue = false;

  if ( IsEvaluableAt(point, 0, name) )
    {
    if ( !this->SetInternalInverseTransformToWorldToIndexTransform() )
      {
      return returnValue;
      }

    PointType p = this->GetInternalInverseTransform()->TransformPoint(point);

    typename InterpolatorType::ContinuousIndexType index;
    using InterpolatorOutputType = typename InterpolatorType::OutputType;
    for ( unsigned int i = 0; i < TDimension; i++ )
      {
      index[i] = p[i];
      }

    value = static_cast< double >(
      DefaultConvertPixelTraits< InterpolatorOutputType >::GetScalarValue(
        m_Interpolator->EvaluateAtContinuousIndex(index) ) );

    returnValue = true;
    }
  else
    {
    if ( Superclass::IsEvaluableAt(point, depth, name) )
      {
      double val;
      Superclass::ValueAt(point, val, depth, name);
      value = val;
      returnValue = true;
      }
    else
      {
      value = this->GetDefaultOutsideValue();
      returnValue = false;
      }
    }
  return returnValue;
}

/** Compute the bounds of the image */
template< unsigned int TDimension, typename PixelType >
bool
ImageSpatialObject< TDimension,  PixelType >
::ComputeObjectBoundingBox() const
{
  if ( this->GetBoundingBoxChildrenName().empty()
       || strstr( typeid( Self ).name(),
                  this->GetBoundingBoxChildrenName().c_str() ) )
    {
    typename ImageType::RegionType region =
      m_Image->GetLargestPossibleRegion();
    itk::Size< TDimension > size = region.GetSize();
    PointType               pointLow, pointHigh;

    unsigned int i;
    for ( i = 0; i < TDimension; i++ )
      {
      pointLow[i] = 0;
      pointHigh[i] = size[i];
      }

    typename BoundingBoxType::Pointer bb = BoundingBoxType::New();
    bb->SetMinimum(pointLow);
    bb->SetMaximum(pointHigh);
    using PointsContainerType = typename BoundingBoxType::PointsContainer;
    const PointsContainerType *corners = bb->GetCorners();

    auto itC = corners->begin();
    i = 0;
    while ( itC != corners->end() )
      {
      PointType transformedPoint = this->GetIndexToWorldTransform()->TransformPoint(*itC);
      if ( i == 0 )
        {
        const_cast< BoundingBoxType * >( this->GetBounds() )->SetMinimum(transformedPoint);
        }
      else if ( i == 1 )
        {
        const_cast< BoundingBoxType * >( this->GetBounds() )->SetMaximum(transformedPoint);
        }
      else
        {
        const_cast< BoundingBoxType * >( this->GetBounds() )->ConsiderPoint(transformedPoint);
        }
      itC++;
      i++;
      }

    return true;
    }

  return false;
}

/** Set the image in the spatial object */
template< unsigned int TDimension, typename PixelType >
void
ImageSpatialObject< TDimension,  PixelType >
::SetImage(const ImageType *image)
{
  if ( !image )
    {
    return;
    }

  m_Image = image;

  this->Modified();

  m_Interpolator->SetInputImage(m_Image);
}

/** Get the image inside the spatial object */
template< unsigned int TDimension, typename PixelType >
const typename ImageSpatialObject< TDimension,  PixelType >::ImageType *
ImageSpatialObject< TDimension,  PixelType >
::GetImage() const
{
  return m_Image.GetPointer();
}

/** Print the object */
template< unsigned int TDimension, typename PixelType >
void
ImageSpatialObject< TDimension,  PixelType >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << "Image: " << std::endl;
  os << indent << m_Image << std::endl;
  os << "Interpolator: " << std::endl;
  os << indent << m_Interpolator << std::endl;
}

/** Get the modification time */
template< unsigned int TDimension, typename PixelType >
ModifiedTimeType
ImageSpatialObject< TDimension,  PixelType >
::GetMTime() const
{
  ModifiedTimeType latestMTime = Superclass::GetMTime();
  const ModifiedTimeType imageMTime = m_Image->GetMTime();

  if ( imageMTime > latestMTime )
    {
    latestMTime = imageMTime;
    }

  return latestMTime;
}

/** Set the slice position */
template< unsigned int TDimension, typename PixelType >
void
ImageSpatialObject< TDimension,  PixelType >
::SetSlicePosition(unsigned int dimension, int position)
{
  m_SlicePosition[dimension] = position;
  this->Modified();
}
} // end namespace itk

#endif //__ImageSpatialObject_hxx
