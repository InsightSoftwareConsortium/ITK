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
  m_SlicePosition.Fill( 0 );

  this->SetPixelTypeName(static_cast<const PixelType *>(nullptr));

  m_Interpolator = NNInterpolatorType::New();
}

/** Destructor */
template< unsigned int TDimension, typename PixelType >
ImageSpatialObject< TDimension,  PixelType >
::~ImageSpatialObject()
{
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
::IsInsideInWorldSpace(const PointType & point, unsigned int depth,
  const std::string & name) const
{
  if( this->GetTypeName().find( name ) != std::string::npos )
    {
    PointType transformedPoint =
      this->GetObjectToWorldTransform()->GetInverseTransform()
        ->TransformPoint(point);

    IndexType index;
    bool isInside = m_Image->TransformPhysicalPointToIndex( transformedPoint,
      index );

    if( isInside )
      {
      return true;
      }
    }

  if( depth > 0 )
    {
    return Superclass::IsInsideChildrenInWorldSpace(point, depth-1, name);
    }

  return false;
}

/** Return the value of the image at a specified point
 *  The value returned is always of type double
 *  For RGB Images the value returned is the value of the first channel.
 */
template< unsigned int TDimension, typename PixelType >
bool
ImageSpatialObject< TDimension,  PixelType >
::ValueAtInWorldSpace(const PointType & point, double & value, unsigned int depth,
  const std::string & name) const
{
  if( this->GetTypeName().find( name ) != std::string::npos )
    {
    if( this->IsEvaluableAtInWorldSpace(point, 0, name) )
      {
      PointType transformedPoint = this->GetObjectToWorldTransform()->
        GetInverseTransform()->TransformPoint( point );

      ContinuousIndexType cIndex;
      bool isInside = m_Image->TransformPhysicalPointToContinuousIndex(
        transformedPoint, cIndex );

      if( isInside )
        {
        using InterpolatorOutputType = typename InterpolatorType::OutputType;
        value = static_cast< double >(
          DefaultConvertPixelTraits< InterpolatorOutputType >::GetScalarValue(
            m_Interpolator->EvaluateAtContinuousIndex(cIndex) ) );

        return true;
        }
      }
    }

  if ( depth > 0 )
    {
    return Superclass::ValueAtChildrenInWorldSpace(point, value, depth-1, name);
    }

  return false;
}

/** Compute the bounds of the image */
template< unsigned int TDimension, typename PixelType >
bool
ImageSpatialObject< TDimension,  PixelType >
::ComputeMyBoundingBoxInWorldSpace() const
{
  itkDebugMacro("Computing ImageSpatialObject bounding box");

  // First we compute the bounding box in the index space
  typename BoundingBoxType::Pointer indexSpaceBB = BoundingBoxType::New();

  IndexType    index = m_Image->GetLargestPossibleRegion().GetIndex();
  typename ImageType::SizeType     size = m_Image->GetLargestPossibleRegion().GetSize();

  PointType    pnt1;
  PointType    pnt2;
  for ( unsigned int i = 0; i < TDimension; i++ )
    {
    pnt1[i] = index[i];
    pnt2[i] = index[i] + size[i];
    }

  indexSpaceBB->SetMinimum(pnt1);
  indexSpaceBB->SetMaximum(pnt1);
  indexSpaceBB->ConsiderPoint(pnt2);
  indexSpaceBB->ComputeBoundingBox();

  // Next Transform the corners of the bounding box
  using PointsContainer = typename BoundingBoxType::PointsContainer;
  const PointsContainer *indexSpaceCorners = indexSpaceBB->GetCorners();
  typename PointsContainer::Pointer transformedCorners =
    PointsContainer::New();
  transformedCorners->Reserve(
    static_cast<typename PointsContainer::ElementIdentifier>(
      indexSpaceCorners->size() ) );

  auto it = indexSpaceCorners->begin();
  auto itTrans = transformedCorners->begin();
  while ( it != indexSpaceCorners->end() )
    {
    IndexType ind;
    for (unsigned int i = 0; i < TDimension; i++){
        ind[i] = (*it)[i];
    }
    PointType objectSpacePoint;
    m_Image->TransformIndexToPhysicalPoint(ind, objectSpacePoint);
    PointType pnt = this->GetObjectToWorldTransform()
      ->TransformPoint(objectSpacePoint);
    *itTrans = pnt;
    ++it;
    ++itTrans;
    }

  // refresh the bounding box with the transformed corners
  const_cast< BoundingBoxType * >( this->GetMyBoundingBoxInWorldSpace() )
    ->SetPoints(transformedCorners);
  this->GetMyBoundingBoxInWorldSpace()->ComputeBoundingBox();

  return true;
}


/** Set the image in the spatial object */
template< unsigned int TDimension, typename PixelType >
void
ImageSpatialObject< TDimension,  PixelType >
::SetImage(const ImageType *image)
{
  if ( !image )
    {
    itkDebugMacro("Image passed to ImageSpatialObject was null");
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
  os << indent << "Image: " << std::endl;
  os << indent << m_Image << std::endl;
  os << indent << "Interpolator: " << std::endl;
  os << indent << m_Interpolator << std::endl;
  os << indent << "SlicePosition: " << m_SlicePosition << std::endl;
  os << indent << "PixelType: " << m_PixelType << std::endl;
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
::SetSlicePositionInWorldSpace(unsigned int dimension, int position)
{
  m_SlicePosition[dimension] = position;
  this->Modified();
}
} // end namespace itk

#endif //__ImageSpatialObject_hxx
