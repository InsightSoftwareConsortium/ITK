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
bool
ImageMaskSpatialObject< TDimension, TPixel >
::ComputeMyBoundingBox() const
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
      NumericTraits< typename BoundingBoxType::PointType::ValueType >::
      ZeroValue() );

    this->GetModifiableMyBoundingBoxInObjectSpace()->SetMinimum( tmpPoint );
    this->GetModifiableMyBoundingBoxInObjectSpace()->SetMaximum( tmpPoint );

    return false;
    }

  return true;
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
} // end namespace itk

#endif //__ImageMaskSpatialObject_hxx
