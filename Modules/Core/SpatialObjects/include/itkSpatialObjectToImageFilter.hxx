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
#ifndef itkSpatialObjectToImageFilter_hxx
#define itkSpatialObjectToImageFilter_hxx

#include "itkSpatialObjectToImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkProgressReporter.h"
#include "itkMath.h"

namespace itk
{
/** Constructor */
template< typename TInputSpatialObject, typename TOutputImage >
SpatialObjectToImageFilter< TInputSpatialObject, TOutputImage >
::SpatialObjectToImageFilter()
{
  this->SetNumberOfRequiredInputs(1);
  m_ChildrenDepth = 999999;
  m_Size.Fill(0);
  m_Direction.SetIdentity();

  for ( unsigned int i = 0; i < OutputImageDimension; i++ )
    {
    m_Spacing[i] = 1.0;
    m_Origin[i] = 0.;
    }

  m_InsideValue = 0;
  m_OutsideValue = 0;
  m_UseObjectValue = false;
}

/** Destructor */
template< typename TInputSpatialObject, typename TOutputImage >
SpatialObjectToImageFilter< TInputSpatialObject, TOutputImage >
::~SpatialObjectToImageFilter()
{}

/** Set the Input SpatialObject */
template< typename TInputSpatialObject, typename TOutputImage >
void
SpatialObjectToImageFilter< TInputSpatialObject, TOutputImage >
::SetInput(const InputSpatialObjectType *input)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput( 0,
                                    const_cast< InputSpatialObjectType * >( input ) );
}

/** Connect one of the operands  */
template< typename TInputSpatialObject, typename TOutputImage >
void
SpatialObjectToImageFilter< TInputSpatialObject, TOutputImage >
::SetInput(unsigned int index, const TInputSpatialObject *object)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput( index,
                                    const_cast< TInputSpatialObject * >( object ) );
}

/** Get the input Spatial Object */
template< typename TInputSpatialObject, typename TOutputImage >
const typename SpatialObjectToImageFilter< TInputSpatialObject, TOutputImage >::InputSpatialObjectType *
SpatialObjectToImageFilter< TInputSpatialObject, TOutputImage >
::GetInput(void)
{
  return static_cast< const TInputSpatialObject * >( this->GetPrimaryInput() );
}

/** Get the input Spatial Object */
template< typename TInputSpatialObject, typename TOutputImage >
const typename SpatialObjectToImageFilter< TInputSpatialObject, TOutputImage >::InputSpatialObjectType *
SpatialObjectToImageFilter< TInputSpatialObject, TOutputImage >
::GetInput(unsigned int idx)
{
  return static_cast< const TInputSpatialObject * >
         ( this->ProcessObject::GetInput(idx) );
}

//----------------------------------------------------------------------------
template< typename TInputSpatialObject, typename TOutputImage >
void
SpatialObjectToImageFilter< TInputSpatialObject, TOutputImage >
::SetSpacing(const SpacingType & spacing)
{
  unsigned int i;

  for ( i = 0; i < TOutputImage::ImageDimension; i++ )
    {
    if ( Math::NotExactlyEquals((double)spacing[i], m_Spacing[i]) )
      {
      break;
      }
    }
  if ( i < TOutputImage::ImageDimension )
    {
    for ( i = 0; i < TOutputImage::ImageDimension; i++ )
      {
      m_Spacing[i] = spacing[i];
      }
    this->Modified();
    }
}

//----------------------------------------------------------------------------
template< typename TInputSpatialObject, typename TOutputImage >
void
SpatialObjectToImageFilter< TInputSpatialObject, TOutputImage >
::SetSpacing(const double *spacing)
{
  unsigned int i;

  for ( i = 0; i < OutputImageDimension; i++ )
    {
    if ( Math::NotExactlyEquals(spacing[i], m_Spacing[i]) )
      {
      break;
      }
    }
  if ( i < OutputImageDimension )
    {
    for ( i = 0; i < OutputImageDimension; i++ )
      {
      m_Spacing[i] = spacing[i];
      }
    this->Modified();
    }
}

template< typename TInputSpatialObject, typename TOutputImage >
void
SpatialObjectToImageFilter< TInputSpatialObject, TOutputImage >
::SetSpacing(const float *spacing)
{
  unsigned int i;

  for ( i = 0; i < OutputImageDimension; i++ )
    {
    if ( Math::NotExactlyEquals((double)spacing[i], m_Spacing[i]) )
      {
      break;
      }
    }
  if ( i < OutputImageDimension )
    {
    for ( i = 0; i < OutputImageDimension; i++ )
      {
      m_Spacing[i] = spacing[i];
      }
    this->Modified();
    }
}

template< typename TInputSpatialObject, typename TOutputImage >
const double *
SpatialObjectToImageFilter< TInputSpatialObject, TOutputImage >
::GetSpacing() const
{
  return m_Spacing;
}

//----------------------------------------------------------------------------
template< typename TInputSpatialObject, typename TOutputImage >
void
SpatialObjectToImageFilter< TInputSpatialObject, TOutputImage >
::SetOrigin(const PointType & origin)
{
  unsigned int i;

  for ( i = 0; i < OutputImageDimension; i++ )
    {
    if ( Math::NotExactlyEquals((double)origin[i], m_Origin[i]) )
      {
      break;
      }
    }
  if ( i < OutputImageDimension )
    {
    for ( i = 0; i < OutputImageDimension; i++ )
      {
      m_Origin[i] = origin[i];
      }
    this->Modified();
    }
}

//----------------------------------------------------------------------------
template< typename TInputSpatialObject, typename TOutputImage >
void
SpatialObjectToImageFilter< TInputSpatialObject, TOutputImage >
::SetOrigin(const double *origin)
{
  unsigned int i;

  for ( i = 0; i < OutputImageDimension; i++ )
    {
    if ( Math::NotExactlyEquals(origin[i], m_Origin[i]) )
      {
      break;
      }
    }
  if ( i < OutputImageDimension )
    {
    for ( i = 0; i < OutputImageDimension; i++ )
      {
      m_Origin[i] = origin[i];
      }
    this->Modified();
    }
}

template< typename TInputSpatialObject, typename TOutputImage >
void
SpatialObjectToImageFilter< TInputSpatialObject, TOutputImage >
::SetOrigin(const float *origin)
{
  unsigned int i;

  for ( i = 0; i < OutputImageDimension; i++ )
    {
    if ( Math::NotExactlyEquals((double)origin[i], m_Origin[i]) )
      {
      break;
      }
    }
  if ( i < OutputImageDimension )
    {
    for ( i = 0; i < OutputImageDimension; i++ )
      {
      m_Origin[i] = origin[i];
      }
    this->Modified();
    }
}

template< typename TInputSpatialObject, typename TOutputImage >
const double *
SpatialObjectToImageFilter< TInputSpatialObject, TOutputImage >
::GetOrigin() const
{
  return m_Origin;
}

//----------------------------------------------------------------------------

template< typename TInputSpatialObject, typename TOutputImage >
void
SpatialObjectToImageFilter< TInputSpatialObject, TOutputImage >
::SetDirection(const DirectionType & dir)
{
  m_Direction = dir;
  this->Modified();
}

template< typename TInputSpatialObject, typename TOutputImage >
const typename SpatialObjectToImageFilter< TInputSpatialObject, TOutputImage >::DirectionType &
SpatialObjectToImageFilter< TInputSpatialObject, TOutputImage >
::GetDirection(void) const
{
  return m_Direction;
}

//----------------------------------------------------------------------------

/** Update */
template< typename TInputSpatialObject, typename TOutputImage >
void
SpatialObjectToImageFilter< TInputSpatialObject, TOutputImage >
::GenerateData(void)
{
  unsigned int i;

  itkDebugMacro(<< "SpatialObjectToImageFilter::Update() called");

  // Get the input and output pointers
  const InputSpatialObjectType *InputObject  = this->GetInput();
  OutputImagePointer            OutputImage = this->GetOutput();

  // Generate the image
  SizeType size;

  InputObject->ComputeBoundingBox();
  for ( i = 0; i < ObjectDimension; i++ )
    {
    size[i] = static_cast<SizeValueType>( InputObject->GetBoundingBox()->GetMaximum()[i]
                                   - InputObject->GetBoundingBox()->GetMinimum()[i] );
    }

  typename OutputImageType::IndexType index;
  index.Fill(0);
  typename OutputImageType::RegionType region;

  // If the size of the output has been explicitly specified, the filter
  // will set the output size to the explicit size, otherwise the size from the
  // spatial
  // object's bounding box will be used as default.

  bool specified = false;
  for ( i = 0; i < OutputImageDimension; i++ )
    {
    if ( m_Size[i] != 0 )
      {
      specified = true;
      break;
      }
    }

  if ( specified )
    {
    region.SetSize(m_Size);
    }
  else
    {
    region.SetSize(size);
    }
  region.SetIndex(index);

  OutputImage->SetLargestPossibleRegion(region);      //
  OutputImage->SetBufferedRegion(region);             // set the region
  OutputImage->SetRequestedRegion(region);            //
  // If the spacing has been explicitly specified, the filter
  // will set the output spacing to that explicit spacing, otherwise the spacing
  // from
  // the spatial object is used as default.

  specified = false;
  for ( i = 0; i < OutputImageDimension; i++ )
    {
    if ( Math::NotExactlyEquals(m_Spacing[i], 0) )
      {
      specified = true;
      break;
      }
    }

  if ( specified )
    {
    OutputImage->SetSpacing(this->m_Spacing);         // set spacing
    }
  else
    {
    // set spacing
    OutputImage->SetSpacing( InputObject->GetIndexToObjectTransform()->GetScaleComponent() );
    }
  OutputImage->SetOrigin(m_Origin);     //   and origin
  OutputImage->SetDirection(m_Direction);
  OutputImage->Allocate();   // allocate the image

  typedef itk::ImageRegionIteratorWithIndex< OutputImageType > myIteratorType;

  myIteratorType it(OutputImage, region);

  itk::Point< double, ObjectDimension >      objectPoint;
  itk::Point< double, OutputImageDimension > imagePoint;

  ProgressReporter
    progress(this,0,OutputImage->GetRequestedRegion().GetNumberOfPixels());

  while( !it.IsAtEnd() )
    {
    // ValueAt requires the point to be in physical coordinate i.e
    OutputImage->TransformIndexToPhysicalPoint(it.GetIndex(), imagePoint);
    for ( i = 0; i < ObjectDimension; i++ )
      {
      objectPoint[i] = imagePoint[i];
      }

    double val = 0;

    bool evaluable = InputObject->ValueAt(objectPoint, val, m_ChildrenDepth);
    if ( Math::NotExactlyEquals(m_InsideValue, NumericTraits< ValueType >:: ZeroValue()) || Math::NotExactlyEquals(m_OutsideValue, NumericTraits< ValueType >::ZeroValue()) )
      {
      if ( evaluable )
        {
        if ( m_UseObjectValue )
          {
          it.Set( static_cast< ValueType >( val ) );
          }
        else
          {
          it.Set(m_InsideValue);
          }
        }
      else
        {
        it.Set(m_OutsideValue);
        }
      }
    else
      {
      it.Set( static_cast< ValueType >( val ) );
      }
    ++it;
    progress.CompletedPixel();
    }

  itkDebugMacro(<< "SpatialObjectToImageFilter::Update() finished");
} // end update function

template< typename TInputSpatialObject, typename TOutputImage >
void
SpatialObjectToImageFilter< TInputSpatialObject, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Size : " << m_Size << std::endl;
  os << indent << "Children depth : " << m_ChildrenDepth << std::endl;
  os << indent << "Inside Value : " << m_InsideValue << std::endl;
  os << indent << "Outside Value : " << m_OutsideValue << std::endl;
  if ( m_UseObjectValue )
    {
    os << indent << "Using Object Value : ON" << std::endl;
    }
  else
    {
    os << indent << "Using Object Value : OFF" << std::endl;
    }
}
} // end namespace itk

#endif
