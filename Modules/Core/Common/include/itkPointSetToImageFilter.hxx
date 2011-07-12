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
#ifndef __itkPointSetToImageFilter_txx
#define __itkPointSetToImageFilter_txx

#include "itkPointSetToImageFilter.h"

#include "itkBoundingBox.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkNumericTraits.h"

namespace itk
{
/** Constructor */
template< class TInputPointSet, class TOutputImage >
PointSetToImageFilter< TInputPointSet, TOutputImage >
::PointSetToImageFilter()
{
  this->SetNumberOfRequiredInputs(1);
  m_Size.Fill(0);
  m_Origin.Fill(0.0);
  m_Spacing.Fill(1.0);
  m_Direction.SetIdentity();
  m_InsideValue = NumericTraits< ValueType >::OneValue();
  m_OutsideValue = NumericTraits< ValueType >::ZeroValue();
}

/** Destructor */
template< class TInputPointSet, class TOutputImage >
PointSetToImageFilter< TInputPointSet, TOutputImage >
::~PointSetToImageFilter()
{}

/** Set the Input PointSet */
template< class TInputPointSet, class TOutputImage >
void
PointSetToImageFilter< TInputPointSet, TOutputImage >
::SetInput(const InputPointSetType *input)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput( 0,
                                    const_cast< InputPointSetType * >( input ) );
}

/** Connect one of the operands  */
template< class TInputPointSet, class TOutputImage >
void
PointSetToImageFilter< TInputPointSet, TOutputImage >
::SetInput(unsigned int index, const TInputPointSet *pointset)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput( index,
                                    const_cast< TInputPointSet * >( pointset ) );
}

/** Get the input point-set */
template< class TInputPointSet, class TOutputImage >
const typename PointSetToImageFilter< TInputPointSet, TOutputImage >::InputPointSetType *
PointSetToImageFilter< TInputPointSet, TOutputImage >
::GetInput(void)
{
  if ( this->GetNumberOfInputs() < 1 )
    {
    return 0;
    }

  return static_cast< const TInputPointSet * >
         ( this->ProcessObject::GetInput(0) );
}

/** Get the input point-set */
template< class TInputPointSet, class TOutputImage >
const typename PointSetToImageFilter< TInputPointSet, TOutputImage >::InputPointSetType *
PointSetToImageFilter< TInputPointSet, TOutputImage >
::GetInput(unsigned int idx)
{
  return static_cast< const TInputPointSet * >
         ( this->ProcessObject::GetInput(idx) );
}

template< class TInputPointSet, class TOutputImage >
void
PointSetToImageFilter< TInputPointSet, TOutputImage >
::SetSpacing(const float *v)
{
  Vector< float, OutputImageDimension > vf(v);
  SpacingType                           spacing;
  spacing.CastFrom(vf);
  this->SetSpacing(spacing);
}

template< class TInputPointSet, class TOutputImage >
void
PointSetToImageFilter< TInputPointSet, TOutputImage >
::SetSpacing(const double *v)
{
  SpacingType spacing(v);

  this->SetSpacing(spacing);
}

template< class TInputPointSet, class TOutputImage >
void
PointSetToImageFilter< TInputPointSet, TOutputImage >
::SetOrigin(const float *v)
{
  Point< float, OutputImageDimension > pf(v);
  PointType                            origin;
  origin.CastFrom(pf);
  this->SetOrigin(origin);
}

template< class TInputPointSet, class TOutputImage >
void
PointSetToImageFilter< TInputPointSet, TOutputImage >
::SetOrigin(const double *v)
{
  PointType origin(v);

  this->SetOrigin(origin);
}

//----------------------------------------------------------------------------

/** Update */
template< class TInputPointSet, class TOutputImage >
void
PointSetToImageFilter< TInputPointSet, TOutputImage >
::GenerateData(void)
{
  unsigned int i;

  itkDebugMacro(<< "PointSetToImageFilter::Update() called");

  // Get the input and output pointers
  const InputPointSetType *InputPointSet  = this->GetInput();
  OutputImagePointer       OutputImage = this->GetOutput();

  // Generate the image
  double   origin[InputPointSetDimension];
  SizeType size;

  typedef BoundingBox<
    typename InputPointSetType::PointIdentifier,
    InputPointSetDimension,
    typename InputPointSetType::CoordRepType,
    typename InputPointSetType::PointsContainer>  BoundingBoxType;
  typename BoundingBoxType::Pointer bb = BoundingBoxType::New();
  bb->SetPoints( InputPointSet->GetPoints() );
  bb->ComputeBoundingBox();

  for ( i = 0; i < InputPointSetDimension; i++ )
    {
    size[i] = static_cast<SizeValueType>( bb->GetBounds()[2 * i + 1] - bb->GetBounds()[2 * i] );
    origin[i] = 0; //bb->GetBounds()[2*i];
    }

  typename OutputImageType::RegionType region;

  // If the size of the output has been explicitly specified, the filter
  // will set the output size to the explicit size, otherwise the size from the
  // spatial
  // PointSet's bounding box will be used as default.

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

  OutputImage->SetRegions(region);

  // If the spacing has been explicitly specified, the filter
  // will set the output spacing to that explicit spacing, otherwise the spacing
  // from
  // the point-set is used as default.

  specified = false;
  for ( i = 0; i < OutputImageDimension; i++ )
    {
    if ( m_Spacing[i] != 0 )
      {
      specified = true;
      break;
      }
    }

  if ( specified )
    {
    OutputImage->SetSpacing(this->m_Spacing);         // set spacing
    }

  specified = false;
  for ( i = 0; i < OutputImageDimension; i++ )
    {
    if ( m_Origin[i] != 0 )
      {
      specified = true;
      break;
      }
    }

  if ( specified )
    {
    for ( i = 0; i < OutputImageDimension; i++ )
      {
      origin[i] = m_Origin[i];         // set origin
      }
    }

  OutputImage->SetOrigin(origin);         //   and origin
  OutputImage->SetDirection(m_Direction); //   and Direction
  OutputImage->Allocate();                // allocate the image
  OutputImage->FillBuffer(m_OutsideValue);

  typedef typename InputPointSetType::PointsContainer::ConstIterator PointIterator;
  PointIterator pointItr = InputPointSet->GetPoints()->Begin();
  PointIterator pointEnd = InputPointSet->GetPoints()->End();

  typename OutputImageType::IndexType index;

  while ( pointItr != pointEnd )
    {
    if ( OutputImage->TransformPhysicalPointToIndex(pointItr.Value(), index) )
      {
      OutputImage->SetPixel(index, m_InsideValue);
      }
    pointItr++;
    }

  itkDebugMacro(<< "PointSetToImageFilter::Update() finished");
} // end update function

template< class TInputPointSet, class TOutputImage >
void
PointSetToImageFilter< TInputPointSet, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Size : " << m_Size << std::endl;
  os << indent << "Origin: " << m_Origin << std::endl;
  os << indent << "Spacing: " << m_Spacing << std::endl;
  os << indent << "Direction: " << m_Direction << std::endl;
  os << indent << "Inside Value : "
     << static_cast< typename NumericTraits< ValueType >::PrintType >( m_InsideValue )
     << std::endl;
  os << indent << "Outside Value : "
     << static_cast< typename NumericTraits< ValueType >::PrintType >( m_OutsideValue )
     << std::endl;
}
} // end namespace itk

#endif
