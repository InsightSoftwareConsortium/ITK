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
#ifndef itkNeighborhoodOperator_hxx
#define itkNeighborhoodOperator_hxx

#include "itkNeighborhoodOperator.h"
#include "itkIntTypes.h"

namespace itk
{
template< typename TPixel, unsigned int VDimension, typename TAllocator >
void
NeighborhoodOperator< TPixel, VDimension, TAllocator >
::ScaleCoefficients(PixelRealType s)
{
  for ( unsigned i = 0; i < this->Size(); i++ )
    {
    this->operator[](i) = static_cast< TPixel >( this->operator[](i) * s );
    }
}

template< typename TPixel, unsigned int VDimension, typename TAllocator >
void
NeighborhoodOperator< TPixel, VDimension, TAllocator >
::FlipAxes()
{
  // To flip the operator across all of its axes, all we have to do is reverse
  // the order of all coefficients.
  const unsigned size = this->Size();
  PixelType      temp;

  for ( unsigned i = 0; i < size / 2; ++i )
    {
    unsigned swap_with = size - 1 - i;
    temp = this->operator[](i);

    this->operator[](i) = this->operator[](swap_with);

    this->operator[](swap_with) = temp;
    }
}

template< typename TPixel, unsigned int VDimension, typename TAllocator >
void
NeighborhoodOperator< TPixel, VDimension, TAllocator >
::CreateDirectional()
{
  SizeValueType     k[VDimension];
  CoefficientVector coefficients;

  coefficients = this->GenerateCoefficients();
  for ( unsigned int i = 0; i < VDimension; ++i )
    {
    if ( i == this->GetDirection() )
      {
      k[i] = static_cast< SizeValueType >( coefficients.size() ) >> 1;
      }
    else
      {
      k[i] = 0;
      }
    }
  this->SetRadius(k);
  this->Fill(coefficients);
}

template< typename TPixel, unsigned int VDimension, typename TAllocator >
void
NeighborhoodOperator< TPixel, VDimension, TAllocator >
::CreateToRadius(const SizeType & sz)
{
  CoefficientVector coefficients;

  coefficients = this->GenerateCoefficients();
  this->SetRadius(sz);
  this->Fill(coefficients);
}

template< typename TPixel, unsigned int VDimension, typename TAllocator >
void
NeighborhoodOperator< TPixel, VDimension, TAllocator >
::CreateToRadius(const SizeValueType sz)
{
  SizeType k;

  for ( unsigned int i = 0; i < VDimension; i++ )
    {
    k[i] = sz;
    }
  this->CreateToRadius(k);
}

template< typename TPixel, unsigned int VDimension, typename TAllocator >
void
NeighborhoodOperator< TPixel, VDimension, TAllocator >
::FillCenteredDirectional(const CoefficientVector & coeff)
{
  // Initialize all coefficients to zero
  this->InitializeToZero();

  // Collect slice information
  unsigned long start=0;
  const unsigned long stride = this->GetStride(m_Direction);
  const unsigned long size   = this->GetSize(m_Direction);
  for ( unsigned int i = 0; i < VDimension; ++i )
    {
    if ( i != m_Direction )
      {
      start += this->GetStride(i) * ( this->GetSize(i) >> 1 );
      }
    }

  // Compare the neighborhood size with the coefficient array size..
  const int sizediff = ( (int)size - (int)coeff.size() ) >> 1;

  // Create a slice iterator centered in the neighborhood.
  std::slice *                      temp_slice;
  typename CoefficientVector::const_iterator it;
  if ( sizediff >= 0 )
    {
    temp_slice = new std::slice(start + sizediff * stride, coeff.size(),
                                stride);
    it = coeff.begin();
    }
  else
    {
    temp_slice = new std::slice(start, size, stride);
    it = coeff.begin() - sizediff;
    }

  SliceIteratorType data(this, *temp_slice);
  delete temp_slice;

  // Copy the coefficients into the neighborhood, truncating them if there
  // are too many.
  for ( data = data.Begin(); data < data.End(); ++data, ++it )
    {
    *data = static_cast< TPixel >( *it );
    }
}
} // namespace itk

#endif
