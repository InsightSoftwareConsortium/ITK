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
#ifndef itkLaplacianOperator_hxx
#define itkLaplacianOperator_hxx
#include "itkLaplacianOperator.h"

namespace itk
{
template< typename TPixel, unsigned int VDimension, typename TAllocator >
void
LaplacianOperator< TPixel, VDimension, TAllocator >
::SetDerivativeScalings(const double *s)
{
  for ( unsigned int i = 0; i < VDimension; ++i )
    {
    m_DerivativeScalings[i] = s[i];
    }
}

//Create the operator
template< typename TPixel, unsigned int VDimension, typename TAllocator >
void
LaplacianOperator< TPixel, VDimension, TAllocator >
::CreateOperator()
{
  CoefficientVector coefficients;

  coefficients = this->GenerateCoefficients();

  this->Fill(coefficients);
}

//This function fills the coefficients into the corresponding neighborhodd.
template< typename TPixel, unsigned int VDimension, typename TAllocator >
void
LaplacianOperator< TPixel, VDimension, TAllocator >
::Fill(const CoefficientVector & coeff)
{
  typename Superclass::CoefficientVector::const_iterator it;

  std::slice *temp_slice;
  temp_slice = new std::slice(0, coeff.size(), 1);

  typename Self::SliceIteratorType data(this, *temp_slice);
  delete temp_slice;

  it = coeff.begin();

  // Copy the coefficients into the neighborhood
  for ( data = data.Begin(); data < data.End(); ++data, ++it )
    {
    *data = *it;
    }
}

template< typename TPixel, unsigned int VDimension, typename TAllocator >
typename LaplacianOperator< TPixel, VDimension, TAllocator >
::CoefficientVector
LaplacianOperator< TPixel, VDimension, TAllocator >
::GenerateCoefficients()
{
  unsigned int i, w;

  // Here we set the radius to 1's, here the
  // operator is 3x3 for 2D, 3x3x3 for 3D.
  SizeType r;

  r.Fill(1);
  this->SetRadius(r);

  // Create a vector of the correct size to hold the coefficients.
  w = this->Size();
  CoefficientVector coeffP(w);

  //Set the coefficients
  double   sum = 0.0;
  for ( i = 0; i < 2 * VDimension; i += 2 )
    {
    OffsetValueType stride = this->GetStride(i / 2);

    const double   hsq = m_DerivativeScalings[i / 2] * m_DerivativeScalings[i / 2];
    coeffP[w / 2 - stride] =  coeffP[w / 2 + stride] = hsq;
    sum += 2.0 * hsq;
    }
  coeffP[w / 2] = -sum;

  return coeffP;
}
} // namespace itk

#endif
