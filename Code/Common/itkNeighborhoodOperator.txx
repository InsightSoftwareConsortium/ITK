/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodOperator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkNeighborhoodOperator_txx
#define _itkNeighborhoodOperator_txx
namespace itk
{
template <class TPixel, unsigned int VDimension, class TAllocator>
void
NeighborhoodOperator<TPixel, VDimension, TAllocator>
::ScaleCoefficients( TPixel s )
{
  for (unsigned i = 0; i < this->Size(); i++)
    {
    this->operator[](i) = this->operator[](i) * s;
    }
}

template <class TPixel, unsigned int VDimension, class TAllocator>
void
NeighborhoodOperator<TPixel, VDimension, TAllocator>
::FlipAxes()
{
  // To flip the operator across all of its axes, all we have to do is reverse
  // the order of all coefficients.
  const unsigned size = this->Size();
  unsigned i, swap_with;
  PixelType temp;

  for (i = 0; i < size/2; ++i)
    {
      swap_with = size - 1 - i;
      temp = this->operator[](i);
      this->operator[](i) = this->operator[](swap_with);
      this->operator[](swap_with) = temp;
    }
}

  
template <class TPixel, unsigned int VDimension, class TAllocator>
void
NeighborhoodOperator<TPixel, VDimension, TAllocator>
::CreateDirectional()
{
  unsigned long k[VDimension];
  CoefficientVector coefficients;

  coefficients = this->GenerateCoefficients();
  for (unsigned int i = 0; i<VDimension; ++i)
    {
      if (i == this->GetDirection())
        {
          k[i] = static_cast<unsigned long>( coefficients.size() ) >> 1;
        }
      else
        {
          k[i] = 0;
        }
    }
  this->SetRadius(k);
  this->Fill(coefficients);
}
  
template <class TPixel, unsigned int VDimension, class TAllocator>
void
NeighborhoodOperator<TPixel, VDimension, TAllocator>
::CreateToRadius(const SizeType &sz)
{
  CoefficientVector coefficients;
  coefficients = this->GenerateCoefficients();
  this->SetRadius(sz);
  this->Fill(coefficients);
}

template <class TPixel, unsigned int VDimension, class TAllocator>
void
NeighborhoodOperator<TPixel, VDimension, TAllocator>
::CreateToRadius(const unsigned long sz)
{
  SizeType k;
  for (unsigned int i = 0; i< VDimension; i++)
    {
      k[i] = sz;
    }
  this->CreateToRadius(k);
}

template <class TPixel, unsigned int VDimension, class TAllocator>
void
NeighborhoodOperator<TPixel, VDimension, TAllocator>
::FillCenteredDirectional(const CoefficientVector &coeff)
{
  unsigned int i;
  unsigned long start;
  std::slice* temp_slice;
  CoefficientVector::const_iterator it;

  // Initialize all coefficients to zero
  this->InitializeToZero();
  
  // Collect slice information
  const unsigned long stride = this->GetStride(m_Direction);
  const unsigned long size   = this->GetSize(m_Direction);
  for (i = 0, start = 0; i <VDimension; ++i)
    {
      if (i != m_Direction)
        {
          start += this->GetStride(i) * (this->GetSize(i) >> 1);
        }
    }
    
  // Compare the neighborhood size with the coefficient array size..
  const int sizediff = ( (int)size - (int)coeff.size() ) >>1;
 
  // Create a slice iterator centered in the neighborhood.
  if (sizediff >= 0)
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

  Self::SliceIteratorType data(this, *temp_slice);
  delete temp_slice;

  // Copy the coefficients into the neighborhood, truncating them if there
  // are too many.
  for (data = data.Begin(); data < data.End(); ++data, ++it)
    {
      *data = static_cast<TPixel>(*it);
    }
}

}// namespace itk

#endif
