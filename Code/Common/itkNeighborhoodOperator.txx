/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodOperator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef _itkNeighborhoodOperator_txx
#define _itkNeighborhoodOperator_txx
namespace itk
{

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
          k[i] = coefficients.size() >> 1;
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
      *data = *it;
    }
}

}// namespace itk

#endif
