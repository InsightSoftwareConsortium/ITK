/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodOperator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

  =========================================================================*/
namespace itk
{

template<class TPixel, unsigned int VDimension>
void
NeighborhoodOperator<TPixel, VDimension>
::CreateDirectional()
{
  unsigned long k[VDimension];
  std::vector<TPixel> coefficients;

  coefficients = this->GenerateCoefficients();
  for (int i = 0; i<VDimension; ++i)
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
  
template<class TPixel, unsigned int VDimension>
void
NeighborhoodOperator<TPixel, VDimension>
::CreateToRadius(const unsigned long *sz)
{
  std::vector<TPixel> coefficients;
  coefficients = this->GenerateCoefficients();
  this->SetRadius(sz);
  this->Fill(coefficients);
}

template<class TPixel, unsigned int VDimension>
void
NeighborhoodOperator<TPixel, VDimension>
::CreateToRadius(const unsigned long &sz)
{
  unsigned long k[VDimension];
  for (int i = 0; i< VDimension; i++)
    {
      k[i] = sz;
    }
  this->CreateToRadius(k);
}

template<class TPixel, unsigned int VDimension>
void
NeighborhoodOperator<TPixel, VDimension>
::FillCenteredDirectional(const std::vector<TPixel> &coeff)
{
  int i;
  unsigned long start;
  unsigned long len;
  std::slice* temp_slice;
  std::vector<float>::const_iterator it;

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
  typename Self::SliceIteratorType data(this, *temp_slice);
  delete temp_slice;

  // Copy the coefficients into the neighborhood, truncating them if there
  // are too many.
  for (data = data.Begin(); data < data.End(); ++data, ++it)
    {
      *data = *it;
    }
}

}// namespace itk
