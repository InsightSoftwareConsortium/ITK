/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSobelOperator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkSobelOperator_txx
#define _itkSobelOperator_txx

#include "itkSobelOperator.h"
#include "itkObject.h"

namespace itk
{

  //Create the operator
template <class TPixel, unsigned int VDimension, class TAllocator>
void
SobelOperator<TPixel, VDimension, TAllocator>
::CreateOperator()
{
  CoefficientVector coefficients;
  
  coefficients = this->GenerateCoefficients();

  this->Fill(coefficients);
  
}

 //This function fills the coefficients into the corresponding neighborhodd.
template <class TPixel, unsigned int VDimension, class TAllocator>
void  
SobelOperator <TPixel, VDimension, TAllocator>
::Fill(const CoefficientVector &coeff)
{

  typename CoefficientVector::const_iterator it;

  std::slice* temp_slice;
  temp_slice = new std::slice(0, coeff.size(),1);
  
  typename Self::SliceIteratorType data(this, *temp_slice);
  delete temp_slice;
 
  it = coeff.begin();

  // Copy the coefficients into the neighborhood
  for (data = data.Begin(); data < data.End(); ++data, ++it)
    {
      *data = *it;
    }

}



template <class TPixel, unsigned int VDimension, class TAllocator>
typename SobelOperator<TPixel, VDimension, TAllocator>
::CoefficientVector
SobelOperator<TPixel, VDimension, TAllocator>
::GenerateCoefficients()
{
  unsigned int i;

  unsigned int w = 1;

  for(i = 0; i < VDimension; i ++)
    {
      w = w*3;
    }

  std::vector<PixelType> coeff(w);
  CoefficientVector coeffP(w);

  // Here we set the radius to 1's, here the
  // operator is 3x3 for 2D, 3x3x3 for 3D.

  unsigned long k[VDimension];
  
  if(VDimension < 2)
    {
    itkGenericOutputMacro(<<"Dimension must be larger than 1 !");
    }

  k[0] = 1;
  k[1] = 1;

  unsigned int direction = this->GetDirection();
  
  for ( i = 2; i < VDimension; i++)
    {
      if(i == direction)
        k[i] = 1;
      else
        k[i] = 0;
    }

  this->SetRadius(k);
  
  //calculate offset
  unsigned int offset[6];

  offset[0] = this->GetStride(direction);
  offset[1] = - this->GetStride(direction);

  if ( direction == 0)
    {
      offset[2] = this->GetStride(direction) + this->GetStride(direction +1);
      offset[3] =  this->GetStride(direction) - this->GetStride(direction +1) ;
      offset[4] = - this->GetStride(direction) - this->GetStride(direction +1);
      offset[5] = - this->GetStride(direction) + this->GetStride(direction +1) ;
    }
  else
    {
      offset[2] = this->GetStride(direction) + this->GetStride(direction -1);
      offset[3] = this->GetStride(direction) - this->GetStride(direction -1) ;
      offset[4] = this->GetStride(direction) - this->GetStride(direction -1);
      offset[5] = - this->GetStride(direction) + this->GetStride(direction -1) ;
    }

  coeff[w/2 + offset[0]] = 2.0;
  coeff[w/2 + offset[1]] = -2.0;
  coeff[w/2 + offset[2]] = 1.0;
  coeff[w/2 + offset[3]] = 1.0;
  coeff[w/2 + offset[4]] = -1.0;
  coeff[w/2 + offset[5]] = -1.0;

  for ( i = 0; i < w; i ++)
    {
      coeffP[i] = coeff[i]/4.0f;
    }

 
    return coeffP;
    
}

} // namespace itk

#endif
