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

template <class TPixel, unsigned int VDimension, class TAllocator>
void  
SobelOperator <TPixel, VDimension, TAllocator>
::Fill(const CoefficientVector &coeff)
{
  this->InitializeToZero();
  
  // Note that this code is only good for 2d and 3d operators.  Places the
  // coefficients in the exact center of the neighborhood
  unsigned int i;
  int x,y,z, pos;
  unsigned int center = this->GetCenterNeighborhoodIndex();

  if (VDimension == 3)
    {
      i = 0;
      for (z = -1; z <= 1; z++)
        {
          for (y = -1; y <= 1; y++ )
            {
              for (x = -1; x <= 1; x++)
                {
                  pos = center + z * this->GetStride(2) + y * this->GetStride(1) +
                    x * this->GetStride(0);
                  this->operator[](pos) = coeff[i];
                  i++;
                }
            }
        }
    }
  else if (VDimension == 2)
    {
      i = 0;
      for (y = -1; y <= 1; y++ )
        {
          for (x = -1; x <= 1; x++)
            {
              pos = center + y * this->GetStride(1) + x * this->GetStride(0);
              this->operator[](pos) = coeff[i];
              i++;
            }
        } 
    }
  else
    {
      itkExceptionMacro( << "The ND version of the Sobel operator is not yet implemented.  Currently only the 2D and 3D versions are available." );
    }
  
}
  
template <class TPixel, unsigned int VDimension, class TAllocator>
typename SobelOperator<TPixel, VDimension, TAllocator>
::CoefficientVector
SobelOperator<TPixel, VDimension, TAllocator>
::GenerateCoefficients()
{
  unsigned i, j;
  std::vector<double> coeff;
  if (VDimension == 2 && this->GetDirection() == 0)
    {
      coeff.push_back(-1.0);  coeff.push_back(0.0);  coeff.push_back(1.0);
      coeff.push_back(-2.0);  coeff.push_back(0.0);  coeff.push_back(2);
      coeff.push_back(-1.0);  coeff.push_back(0.0);  coeff.push_back(1.0);
    }
  else if (VDimension == 2 && this->GetDirection() == 1)
    {
      coeff.push_back(-1.0);  coeff.push_back(-2);  coeff.push_back(-1.0);
      coeff.push_back(0.0);  coeff.push_back(0.0);  coeff.push_back(0.0);
      coeff.push_back(1.0);  coeff.push_back(2);  coeff.push_back(1.0);
    }
  else if (VDimension == 3 && this->GetDirection() == 0)
    {
      coeff.push_back(-1.0);  coeff.push_back(0.0);  coeff.push_back(1.0);
      coeff.push_back(-3.0);  coeff.push_back(0.0);  coeff.push_back(3.0);
      coeff.push_back(-1.0);  coeff.push_back(0.0);  coeff.push_back(1.0);

      coeff.push_back(-3.0);  coeff.push_back(0.0);  coeff.push_back(3.0);
      coeff.push_back(-6.0);  coeff.push_back(0.0);  coeff.push_back(6.0);
      coeff.push_back(-3.0);  coeff.push_back(0.0);  coeff.push_back(3.0);

      coeff.push_back(-1.0);  coeff.push_back(0.0);  coeff.push_back(1.0);
      coeff.push_back(-3.0);  coeff.push_back(0.0);  coeff.push_back(3.0);
      coeff.push_back(-1.0);  coeff.push_back(0.0);  coeff.push_back(1.0);
    }
  else if (VDimension == 3 && this->GetDirection() == 1)
    {
      coeff.push_back(-1.0);  coeff.push_back(-3.0);  coeff.push_back(-1.0);
      coeff.push_back(0.0);  coeff.push_back(0.0);  coeff.push_back(0.0);
      coeff.push_back(1.0);  coeff.push_back(3.0);  coeff.push_back(1.0);
      
      coeff.push_back(-3.0);  coeff.push_back(-6.0);  coeff.push_back(-3.0);
      coeff.push_back(0.0);  coeff.push_back(0.0);  coeff.push_back(0.0);
      coeff.push_back(3.0);  coeff.push_back(6.0);  coeff.push_back(3.0);
      
      coeff.push_back(-1.0);  coeff.push_back(-3.0);  coeff.push_back(-1.0);
      coeff.push_back(0.0);  coeff.push_back(0.0);  coeff.push_back(0.0);
      coeff.push_back(1.0);  coeff.push_back(3.0);  coeff.push_back(1.0);
    }
  else if (VDimension == 3 && this->GetDirection() == 2)
    {
      coeff.push_back(-1.0);  coeff.push_back(-3.0);  coeff.push_back(-1.0);
      coeff.push_back(-3.0);  coeff.push_back(-6.0);  coeff.push_back(-3.0);
      coeff.push_back(-1.0);  coeff.push_back(-3.0);  coeff.push_back(-1.0);
      
      coeff.push_back(0.0);  coeff.push_back(0.0);  coeff.push_back(0.0);
      coeff.push_back(0.0);  coeff.push_back(0.0);  coeff.push_back(0.0);
      coeff.push_back(0.0);  coeff.push_back(0.0);  coeff.push_back(0.0);
      
      coeff.push_back(1.0);  coeff.push_back(3.0);  coeff.push_back(1.0);
      coeff.push_back(3.0);  coeff.push_back(6.0);  coeff.push_back(3.0);
      coeff.push_back(1.0);  coeff.push_back(3.0);  coeff.push_back(1.0);
    }
  else
    {
      itkExceptionMacro( << "The ND version of the Sobel operator has not been implemented.  Currently only 2D and 3D versions are available." );
    }
  
  return coeff;
}

} // namespace itk

#endif
