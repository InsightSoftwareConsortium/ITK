/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhood.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

  =========================================================================*/
#include <math.h>
#include <iostream>

namespace itk {

template<class TPixel, unsigned int VDimension>
void
Neighborhood<TPixel, VDimension>
::PrintSelf()
{
  NeighborhoodBase<TPixel, VDimension>::PrintSelf();
  std::cout << "Neighborhood" << std::endl;
  std::cout << "        this = " << this << std::endl;
  std::cout << "  this->size = " << this->size() << std::endl;
  
  //   this->PrintScalarData();
}

template<class TPixel, unsigned int VDimension>
void
Neighborhood<TPixel, VDimension>
::PrintScalarData()
{
  unsigned int iDim;
  Iterator iter;
  unsigned long loop[VDimension];
  memset(loop, 0, sizeof(unsigned long) * VDimension);
  
  for (iter = Begin(); iter < End(); ++iter)
    {
      //std::cout << *iter << " ";
      for (iDim = 0; iDim < VDimension; ++iDim)
        {
          loop[iDim]++;
          if (loop[iDim] == this->GetSize(iDim))
            {
              loop[iDim] = 0;
              std::cout << std::endl;
            }
          else break;
        }
    }
}
  
} // namespace itk

