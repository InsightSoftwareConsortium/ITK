/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSliceIteratorTest.cxx
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
#include "itkImage.h"
#include "itkNeighborhood.h"
#include "itkSliceIterator.h"
#include "itkImageRegionIterator.h"
#include "itkSmartRegionNeighborhoodIterator.h"
#include <iostream>

template< class T, unsigned int N >
void FillRegionSequential(itk::SmartPointer< itk::Image<T, N> > I)
{
  unsigned int iDim, ArrayLength, i;
  itk::Size<N> Index;
  unsigned long int Location[N];
  unsigned int mult;
  T value;
  
  itk::ImageRegionIterator<itk::Image<T, N> > data(I, I->GetRequestedRegion());

  Index = (I->GetRequestedRegion()).GetSize();
  data.Begin();
  
  for (ArrayLength=1, iDim = 0; iDim<N; ++iDim)
	{
	  Location[iDim] =0;
	  ArrayLength*=Index[iDim];
	}
  
  for (i=0; i<ArrayLength; ++i, ++data)
	{
	  for (iDim=0, mult=1, value=0; iDim<N; ++iDim, mult*=10)
		{
		  value += mult *  Location[N-iDim-1];
		}
	  data.Set( value );
          
	  iDim = N-1;
          bool done=false;
          while(!done)
            {
            ++Location[iDim];
            if(Location[iDim] == Index[(N-1)-iDim])
              { Location[iDim] = 0; }
            else
              { done = true; }
            if(iDim == 0)
              { done = true; }
            else
              { --iDim; }
            }
	}
}

template< class T, unsigned int VDimension >
void PrintRegion(itk::SmartPointer< itk::Image<T, VDimension> > I)
{
  unsigned int iDim;
  long rsz[VDimension];
  long Location[VDimension];
  
  memcpy(rsz, I->GetRequestedRegion().GetSize().m_Size,
         sizeof(unsigned long) * VDimension);
  memset(Location, 0, sizeof(unsigned long) * VDimension); 
  for (iDim = 0; iDim < VDimension; ++iDim)
    {
      std::cout << "iDim = " << iDim << std::endl;
      std::cout << "\tRegionSize = "
           << I->GetRequestedRegion().GetSize().m_Size[iDim]
           << std::endl;
      std::cout << "\tRegionStartIndex = "
           << I->GetRequestedRegion().GetIndex()[iDim] << std::endl;
    }
  
  itk::ImageRegionIterator<itk::Image<T, VDimension> > iter( I, I->GetRequestedRegion());
    
  for (iter.Begin(); ! iter.IsAtEnd(); ++iter)
    {
      std::cout << iter.Get() << " ";

      iDim=VDimension-1;
      bool done=false;
      while(!done)
        {
        ++Location[iDim];
        if(Location[iDim]==rsz[(VDimension-1)-iDim])
          {
          std::cout << std::endl;
          Location[iDim]=0;
          }
        else
          { done = true; }
        if(iDim == 0)
          { done = true; }
        else
          { --iDim; }
        }
    }
}

template <class TContainer>
void PrintSlice(TContainer s)
{
  std::cout << "[" ;
  for (s=s.Begin(); s < s.End(); s++)
    {
      std::cout << *s << " ";
    }
  std::cout << "]" << std::endl;
  
}


int main()
{

  try {
    
      itk::ImageRegion<2> reg;
      itk::Size<2> hoodRadius;
      itk::Size<2> imgSize;
      itk::Index<2> zeroIndex;
      zeroIndex[0]=zeroIndex[1]=0;
      imgSize[0]=imgSize[1]=20;
      hoodRadius[0]=hoodRadius[1]=2;
      reg.SetIndex(zeroIndex);
      reg.SetSize(imgSize);
      
      std::slice hslice(10, 5, 1); // slice through the horizontal center
      std::slice vslice(2, 5, 5);  // slice through the vertical center
      itk::Neighborhood<int, 2> temp;
      itk::SliceIterator<int, itk::Neighborhood<int,2> > hnsi(&temp, hslice);
      itk::SliceIterator<int, itk::Neighborhood<int,2> > vnsi(&temp, vslice);
      itk::Neighborhood<int, 2> op;
      op.SetRadius(hoodRadius);
      
      itk::Index<2> idx;
      idx[0]=idx[1]=0;
      
      itk::Image<int, 2>::Pointer ip = itk::Image<int,2>::New();
      ip->SetRequestedRegion(reg);
      ip->SetBufferedRegion(reg);
      ip->SetLargestPossibleRegion(reg);
      ip->Allocate();
      
      FillRegionSequential<int, 2>(ip);
      PrintRegion<int,2>(ip);
      
      itk::SmartRegionNeighborhoodIterator<itk::Image<int,2> >
        it(hoodRadius, ip, reg);
      
      for (it = it.Begin(); !it.IsAtEnd(); ++it)
        {
          temp = it.GetNeighborhood();
          temp.Print(std::cout);
          PrintSlice(hnsi);
          PrintSlice(vnsi);
        }
  }
  catch (itk::ExceptionObject &err)
    {
      (&err)->Print(std::cerr);
      return 2;
    }
  return EXIT_SUCCESS;
}
