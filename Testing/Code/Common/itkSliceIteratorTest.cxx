/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSliceIteratorTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

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
  unsigned int iTemp, ArrayLength, iDim,i;
  itk::Size<N> Index;
  unsigned long int Location[N];
  unsigned int mult;
  T value;
  
  itk::ImageRegionIterator<T, N> data(I, I->GetRequestedRegion());

  Index = (I->GetRequestedRegion()).GetSize();
  data.Begin();
  
  for (ArrayLength=1, iTemp = 0; iTemp<N; iTemp++)
	{
	  Location[iTemp] =0;
	  ArrayLength*=Index[iTemp];
	}
  
  for (i=0; i<ArrayLength; i++, ++data)
	{
	  for (iDim=0, mult=1, value=0; iDim<N; iDim++, mult*=10)
		{
		  value += mult *  Location[N-iDim-1];
		}
	  *data = value;
	  
	  for (iDim=N-1; iDim>=0; iDim--)
		{
		  Location[iDim]++;
		  if (Location[iDim]==Index[(N-1)-iDim]) Location[iDim]=0;
		  else break;
		}
	  
	}
}

template< class T, unsigned int TDimension >
void PrintRegion(itk::SmartPointer< itk::Image<T, TDimension> > I)
{
  int iDim, i, ArrayLength;
  int nnf[TDimension];
  unsigned long rsz[TDimension];
  unsigned long Location[TDimension];
  
  memcpy(rsz, I->GetRequestedRegion().GetSize().m_Size,
         sizeof(unsigned long) * TDimension);
  memset(Location, 0, sizeof(unsigned long) * TDimension); 
  for (iDim = 0; iDim < TDimension; ++iDim)
    {
      std::cout << "iDim = " << iDim << std::endl;
      std::cout << "\tRegionSize = "
           << I->GetRequestedRegion().GetSize().m_Size[iDim]
           << std::endl;
      std::cout << "\tRegionStartIndex = "
           << I->GetRequestedRegion().GetIndex()[iDim] << std::endl;
    }
  
  itk::ImageRegionIterator<T, TDimension> iter( I, I->GetRequestedRegion());
    
  for (iter.Begin(); ! iter.IsAtEnd(); ++iter)
    {
      std::cout << *iter << " ";
      
      for (iDim=TDimension-1; iDim>=0; iDim--)
		{
		  Location[iDim]++;
		  if (Location[iDim]==rsz[TDimension-1 -iDim])
            {
              std::cout << std::endl;
              Location[iDim]=0;
            }
		  else break;
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


main()
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
      op = 2;
      op.Print();
      
      itk::Index<2> idx;
      idx[0]=idx[1]=0;
      
      itk::Image<int, 2>::Pointer ip = itk::Image<int,2>::New();
      ip->SetRequestedRegion(reg);
      ip->SetBufferedRegion(reg);
      ip->SetLargestPossibleRegion(reg);
      ip->Allocate();
      
      std::slice islice(9, 20, 20);  // slice the image down the middle
      itk::SliceIterator<int, itk::Image<int, 2> > imagesi(ip, islice);
      // we can't do anything with it though, since image::operator[] is not
      // defined
      
      FillRegionSequential<int, 2>(ip);
      PrintRegion<int,2>(ip);
      
      itk::SmartRegionNeighborhoodIterator<int,2> it(hoodRadius, ip, reg);
      const itk::SmartRegionNeighborhoodIterator<int,2> itEnd = it.End();
      
      for (it = it.Begin(); it < itEnd; ++it)
        {
          temp = it.GetNeighborhood();
          temp.Print();
          PrintSlice(hnsi);
          PrintSlice(vnsi);
          std::cout <<  temp.InnerProduct(op) << std::endl;
          std::cout <<  it.InnerProduct(op)   << std::endl;
          
        }
  }
  catch (itk::ExceptionObject &err)
    {
      (&err)->Print(std::cerr);
      return 2;
    }
  return 1;
}
