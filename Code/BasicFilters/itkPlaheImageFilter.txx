/*=========================================================================
  
  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPlaheImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _itkPlaheImageFilter_txx
#define _itkPlaheImageFilter_txx

#include <map>

#include "vnl/vnl_math.h"

#include "itkPlaheImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkSimpleImageRegionIterator.h"

namespace itk
{

template <class TPixel, unsigned int VImageDimension>
float
PlaheImageFilter<TPixel, VImageDimension>
::CumulativeFunction(float u, float v)
{
  // Calculate cumulative function
  return 0.5*vnl_math_sgn(u-v)*pow(abs(2*(u-v)),m_Alpha) + m_Beta*v;
}

template <class TPixel, unsigned int VImageDimension>
void
PlaheImageFilter<TPixel, VImageDimension>
::GenerateData()
{
  
  typedef Image<TPixel, VImageDimension> ImageType;
  ImageType::Pointer input = this->GetInput();
  ImageType::Pointer output = this->GetOutput();
  
  output->SetBufferedRegion(input->GetBufferedRegion());
  output->SetRequestedRegion(input->GetRequestedRegion() );
  output->SetLargestPossibleRegion(input->GetLargestPossibleRegion());
  output->Allocate();
  
  int i;

  //Set the kernel value of PLAHE algorithm
  float kernel = 1;
  for (i = 0; i < VImageDimension; i++)
    {
    kernel = kernel * (this->GetWindow())[i];
    }
  kernel = 1/kernel;

  //Set Iterator which traverse whole image
  ImageType::RegionType region;
  ImageType::IndexType index;
  ImageType::SizeType size;
  region = input->GetRequestedRegion();
  for ( i = 0; i < VImageDimension; i++)
    {
    index[i] = 0;
    size[i]  = input->GetBufferedRegion().GetSize()[i];
    }
  region.SetIndex(index);
  region.SetSize(size);
  ImageRegionIterator<TPixel,VImageDimension> itInput(input, region);

  // Calculate min and max gray level of an input image
  itInput = itInput.Begin();
  double min = *itInput;
  double max = *itInput;
  while( !itInput.IsAtEnd() )
    {
    if ( min > *itInput )
      min = *itInput;
    if ( max < *itInput )
      max = *itInput;
    ++itInput;
    }

  
  // Allocate a float type image which has the same size with an input image.
  // This image store normalized pixel values [-0.5 0.5] of the input image.
  typedef Image<float, VImageDimension> ImageFloatType;
  ImageFloatType::Pointer inputFloat = ImageFloatType::New();
  inputFloat->SetBufferedRegion(region);
  inputFloat->SetRequestedRegion( region );
  inputFloat->SetLargestPossibleRegion( region );
  inputFloat->Allocate();

  // iterator which traverse the float type image
  ImageRegionIterator<float, VImageDimension> itFloat(inputFloat, region); 

  float iscale = max - min;
  float scale = (float)1/iscale;
  
  // Normalize input image to [-0.5 0.5] gray level and store in inputFloat
  // Plahe only use float type image which has gray range [-0.5 0.5] 
  itFloat = itFloat.Begin();
  itInput = itInput.Begin();
  while( !itInput.IsAtEnd() )
    {
    *itFloat = scale*(max - *itInput)-0.5;
    ++itFloat;
    ++itInput;
    }
    
  // Calculate cumulative array which will store the value of cumulative function 
  // During the Plahe process, cumulative function will not be calcuated, instead 
  // the cumulative function will be calculated and result will be stored in 
  // cumulative array.  Then cumulative array will be referenced during processing.
  // This pre-calculation reduce computation time even though this method use huge 
  // array.  If the cumulative array can not be assigned, the cumulative function
  // will be calculated each time.
  typedef std::map<float, float> FloatFloatMapType;
  FloatFloatMapType row;
  itFloat = itFloat.Begin();
  while ( !itFloat.IsAtEnd() )
    {
    row.insert( FloatFloatMapType::value_type(*itFloat,0 ) );
    ++itFloat;
    }
  typedef std::map < float, FloatFloatMapType > ArrayMapType;
  ArrayMapType CumulativeArray;
  std::pair<ArrayMapType::iterator, bool> array;
  itFloat = itFloat.Begin();
  while ( !itFloat.IsAtEnd() )
    {
    array = CumulativeArray.insert( ArrayMapType::value_type(*itFloat,row ) );
    // if CumulativeArray is too big to assign, stop assigning
    // the cumulative function will be used to evaluate pixels
    if ( !array.second )
      {
      break;
      }
    ++itFloat;
    }    

  if ( array.second )
    {
    // if cumulative array is properly assigned,
    // calculate cumulative function and store result in cumulative array
    ArrayMapType::iterator itU;
    FloatFloatMapType::iterator itV;
    itU = CumulativeArray.begin();
    while ( itU != CumulativeArray.end() )
      {
      itV = row.begin();
      while ( itV != row.end() )
        {
        CumulativeArray[(*itU).first][(*itV).first] 
          = CumulativeFunction((*itU).first, (*itV).first);
        itV++;
        }
      itU++;
      }
    }
  else
    {
    // if cumulative array is not properly assigned, remove data from
    // cumulative array
    row.clear();
    CumulativeArray.clear();
    }


  int radius[VImageDimension];
  for ( i = 0; i < VImageDimension; i++)
    {
    // Radius of window (neighborhood of the evaluated pixel)
    radius[i] = (long)(m_Window[i]-1)/2;
    index[i] = radius[i];

    // Size of region of interest in the image
    // On the boundary area, the range of the window exceeds the
    // range of the image. So, iterator will not traverse boundary area  
    size[i] = input->GetBufferedRegion().GetSize()[i] - 2*radius[i];
    }
  region.SetIndex(index);
  region.SetSize(size);
  
  // Iterators which will travel the center area, not boundary of the image.
  // "it" is for input image and itOut is for output image
  SimpleImageRegionIterator<ImageFloatType> it(inputFloat, region);
  ImageRegionIterator<TPixel, VImageDimension> itOut(output, region);
 
  // Assign the size of the window
  float sum;
  for ( i = 0; i < VImageDimension; i++)
    {
    size[i] = 2*radius[i] + 1;
    }
  
  // Map stores (number of pixel)/(window size) for each gray value. 
  typedef std::map<float, float> MapType;
  MapType count;
  MapType::iterator itMap;
  
  // Plahe algorithm
  it.Begin();
  itOut.Begin();
  float f;
  int cnt = 0;
  while( !it.IsAtEnd() )
    {
    count.clear();
    // Assign start index of the window
    for ( i = 0; i < VImageDimension; i++)
      {
      index[i] = it.GetIndex()[i] - radius[i];
      }
    region.SetIndex(index);
    region.SetSize(size);
    ImageRegionIterator<float, VImageDimension> itWin(inputFloat, region);
    itWin.Begin();

    while ( !itWin.IsAtEnd() )
      {
      itMap = count.find(*itWin);
      if ( itMap != count.end() )
        {
        count[*itWin] = count[*itWin] + kernel;
        }
      else
        {
        count.insert(MapType::value_type(*itWin,kernel));
        }

      ++itWin;
      }

    
    // if CumulativeArray is properly assign, use it, 
    // if not, use CumulativeFunction()
    if (array.second)
      {
      sum = 0;
      itMap = count.begin();
      f = it.Get();
      while ( itMap != count.end()  )
        {
        sum = sum + itMap->second*CumulativeArray[f][itMap->first];
        ++itMap;
        }
      *itOut = (TPixel)(iscale*(sum+0.5) + min);
      ++itOut;
      ++it;
      }
    else
      {
      sum = 0;
      itMap = count.begin();
      f = it.Get();
      while ( itMap != count.end()  )
        {
        sum = sum + itMap->second*CumulativeFunction(f,itMap->first);
        ++itMap;
        }
      *itOut = (TPixel)(iscale*(sum+0.5) + min);
      ++itOut;
      ++it;
      }
    }
    
}

} // end namespace


#endif
