/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPlaheImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkPlaheImageFilter_txx
#define _itkPlaheImageFilter_txx

#include <map>

#include "vnl/vnl_math.h"

#include "itkPlaheImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionConstIterator.h"
#include "itkImageRegionIteratorWithIndex.h"

namespace itk
{

template <class TImageType>
float
PlaheImageFilter<TImageType>
::CumulativeFunction(float u, float v)
{
  // Calculate cumulative function
  return 0.5*vnl_math_sgn(u-v)*pow(vnl_math_abs(2*(u-v)),m_Alpha) + m_Beta*v;
}

template <class TImageType>
void
PlaheImageFilter<TImageType>
::GenerateData()
{
  
  typedef TImageType ImageType;
  typename ImageType::ConstPointer input = this->GetInput();
  typename ImageType::Pointer output = this->GetOutput();
  
  output->SetBufferedRegion(input->GetBufferedRegion());
  output->SetRequestedRegion(input->GetRequestedRegion() );
  output->SetLargestPossibleRegion(input->GetLargestPossibleRegion());
  output->Allocate();
  
  unsigned int i;

  //Set the kernel value of PLAHE algorithm
  float kernel = 1;
  for (i = 0; i < ImageDimension; i++)
    {
    kernel = kernel * (this->GetWindow())[i];
    }
  kernel = 1/kernel;

  //Set Iterator which traverse whole image
  typename ImageType::RegionType region;
  typename ImageType::IndexType index;
  typename ImageType::SizeType size;
  region = input->GetRequestedRegion();
  for ( i = 0; i < ImageDimension; i++)
    {
    index[i] = 0;
    size[i]  = input->GetBufferedRegion().GetSize()[i];
    }
  region.SetIndex(index);
  region.SetSize(size);
  ImageRegionConstIterator<ImageType> itInput(input, region);

  // Calculate min and max gray level of an input image
  double min = itInput.Get();
  double max = itInput.Get();
  while( !itInput.IsAtEnd() )
    {
    if ( min > itInput.Get() )
      min = itInput.Get();
    if ( max < itInput.Get() )
      max = itInput.Get();
    ++itInput;
    }

  
  // Allocate a float type image which has the same size with an input image.
  // This image store normalized pixel values [-0.5 0.5] of the input image.
  typedef Image<float, ImageDimension> ImageFloatType;
  typename ImageFloatType::Pointer inputFloat = ImageFloatType::New();
  inputFloat->SetBufferedRegion(region);
  inputFloat->SetRequestedRegion( region );
  inputFloat->SetLargestPossibleRegion( region );
  inputFloat->Allocate();

  // iterator which traverse the float type image
  ImageRegionIterator<ImageType> itFloat(inputFloat, region); 

  float iscale = max - min;
  float scale = (float)1/iscale;
  
  // Normalize input image to [-0.5 0.5] gray level and store in inputFloat
  // Plahe only use float type image which has gray range [-0.5 0.5] 
  itInput.GoToBegin();
  while( !itInput.IsAtEnd() )
    {
    itFloat.Set( scale*(max - itInput.Get())-0.5 );
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
  itFloat.GoToBegin();
  while ( !itFloat.IsAtEnd() )
    {
    row.insert( FloatFloatMapType::value_type( itFloat.Get(),0 ) );
    ++itFloat;
    }
  typedef std::map < float, FloatFloatMapType > ArrayMapType;
  ArrayMapType CumulativeArray;
  std::pair<ArrayMapType::iterator, bool> array;
  itFloat.GoToBegin();
  while ( !itFloat.IsAtEnd() )
    {
    array = CumulativeArray.insert( ArrayMapType::value_type(itFloat.Get(),row ) );
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


  int radius[ImageDimension];
  for ( i = 0; i < ImageDimension; i++)
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
  ImageRegionIteratorWithIndex<ImageFloatType> it(inputFloat, region);
  ImageRegionIterator<ImageType> itOut(output, region);
 
  // Assign the size of the window
  float sum;
  for ( i = 0; i < ImageDimension; i++)
    {
    size[i] = 2*radius[i] + 1;
    }
  
  // Map stores (number of pixel)/(window size) for each gray value. 
  typedef std::map<float, float> MapType;
  MapType count;
  MapType::iterator itMap;
  
  // Plahe algorithm
  it.GoToBegin();
  itOut.GoToBegin();
  float f;

  while( !it.IsAtEnd() )
    {
    count.clear();
    // Assign start index of the window
    for ( i = 0; i < ImageDimension; i++)
      {
      index[i] = it.GetIndex()[i] - radius[i];
      }
    region.SetIndex(index);
    region.SetSize(size);
    ImageRegionIterator< ImageType > itWin(inputFloat, region);
    itWin.GoToBegin();

    while ( !itWin.IsAtEnd() )
      {
      itMap = count.find(itWin.Get());
      if ( itMap != count.end() )
        {
        count[itWin.Get()] = count[itWin.Get()] + kernel;
        }
      else
        {
        count.insert(MapType::value_type(itWin.Get(),kernel));
        }

      ++itWin;
      }

    typedef typename ImageType::PixelType PixelType;    

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
      itOut.Set( (PixelType)(iscale*(sum+0.5) + min) );
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
      itOut.Set((PixelType)(iscale*(sum+0.5) + min));
      ++itOut;
      ++it;
      }
    }
    
}

template <class TImageType>
void
PlaheImageFilter<TImageType>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << "Window: " << m_Window << std::endl;
  os << "Alpha: " << m_Alpha << std::endl;
  os << "Beta: " << m_Beta << std::endl;
}
} // end namespace


#endif
