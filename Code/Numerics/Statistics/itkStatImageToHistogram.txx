/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStatImageToHistogram.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkStatImageToHistogram_txx
#define __itkStatImageToHistogram_txx

#include <typeinfo>
#include "itkStatImageToHistogram.h"
#include "itkImageRegionIterator.h"

namespace itk
{

template <class TImage, class THistogram>
StatImageToHistogram<TImage, THistogram>
::StatImageToHistogram()
{
  /*
  m_LowerBound = 0;
  m_UpperBound = 1;;
  m_NBins = 256;
  m_UpperFlag = false;
  m_LowerFlag = false;
  m_NBinsFlag = false;
  */
}

/*
template <class TImage, class THistogram>
void
StatImageToHistogram<TImage, THistogram>
::SetLowerBound(float lower)
{
  //m_LowerBound = lower;
  //m_LowerFlag  = true;
}

template <class TImage, class THistogram>
void
StatImageToHistogram<TImage, THistogram>
::SetUpperBound(float upper)
{
  //m_UpperBound = upper;
  //m_UpperFlag = true;
}

template <class TImage, class THistogram>
void
StatImageToHistogram<TImage, THistogram>
::SetNBins(int nbins)
{
  //m_NBins = nbins;
  //m_NBinsFlag = true;
}
*/

template <class TImage, class THistogram>
void
StatImageToHistogram<TImage, THistogram>
::SetInput( ImagePointer image )
{
  this->m_Image = image;
  //SetNthInput(0, image );
}

template <class TImage, class THistogram>
void
StatImageToHistogram<TImage, THistogram>
::GenerateData( void )
{
  typedef ImageRegionIterator<TImage> IteratorType;
  typename TImage::RegionType region;
  region = m_Image->GetBufferedRegion();
  IteratorType itIn(m_Image, region);

  typedef typename THistogram::PointType PointType;
  //typedef typename TImage::PixelType PixelType;
  PointType point;
  //PixelType pixel;

  // Now it is working for one dimensional histogram
  int i=0;
  while(!itIn.IsAtEnd())
    {
    point[0] = itIn.Get();
    m_Histogram->AddFrequencyByOne(point);
    ++itIn;
    }

}

} // end of namespace

#endif
