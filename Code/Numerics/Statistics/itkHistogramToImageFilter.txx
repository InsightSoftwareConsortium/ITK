/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkHistogramToImageFilter.txx
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) Insight Software Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkHistogramToImageFilter_txx
#define _itkHistogramToImageFilter_txx

#include "itkHistogramToImageFilter.h"
#include <itkNumericTraits.h>

namespace itk
{

/** Constructor */
template <class THistogram>
HistogramToImageFilter<THistogram>
::HistogramToImageFilter()
{
  this->SetNumberOfRequiredInputs(1);
  m_Size.Fill(0);
  m_Spacing.Fill(1.0);
  m_Origin.Fill(0.0);
}

/** Destructor */
template <class THistogram>
HistogramToImageFilter<THistogram>
::~HistogramToImageFilter()
{
}
  

/** Set the Input Histogram */
template <class THistogram>
void 
HistogramToImageFilter<THistogram>
::SetInput( const HistogramType *input)
{
  
  // Histograms are not dataobjects, so need to decorate it to push it down
  // the pipeline 
  typename InputHistogramObjectType::Pointer histogramObject = 
    InputHistogramObjectType::New();
  histogramObject->Set( const_cast< HistogramType * >(input) );
  
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput(0,  histogramObject  );
}


/** Set the Input Histogram if already decorated */
template <class THistogram>
void 
HistogramToImageFilter<THistogram>
::SetInput( const InputHistogramObjectType *inputObject)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput(0, 
                       const_cast< InputHistogramObjectType * >( inputObject ) );
}

template <class THistogram>
const typename HistogramToImageFilter< THistogram >::InputHistogramObjectType * 
HistogramToImageFilter<THistogram>
::GetInput(void) 
{
  if (this->GetNumberOfInputs() < 1)
    {
    return 0;
    }
  
  return static_cast<const InputHistogramObjectType * >
    (this->ProcessObject::GetInput(0) );
}


template <class THistogram>
void 
HistogramToImageFilter<THistogram>
::SetSpacing(
  const double spacing[Self::ImageDimension] )
{
  SpacingType s(spacing);
  this->SetSpacing( s );
}


template <class THistogram>
void 
HistogramToImageFilter<THistogram >
::SetOrigin( const double origin[Self::ImageDimension] )
{
  PointType p(origin);
  this->SetOrigin(p);
}

//----------------------------------------------------------------------------

/** Update */
template <class THistogram>
void
HistogramToImageFilter< THistogram >
::GenerateData(void)
{
  unsigned int i;
  itkDebugMacro(<< "HistogramToImageFilter::Update() called");

  // Get the input and output pointers 
  // Get from decorator
  const HistogramType *inputHistogram = this->GetInput()->Get();
  //const HistogramType *inputHistogram  = this->GetInput();
  OutputImageType *   outputImage = this->GetOutput();

  // Generate the image
  double origin[ImageDimension];
  SizeType size;
  
  // Set the image size to the number of bins along each dimension.
  for( i=0; i< ImageDimension; i++)
    { 
    this->m_Size[i] = inputHistogram->GetSize(i);
    }

  // Set output image params and Allocate image
  typename OutputImageType::RegionType region;
  region.SetSize( m_Size );
  outputImage->SetRegions( region);
  outputImage->SetSpacing(this->m_Spacing);         // set spacing
  outputImage->SetOrigin(origin);   //   and origin
  outputImage->Allocate();   // allocate the image   
  
  // Fill image with frequencies from Histogram
  typename HistogramType::IndexType index;
  typename HistogramType::IndexValueType indexValue;
  
  ImageIteratorType iter( outputImage, region );
  while( !iter.IsAtEnd() )
    {
    typename OutputImageType::IndexType idx = iter.GetIndex();
    
    //Get histogram value at this idx and set it
    iter.Set( static_cast< unsigned long >( inputHistogram->GetFrequency( idx)));

    //std::cout << "Idx: " << idx << " Freq: " << static_cast< unsigned long >( inputHistogram->GetFrequency( idx))   << std::endl;
    ++iter;
    }

} // end update function  


template<class THistogram>
void 
HistogramToImageFilter<THistogram>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Size : " << m_Size << std::endl;
  os << indent << "Origin: " << m_Origin << std::endl;
  os << indent << "Spacing: " << m_Spacing << std::endl;
}


} // end namespace itk

#endif
