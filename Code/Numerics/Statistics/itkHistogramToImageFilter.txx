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
template <class THistogram, class TFunction>
HistogramToImageFilter<THistogram, TFunction>
::HistogramToImageFilter()
{
  this->SetNumberOfRequiredInputs(1);
  m_Size.Fill(0);
  m_Spacing.Fill(1.0);
  m_Origin.Fill(0.0);
}

/** Destructor */
template <class THistogram, class TFunction>
HistogramToImageFilter<THistogram, TFunction>
::~HistogramToImageFilter()
{
}
  

/** Set the Input Histogram */
template <class THistogram, class TFunction>
void 
HistogramToImageFilter<THistogram, TFunction>
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
template <class THistogram, class TFunction>
void 
HistogramToImageFilter<THistogram, TFunction>
::SetInput( const InputHistogramObjectType *inputObject)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput(0, 
                       const_cast< InputHistogramObjectType * >( inputObject ) );
}

template <class THistogram, class TFunction>
const typename HistogramToImageFilter< THistogram, TFunction >::InputHistogramObjectType * 
HistogramToImageFilter<THistogram, TFunction>
::GetInput(void) 
{
  if (this->GetNumberOfInputs() < 1)
    {
    return 0;
    }
  
  return static_cast<const InputHistogramObjectType * >
    (this->ProcessObject::GetInput(0) );
}


template <class THistogram, class TFunction>
void 
HistogramToImageFilter<THistogram, TFunction>
::SetSpacing(
  const double spacing[Self::ImageDimension] )
{
  SpacingType s(spacing);
  this->SetSpacing( s );
}


template <class THistogram, class TFunction>
void 
HistogramToImageFilter<THistogram, TFunction>
::SetOrigin( const double origin[Self::ImageDimension] )
{
  PointType p(origin);
  SetOrigin(p);
}


template <class THistogram, class TFunction>
void 
HistogramToImageFilter<THistogram, TFunction>
::SetTotalFrequency( unsigned long n )
{
  if( n < 1 ) 
    {
    itkExceptionMacro("Total frequency in the histogram must be at least 1.") ;
    }
  
  if( n == this->GetFunctor().GetTotalFrequency() )
    {
    return;
    }
  else
    {
    this->GetFunctor().SetTotalFrequency( n );
    this->Modified();
    }
}
    



//----------------------------------------------------------------------------

/** Update */
template <class THistogram, class TFunction>
void
HistogramToImageFilter<THistogram, TFunction>
::GenerateData(void)
{
  unsigned int i;
  itkDebugMacro(<< "HistogramToImageFilter::Update() called");

  // Get the input and output pointers 
  // Get from decorator
  const HistogramType *inputHistogram = this->GetInput()->Get();
  OutputImageType     *outputImage    = this->GetOutput();

  
  // Set the image size to the number of bins along each dimension.
  for( i=0; i< ImageDimension; i++)
    { 
    m_Size[i]    = inputHistogram->GetSize(i);
    m_Origin[i]  = inputHistogram->GetBinMin(i,0);
    m_Spacing[i] = inputHistogram->GetBinMin(i,1) - m_Origin[i];
    }

  // Set output image params and Allocate image
  typename OutputImageType::RegionType region;
  region.SetSize( m_Size );
  outputImage->SetRegions( region );
  outputImage->SetSpacing( m_Spacing );   // set spacing
  outputImage->SetOrigin(  m_Origin  );   // and origin
  outputImage->Allocate();   // allocate the image   
 
  // Set the TotalFrequency in the functor
  this->SetTotalFrequency( static_cast< unsigned long >(
        inputHistogram->GetTotalFrequency() ));
 
  // Fill image with frequencies from Histogram
  ImageIteratorType iter( outputImage, region );
  while( !iter.IsAtEnd() )
    {
    typename OutputImageType::IndexType idx = iter.GetIndex();
    
    //Get histogram value at this idx and set it
    iter.Set( m_Functor( static_cast< unsigned long >(
            inputHistogram->GetFrequency( idx))));
    ++iter;
    }

} // end update function  


template <class THistogram, class TFunction>
void 
HistogramToImageFilter<THistogram, TFunction>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Size : " << m_Size << std::endl;
  os << indent << "Origin: " << m_Origin << std::endl;
  os << indent << "Spacing: " << m_Spacing << std::endl;
  os << indent << "Sum of frequencies of measurement vectors of the histogram: " <<
        m_Functor.GetTotalFrequency() << std::endl;
}


} // end namespace itk

#endif
