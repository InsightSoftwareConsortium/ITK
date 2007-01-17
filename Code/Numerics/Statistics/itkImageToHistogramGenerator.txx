/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToHistogramGenerator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkImageToHistogramGenerator_txx
#define _itkImageToHistogramGenerator_txx

#include "itkImageToHistogramGenerator.h"


namespace itk { 
namespace Statistics {


template < class TImage >
ImageToHistogramGenerator< TImage >
::ImageToHistogramGenerator() 
{
  m_ImageToListAdaptor = AdaptorType::New();
  m_HistogramGenerator = GeneratorType::New();
  m_HistogramGenerator->SetListSample( m_ImageToListAdaptor );

  // set a usable default value
  SizeType size;
  size.Fill( 128 );
  this->SetNumberOfBins( size );

}



template < class TImage >
void
ImageToHistogramGenerator< TImage >
::SetInput( const ImageType * image ) 
{
  m_ImageToListAdaptor->SetImage( image );
}


template < class TImage >
const typename ImageToHistogramGenerator< TImage >::HistogramType *
ImageToHistogramGenerator< TImage >
::GetOutput() const
{
  return m_HistogramGenerator->GetOutput();
}



template < class TImage >
void
ImageToHistogramGenerator< TImage >
::Compute() 
{
  m_HistogramGenerator->Update();
}



template < class TImage >
void
ImageToHistogramGenerator< TImage >
::SetNumberOfBins( const SizeType & size ) 
{
  m_HistogramGenerator->SetNumberOfBins( size );
}



template < class TImage >
void
ImageToHistogramGenerator< TImage >
::SetMarginalScale( double marginalScale )
{
  m_HistogramGenerator->SetMarginalScale( marginalScale );
}


template < class TImage >
void
ImageToHistogramGenerator< TImage >
::SetHistogramMin(const MeasurementVectorType & histogramMin)
{
  m_HistogramGenerator->SetHistogramMin(histogramMin);
}


template < class TImage >
void
ImageToHistogramGenerator< TImage >
::SetHistogramMax(const MeasurementVectorType & histogramMax)
{
  m_HistogramGenerator->SetHistogramMax(histogramMax);
}


template < class TImage >
void
ImageToHistogramGenerator< TImage >
::SetAutoMinMax(bool autoMinMax)
{
  m_HistogramGenerator->SetAutoMinMax(autoMinMax);
}


template < class TImage >
void
ImageToHistogramGenerator< TImage >
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << "ImageToListSample adaptor = " << m_ImageToListAdaptor << std::endl;
  os << "HistogramGenerator = " << m_HistogramGenerator << std::endl;
}



} // end of namespace Statistics 
} // end of namespace itk

#endif


