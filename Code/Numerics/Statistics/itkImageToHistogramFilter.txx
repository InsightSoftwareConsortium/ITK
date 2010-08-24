/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToHistogramFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageToHistogramFilter_txx
#define __itkImageToHistogramFilter_txx

#include "itkImageToHistogramFilter.h"

namespace itk
{
namespace Statistics
{
template< class TImage >
ImageToHistogramFilter< TImage >
::ImageToHistogramFilter()
{
  this->SetNumberOfRequiredInputs(1);
  this->SetNumberOfRequiredOutputs(1);

  this->ProcessObject::SetNthOutput( 0, this->MakeOutput(0) );

  this->m_ImageToListAdaptor = AdaptorType::New();
  this->m_HistogramGenerator = GeneratorType::New();
  this->m_HistogramGenerator->SetInput(this->m_ImageToListAdaptor);
}

template< class TImage >
void
ImageToHistogramFilter< TImage >
::SetInput(const ImageType *image)
{
  this->ProcessObject::SetNthInput( 0, const_cast< ImageType * >( image ) );
}

template< class TImage >
const TImage *
ImageToHistogramFilter< TImage >
::GetInput() const
{
  if ( this->GetNumberOfInputs() < 1 )
    {
    return 0;
    }

  return static_cast< const ImageType * >( this->ProcessObject::GetInput(0) );
}

template< class TImage >
DataObject::Pointer
ImageToHistogramFilter< TImage >
::MakeOutput( unsigned int itkNotUsed(idx) )
{
  typename HistogramType::Pointer output = HistogramType::New();
  return static_cast< DataObject * >( output );
}

template< class TImage >
const typename ImageToHistogramFilter< TImage >::HistogramType *
ImageToHistogramFilter< TImage >
::GetOutput() const
{
  const HistogramType *output =
    static_cast< const HistogramType * >( this->ProcessObject::GetOutput(0) );

  return output;
}

template< class TImage >
void
ImageToHistogramFilter< TImage >
::GraftOutput(DataObject *graft)
{
  DataObject *output =
    const_cast< HistogramType * >( this->GetOutput() );

  // Call Histogram to copy meta-information, and the container
  output->Graft(graft);
}

template< class TImage >
void
ImageToHistogramFilter< TImage >
::GenerateData()
{
  this->m_ImageToListAdaptor->SetImage( this->GetInput() );

  this->m_HistogramGenerator->SetHistogramSizeInput( this->GetHistogramSizeInput() );
  this->m_HistogramGenerator->SetMarginalScaleInput( this->GetMarginalScaleInput() );
  this->m_HistogramGenerator->SetAutoMinimumMaximumInput( this->GetAutoMinimumMaximumInput() );
  this->m_HistogramGenerator->SetHistogramBinMinimumInput( this->GetHistogramBinMinimumInput() );
  this->m_HistogramGenerator->SetHistogramBinMaximumInput( this->GetHistogramBinMaximumInput() );

  this->m_HistogramGenerator->GraftOutput(
    static_cast< HistogramType * >( this->ProcessObject::GetOutput(0) ) );

  this->m_HistogramGenerator->Update();

  /** graft the minipipeline output back into this filter's output */
  this->GraftOutput(
    const_cast< HistogramType * >( this->m_HistogramGenerator->GetOutput() ) );
}

template< class TImage >
void
ImageToHistogramFilter< TImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "ImageToListSample adaptor = " << this->m_ImageToListAdaptor << std::endl;
  os << indent << "HistogramGenerator = " << this->m_HistogramGenerator << std::endl;
  // m_HistogramBinMinimum
  os << indent << "HistogramBinMinimum: " << this->GetHistogramBinMinimumInput() << std::endl;
  // m_HistogramBinMaximum
  os << indent << "HistogramBinMaximum: " << this->GetHistogramBinMaximumInput() << std::endl;
  // m_MarginalScale
  os << indent << "MarginalScale: " << this->GetMarginalScaleInput() << std::endl;
  // m_AutoMinimumMaximum
  os << indent << "AutoMinimumMaximum: " << this->GetAutoMinimumMaximumInput() << std::endl;
  // m_HistogramSize
  os << indent << "HistogramSize: " << this->GetHistogramSizeInput() << std::endl;
}
} // end of namespace Statistics
} // end of namespace itk

#endif
