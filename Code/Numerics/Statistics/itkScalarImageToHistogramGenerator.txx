/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkScalarImageToHistogramGenerator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkScalarImageToHistogramGenerator_txx
#define __itkScalarImageToHistogramGenerator_txx

#include "itkScalarImageToHistogramGenerator.h"

namespace itk
{
namespace Statistics
{
template< class TImage >
ScalarImageToHistogramGenerator< TImage >
::ScalarImageToHistogramGenerator()
{
  m_ImageToListAdaptor = AdaptorType::New();
  m_HistogramGenerator = GeneratorType::New();
  m_HistogramGenerator->SetInput(m_ImageToListAdaptor);
}

template< class TImage >
void
ScalarImageToHistogramGenerator< TImage >
::SetInput(const ImageType *image)
{
  m_ImageToListAdaptor->SetImage(image);
}

template< class TImage >
const typename ScalarImageToHistogramGenerator< TImage >::HistogramType *
ScalarImageToHistogramGenerator< TImage >
::GetOutput() const
{
  return m_HistogramGenerator->GetOutput();
}

template< class TImage >
void
ScalarImageToHistogramGenerator< TImage >
::Compute()
{
  m_HistogramGenerator->Update();
}

template< class TImage >
void
ScalarImageToHistogramGenerator< TImage >
::SetNumberOfBins(unsigned int numberOfBins)
{
  typename HistogramType::SizeType size;
  size.SetSize(1);
  size.Fill(numberOfBins);
  m_HistogramGenerator->SetHistogramSize(size);
}

template< class TImage >
void
ScalarImageToHistogramGenerator< TImage >
::SetHistogramMin(RealPixelType minimumValue)
{
  typedef typename GeneratorType::HistogramMeasurementVectorType MeasurementVectorType;
  MeasurementVectorType minVector;
  MeasurementVectorTraits::SetLength(minVector, 1);
  minVector[0] = minimumValue;
  m_HistogramGenerator->SetHistogramBinMinimum(minVector);
}

template< class TImage >
void
ScalarImageToHistogramGenerator< TImage >
::SetHistogramMax(RealPixelType maximumValue)
{
  typedef typename GeneratorType::HistogramMeasurementVectorType MeasurementVectorType;
  MeasurementVectorType maxVector;
  MeasurementVectorTraits::SetLength(maxVector, 1);
  maxVector[0] = maximumValue;
  m_HistogramGenerator->SetHistogramBinMaximum(maxVector);
}

template< class TImage >
void
ScalarImageToHistogramGenerator< TImage >
::SetMarginalScale(double marginalScale)
{
  m_HistogramGenerator->SetMarginalScale(marginalScale);
}

template< class TImage >
void
ScalarImageToHistogramGenerator< TImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << "ImageToListSample adaptor = " << m_ImageToListAdaptor << std::endl;
  os << "HistogramGenerator = " << m_HistogramGenerator << std::endl;
}
} // end of namespace Statistics
} // end of namespace itk

#endif
