/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryImageToStatisticsLabelMapFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBinaryImageToStatisticsLabelMapFilter_txx
#define __itkBinaryImageToStatisticsLabelMapFilter_txx

#include "itkBinaryImageToStatisticsLabelMapFilter.h"
#include "itkProgressAccumulator.h"

namespace itk
{
template< class TInputImage, class TFeatureImage, class TOutputImage >
BinaryImageToStatisticsLabelMapFilter< TInputImage, TFeatureImage, TOutputImage >
::BinaryImageToStatisticsLabelMapFilter()
{
  m_OutputBackgroundValue = NumericTraits< OutputImagePixelType >::NonpositiveMin();
  m_InputForegroundValue = NumericTraits< OutputImagePixelType >::max();
  m_FullyConnected = false;
  m_ComputeFeretDiameter = false;
  m_ComputePerimeter = false;
  m_NumberOfBins = 128;
  m_ComputeHistogram = true;
  this->SetNumberOfRequiredInputs(2);
}

template< class TInputImage, class TFeatureImage, class TOutputImage >
void
BinaryImageToStatisticsLabelMapFilter< TInputImage, TFeatureImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // We need all the input.
  InputImagePointer input = const_cast< InputImageType * >( this->GetInput() );
  if ( input )
    {
    input->SetRequestedRegion( input->GetLargestPossibleRegion() );
    }
}

template< class TInputImage, class TFeatureImage, class TOutputImage >
void
BinaryImageToStatisticsLabelMapFilter< TInputImage, TFeatureImage, TOutputImage >
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
  ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}

template< class TInputImage, class TFeatureImage, class TOutputImage >
void
BinaryImageToStatisticsLabelMapFilter< TInputImage, TFeatureImage, TOutputImage >
::GenerateData()
{
  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();

  progress->SetMiniPipelineFilter(this);

  // Allocate the output
  this->AllocateOutputs();

  typename LabelizerType::Pointer labelizer = LabelizerType::New();
  labelizer->SetInput( this->GetInput() );
  labelizer->SetInputForegroundValue(m_InputForegroundValue);
  labelizer->SetOutputBackgroundValue(m_OutputBackgroundValue);
  labelizer->SetFullyConnected(m_FullyConnected);
  labelizer->SetNumberOfThreads( this->GetNumberOfThreads() );
  progress->RegisterInternalFilter(labelizer, .5f);

  typename LabelObjectValuatorType::Pointer valuator = LabelObjectValuatorType::New();
  valuator->SetInput( labelizer->GetOutput() );
  valuator->SetFeatureImage( this->GetFeatureImage() );
  valuator->SetNumberOfThreads( this->GetNumberOfThreads() );
  valuator->SetComputePerimeter(m_ComputePerimeter);
  valuator->SetComputeFeretDiameter(m_ComputeFeretDiameter);
  valuator->SetComputeHistogram(m_ComputeHistogram);
  valuator->SetNumberOfBins(m_NumberOfBins);
  progress->RegisterInternalFilter(valuator, .5f);

  valuator->GraftOutput( this->GetOutput() );
  valuator->Update();
  this->GraftOutput( valuator->GetOutput() );
}

template< class TInputImage, class TFeatureImage, class TOutputImage >
void
BinaryImageToStatisticsLabelMapFilter< TInputImage, TFeatureImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FullyConnected: "  << m_FullyConnected << std::endl;
  os << indent << "OutputBackgroundValue: "
     << static_cast< typename NumericTraits< OutputImagePixelType >::PrintType >( m_OutputBackgroundValue )
     << std::endl;
  os << indent << "InputForegroundValue: "
     << static_cast< typename NumericTraits< OutputImagePixelType >::PrintType >( m_InputForegroundValue ) << std::endl;
  os << indent << "ComputeFeretDiameter: " << m_ComputeFeretDiameter << std::endl;
  os << indent << "ComputePerimeter: " << m_ComputePerimeter << std::endl;
  os << indent << "ComputeHistogram: " << m_ComputeHistogram << std::endl;
  os << indent << "NumberOfBins: " << m_NumberOfBins << std::endl;
}
} // end namespace itk
#endif
