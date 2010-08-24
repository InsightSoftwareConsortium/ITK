/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryImageToShapeLabelMapFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBinaryImageToShapeLabelMapFilter_txx
#define __itkBinaryImageToShapeLabelMapFilter_txx

#include "itkBinaryImageToShapeLabelMapFilter.h"
#include "itkProgressAccumulator.h"

namespace itk
{
template< class TInputImage, class TOutputImage >
BinaryImageToShapeLabelMapFilter< TInputImage, TOutputImage >
::BinaryImageToShapeLabelMapFilter()
{
  m_OutputBackgroundValue = NumericTraits< OutputImagePixelType >::NonpositiveMin();
  m_InputForegroundValue = NumericTraits< OutputImagePixelType >::max();
  m_FullyConnected = false;
  m_ComputeFeretDiameter = false;
  m_ComputePerimeter = false;
}

template< class TInputImage, class TOutputImage >
void
BinaryImageToShapeLabelMapFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // We need all the inputs.
  InputImagePointer input = const_cast< InputImageType * >( this->GetInput() );
  if ( input )
    {
    input->SetRequestedRegion( input->GetLargestPossibleRegion() );
    }
}

template< class TInputImage, class TOutputImage >
void
BinaryImageToShapeLabelMapFilter< TInputImage, TOutputImage >
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
  ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}

template< class TInputImage, class TOutputImage >
void
BinaryImageToShapeLabelMapFilter< TInputImage, TOutputImage >
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
  valuator->SetNumberOfThreads( this->GetNumberOfThreads() );
  valuator->SetComputePerimeter(m_ComputePerimeter);
  valuator->SetComputeFeretDiameter(m_ComputeFeretDiameter);
  progress->RegisterInternalFilter(valuator, .5f);

  valuator->GraftOutput( this->GetOutput() );
  valuator->Update();
  this->GraftOutput( valuator->GetOutput() );
}

template< class TInputImage, class TOutputImage >
void
BinaryImageToShapeLabelMapFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FullyConnected: "  << m_FullyConnected << std::endl;
  os << indent << "BackgroundValue: "
     << static_cast< typename NumericTraits< OutputImagePixelType >::PrintType >( m_OutputBackgroundValue )
     << std::endl;
  os << indent << "ForegroundValue: "
     << static_cast< typename NumericTraits< OutputImagePixelType >::PrintType >( m_InputForegroundValue ) << std::endl;
  os << indent << "ComputeFeretDiameter: " << m_ComputeFeretDiameter << std::endl;
  os << indent << "ComputePerimeter: " << m_ComputePerimeter << std::endl;
}
} // end namespace itk
#endif
