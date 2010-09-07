/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBinaryShapeKeepNObjectsImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBinaryShapeKeepNObjectsImageFilter_txx
#define __itkBinaryShapeKeepNObjectsImageFilter_txx

#include "itkBinaryShapeKeepNObjectsImageFilter.h"
#include "itkProgressAccumulator.h"

namespace itk
{
template< class TInputImage >
BinaryShapeKeepNObjectsImageFilter< TInputImage >
::BinaryShapeKeepNObjectsImageFilter()
{
  m_BackgroundValue = NumericTraits< OutputImagePixelType >::NonpositiveMin();
  m_ForegroundValue = NumericTraits< OutputImagePixelType >::max();
  m_FullyConnected = false;
  m_ReverseOrdering = false;
  m_Attribute = LabelObjectType::SIZE;
}

template< class TInputImage >
void
BinaryShapeKeepNObjectsImageFilter< TInputImage >
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

template< class TInputImage >
void
BinaryShapeKeepNObjectsImageFilter< TInputImage >
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
  ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}

template< class TInputImage >
void
BinaryShapeKeepNObjectsImageFilter< TInputImage >
::GenerateData()
{
  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();

  progress->SetMiniPipelineFilter(this);

  // Allocate the output
  this->AllocateOutputs();

  typename LabelizerType::Pointer labelizer = LabelizerType::New();
  labelizer->SetInput( this->GetInput() );
  labelizer->SetInputForegroundValue(m_ForegroundValue);
  labelizer->SetOutputBackgroundValue(m_BackgroundValue);
  labelizer->SetFullyConnected(m_FullyConnected);
  labelizer->SetNumberOfThreads( this->GetNumberOfThreads() );
  progress->RegisterInternalFilter(labelizer, .3f);

  typename LabelObjectValuatorType::Pointer valuator = LabelObjectValuatorType::New();
  valuator->SetInput( labelizer->GetOutput() );
  valuator->SetNumberOfThreads( this->GetNumberOfThreads() );
  if ( m_Attribute == LabelObjectType::PERIMETER || m_Attribute == LabelObjectType::ROUNDNESS )
    {
    valuator->SetComputePerimeter(true);
    }
  if ( m_Attribute == LabelObjectType::FERET_DIAMETER )
    {
    valuator->SetComputeFeretDiameter(true);
    }
  progress->RegisterInternalFilter(valuator, .3f);

  typename KeepNObjectsType::Pointer opening = KeepNObjectsType::New();
  opening->SetInput( valuator->GetOutput() );
  opening->SetNumberOfObjects(m_NumberOfObjects);
  opening->SetReverseOrdering(m_ReverseOrdering);
  opening->SetAttribute(m_Attribute);
  opening->SetNumberOfThreads( this->GetNumberOfThreads() );
  progress->RegisterInternalFilter(opening, .2f);

  typename BinarizerType::Pointer binarizer = BinarizerType::New();
  binarizer->SetInput( opening->GetOutput() );
  binarizer->SetForegroundValue(m_ForegroundValue);
  binarizer->SetBackgroundValue(m_BackgroundValue);
  binarizer->SetBackgroundImage( this->GetInput() );
  binarizer->SetNumberOfThreads( this->GetNumberOfThreads() );
  progress->RegisterInternalFilter(binarizer, .2f);

  binarizer->GraftOutput( this->GetOutput() );
  binarizer->Update();
  this->GraftOutput( binarizer->GetOutput() );
}

template< class TInputImage >
void
BinaryShapeKeepNObjectsImageFilter< TInputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FullyConnected: "  << m_FullyConnected << std::endl;
  os << indent << "BackgroundValue: "
     << static_cast< typename NumericTraits< OutputImagePixelType >::PrintType >( m_BackgroundValue ) << std::endl;
  os << indent << "ForegroundValue: "
     << static_cast< typename NumericTraits< OutputImagePixelType >::PrintType >( m_ForegroundValue ) << std::endl;
  os << indent << "NumberOfObjects: "  << m_NumberOfObjects << std::endl;
  os << indent << "ReverseOrdering: "  << m_ReverseOrdering << std::endl;
  os << indent << "Attribute: "  << LabelObjectType::GetNameFromAttribute(m_Attribute) << " (" << m_Attribute << ")"
     << std::endl;
}
} // end namespace itk
#endif
