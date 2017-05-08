/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkLabelImageToShapeLabelMapFilter_hxx
#define itkLabelImageToShapeLabelMapFilter_hxx

#include "itkLabelImageToShapeLabelMapFilter.h"
#include "itkProgressAccumulator.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage >
LabelImageToShapeLabelMapFilter< TInputImage, TOutputImage >
::LabelImageToShapeLabelMapFilter()
{
  m_BackgroundValue = NumericTraits< OutputImagePixelType >::NonpositiveMin();
  m_ComputeFeretDiameter = false;
  m_ComputePerimeter = true;
  m_ComputeOrientedBoundingBox = false;
}

template< typename TInputImage, typename TOutputImage >
void
LabelImageToShapeLabelMapFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  // Call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // We need all the inputs
  InputImagePointer input = const_cast< InputImageType * >( this->GetInput() );
  if ( input )
    {
    input->SetRequestedRegion( input->GetLargestPossibleRegion() );
    }
}

template< typename TInputImage, typename TOutputImage >
void
LabelImageToShapeLabelMapFilter< TInputImage, TOutputImage >
::EnlargeOutputRequestedRegion(DataObject *)
{
  this->GetOutput()
  ->SetRequestedRegion( this->GetOutput()->GetLargestPossibleRegion() );
}

template< typename TInputImage, typename TOutputImage >
void
LabelImageToShapeLabelMapFilter< TInputImage, TOutputImage >
::GenerateData()
{
  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();

  progress->SetMiniPipelineFilter(this);

  // Allocate the output
  this->AllocateOutputs();

  typename LabelizerType::Pointer labelizer = LabelizerType::New();
  labelizer->SetInput( this->GetInput() );
  labelizer->SetBackgroundValue(m_BackgroundValue);
  labelizer->SetNumberOfThreads( this->GetNumberOfThreads() );
  progress->RegisterInternalFilter(labelizer, .5f);

  typename LabelObjectValuatorType::Pointer valuator = LabelObjectValuatorType::New();
  valuator->SetInput( labelizer->GetOutput() );
  valuator->SetNumberOfThreads( this->GetNumberOfThreads() );
  valuator->SetComputePerimeter(m_ComputePerimeter);
  valuator->SetComputeFeretDiameter(m_ComputeFeretDiameter);
  valuator->SetComputeOrientedBoundingBox(m_ComputeOrientedBoundingBox);
  progress->RegisterInternalFilter(valuator, .5f);

  valuator->GraftOutput( this->GetOutput() );
  valuator->Update();
  this->GraftOutput( valuator->GetOutput() );
}

template< typename TInputImage, typename TOutputImage >
void
LabelImageToShapeLabelMapFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "BackgroundValue: "
     << static_cast< typename NumericTraits< OutputImagePixelType >::PrintType >( m_BackgroundValue ) << std::endl;
  os << indent << "ComputeFeretDiameter: " << m_ComputeFeretDiameter << std::endl;
  os << indent << "ComputePerimeter: " << m_ComputePerimeter << std::endl;
  os << indent << "ComputeOrientedBoundingBox: " << m_ComputeOrientedBoundingBox << std::endl;
}
} // end namespace itk
#endif
