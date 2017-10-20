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
#ifndef itkObjectnessMeasureImageFilter_hxx
#define itkObjectnessMeasureImageFilter_hxx

#include "itkObjectnessMeasureImageFilter.h"
#include "itkHessianImageFilter.h"
#include "itkHessianToObjectnessMeasureImageFilter.h"
#include "itkProgressAccumulator.h"


namespace itk
{

template< typename TInputImage, typename TOutputImage >
ObjectnessMeasureImageFilter< TInputImage,TOutputImage >
::ObjectnessMeasureImageFilter() :
  m_Alpha( 0.5 ),
  m_Beta( 0.5 ),
  m_Gamma( 5.0 ),
  m_ObjectDimension( 1 ),
  m_BrightObject( true ),
  m_ScaleObjectnessMeasure( true )
{
}

template< typename TInputImage, typename TOutputImage >
ObjectnessMeasureImageFilter< TInputImage,TOutputImage >
::~ObjectnessMeasureImageFilter()
{
}


template< typename TInputImage, typename TOutputImage >
void
ObjectnessMeasureImageFilter< TInputImage,TOutputImage >
::EnlargeOutputRequestedRegion(DataObject *output)
{
  output->SetRequestedRegionToLargestPossibleRegion();
}

template< typename TInputImage, typename TOutputImage >
void
ObjectnessMeasureImageFilter< TInputImage,TOutputImage >
::GenerateData()
{
// Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter( this );

  typename InputImageType::Pointer localInput = InputImageType::New();
  localInput->Graft( this->GetInput() );

  typedef HessianImageFilter<InputImageType>          HessianFilterType;
  typedef typename HessianFilterType::OutputImageType HessianImageType;

  typename HessianFilterType::Pointer hessianFilter = HessianFilterType::New();

  hessianFilter->SetInput( localInput );

  typedef HessianToObjectnessMeasureImageFilter< HessianImageType, OutputImageType > ObjectnessFilterType;

  typename ObjectnessFilterType::Pointer objectnessFilter = ObjectnessFilterType::New();

  objectnessFilter->SetInput( hessianFilter->GetOutput() );

  objectnessFilter->SetAlpha( m_Alpha );
  objectnessFilter->SetBeta( m_Beta );
  objectnessFilter->SetGamma( m_Gamma );
  objectnessFilter->SetScaleObjectnessMeasure( m_ScaleObjectnessMeasure );
  objectnessFilter->SetObjectDimension( m_ObjectDimension );
  objectnessFilter->SetBrightObject( m_BrightObject);

  progress->RegisterInternalFilter( hessianFilter, 0.5f );
  progress->RegisterInternalFilter( objectnessFilter, 0.5f );


  objectnessFilter->GraftOutput( this->GetOutput() );
  objectnessFilter->Update();
  this->GraftOutput(objectnessFilter->GetOutput());
}

template< typename TInputImage, typename TOutputImage >
void
ObjectnessMeasureImageFilter< TInputImage,TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Alpha: " << m_Alpha << std::endl;
  os << indent << "Beta: " << m_Beta << std::endl;
  os << indent << "Gamma: " << m_Gamma << std::endl;
  os << indent << "ScaleObjectnessMeasure: " << m_ScaleObjectnessMeasure << std::endl;
  os << indent << "ObjectDimension: " << m_ObjectDimension << std::endl;
  os << indent << "BrightObject: " << m_BrightObject << std::endl;
}

}
#endif // itkObjectnessMeasureImageFilter_hxx
