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
#ifndef itkMorphologicalGradientImageFilter_hxx
#define itkMorphologicalGradientImageFilter_hxx

#include "itkMorphologicalGradientImageFilter.h"
#include "itkNumericTraits.h"
#include "itkProgressAccumulator.h"
#include <string>

namespace itk
{
template< typename TInputImage, typename TOutputImage, typename TKernel >
MorphologicalGradientImageFilter< TInputImage, TOutputImage, TKernel >
::MorphologicalGradientImageFilter()
{
  m_BasicDilateFilter = BasicDilateFilterType::New();
  m_BasicErodeFilter = BasicErodeFilterType::New();
  m_HistogramFilter = HistogramFilterType::New();
  m_AnchorDilateFilter = AnchorDilateFilterType::New();
  m_AnchorErodeFilter = AnchorErodeFilterType::New();
  m_VanHerkGilWermanDilateFilter = VHGWDilateFilterType::New();
  m_VanHerkGilWermanErodeFilter = VHGWErodeFilterType::New();
  m_Algorithm = HISTO;
}

template< typename TInputImage, typename TOutputImage, typename TKernel >
void
MorphologicalGradientImageFilter< TInputImage, TOutputImage, TKernel >
::SetKernel(const KernelType & kernel)
{
  const FlatKernelType *flatKernel = dynamic_cast< const FlatKernelType * >( &kernel );

  if ( flatKernel != ITK_NULLPTR && flatKernel->GetDecomposable() )
    {
    m_AnchorDilateFilter->SetKernel(*flatKernel);
    m_AnchorErodeFilter->SetKernel(*flatKernel);
    m_Algorithm = ANCHOR;
    }
  else if ( m_HistogramFilter->GetUseVectorBasedAlgorithm() )
    {
    // histogram based filter is as least as good as the basic one, so always
    // use it
    m_Algorithm = HISTO;
    m_HistogramFilter->SetKernel(kernel);
    }
  else
    {
    // basic filter can be better than the histogram based one
    // apply a poor heuristic to find the best one. What is very important is to
    // select the histogram for large kernels

    // we need to set the kernel on the histogram filter to compare basic and
    // histogram algorithm
    m_HistogramFilter->SetKernel(kernel);

    if ( this->GetKernel().Size() < m_HistogramFilter->GetPixelsPerTranslation() * 4.0 )
      {
      m_BasicDilateFilter->SetKernel(kernel);
      m_BasicErodeFilter->SetKernel(kernel);
      m_Algorithm = BASIC;
      }
    else
      {
      m_Algorithm = HISTO;
      }
    }

  Superclass::SetKernel(kernel);
}

template< typename TInputImage, typename TOutputImage, typename TKernel >
void
MorphologicalGradientImageFilter< TInputImage, TOutputImage, TKernel >
::SetAlgorithm(int algo)
{
  const FlatKernelType *flatKernel = dynamic_cast< const FlatKernelType * >( &this->GetKernel() );

  if ( m_Algorithm != algo )
    {
    if ( algo == BASIC )
      {
      m_BasicDilateFilter->SetKernel( this->GetKernel() );
      m_BasicErodeFilter->SetKernel( this->GetKernel() );
      }
    else if ( algo == HISTO )
      {
      m_HistogramFilter->SetKernel( this->GetKernel() );
      }
    else if ( flatKernel != ITK_NULLPTR && flatKernel->GetDecomposable() && algo == ANCHOR )
      {
      m_AnchorDilateFilter->SetKernel(*flatKernel);
      m_AnchorErodeFilter->SetKernel(*flatKernel);
      }
    else if ( flatKernel != ITK_NULLPTR && flatKernel->GetDecomposable() && algo == VHGW )
      {
      m_VanHerkGilWermanDilateFilter->SetKernel(*flatKernel);
      m_VanHerkGilWermanErodeFilter->SetKernel(*flatKernel);
      }
    else
      {
      itkExceptionMacro(<< "Invalid algorithm");
      }

    m_Algorithm = algo;
    this->Modified();
    }
}

template< typename TInputImage, typename TOutputImage, typename TKernel >
void
MorphologicalGradientImageFilter< TInputImage, TOutputImage, TKernel >
::GenerateData()
{
  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();

  progress->SetMiniPipelineFilter(this);

  // Allocate the output
  this->AllocateOutputs();

  // Delegate to a dilate filter.
  if ( m_Algorithm == BASIC )
    {
//     std::cout << "BasicDilateImageFilter" << std::endl;
    m_BasicDilateFilter->SetInput( this->GetInput() );
    progress->RegisterInternalFilter(m_BasicDilateFilter, 0.4f);

    m_BasicErodeFilter->SetInput( this->GetInput() );
    progress->RegisterInternalFilter(m_BasicErodeFilter, 0.4f);

    typename SubtractFilterType::Pointer sub = SubtractFilterType::New();
    sub->SetInput1( m_BasicDilateFilter->GetOutput() );
    sub->SetInput2( m_BasicErodeFilter->GetOutput() );
    progress->RegisterInternalFilter(sub, 0.1f);

    sub->GraftOutput( this->GetOutput() );
    sub->Update();
    this->GraftOutput( sub->GetOutput() );
    }
  else if ( m_Algorithm == HISTO )
    {
//     std::cout << "MovingHistogramDilateImageFilter" << std::endl;
    m_HistogramFilter->SetInput( this->GetInput() );
    progress->RegisterInternalFilter(m_HistogramFilter, 1.0f);

    m_HistogramFilter->GraftOutput( this->GetOutput() );
    m_HistogramFilter->Update();
    this->GraftOutput( m_HistogramFilter->GetOutput() );
    }
  else if ( m_Algorithm == ANCHOR )
    {
    // std::cout << "AnchorDilateImageFilter" << std::endl;
    m_AnchorDilateFilter->SetInput( this->GetInput() );
    progress->RegisterInternalFilter(m_AnchorDilateFilter, 0.4f);

    m_AnchorErodeFilter->SetInput( this->GetInput() );
    progress->RegisterInternalFilter(m_AnchorErodeFilter, 0.4f);

    typename SubtractFilterType::Pointer sub = SubtractFilterType::New();
    sub->SetInput1( m_AnchorDilateFilter->GetOutput() );
    sub->SetInput2( m_AnchorErodeFilter->GetOutput() );
    progress->RegisterInternalFilter(sub, 0.1f);

    sub->GraftOutput( this->GetOutput() );
    sub->Update();
    this->GraftOutput( sub->GetOutput() );
    }
  else if ( m_Algorithm == VHGW )
    {
//     std::cout << "VanHerkGilWermanDilateImageFilter" << std::endl;
    m_VanHerkGilWermanDilateFilter->SetInput( this->GetInput() );
    progress->RegisterInternalFilter(m_VanHerkGilWermanDilateFilter, 0.4f);

    m_VanHerkGilWermanErodeFilter->SetInput( this->GetInput() );
    progress->RegisterInternalFilter(m_VanHerkGilWermanErodeFilter, 0.4f);

    typename SubtractFilterType::Pointer sub = SubtractFilterType::New();
    sub->SetInput1( m_VanHerkGilWermanDilateFilter->GetOutput() );
    sub->SetInput2( m_VanHerkGilWermanErodeFilter->GetOutput() );
    progress->RegisterInternalFilter(sub, 0.1f);

    sub->GraftOutput( this->GetOutput() );
    sub->Update();
    this->GraftOutput( sub->GetOutput() );
    }
}

template< typename TInputImage, typename TOutputImage, typename TKernel >
void
MorphologicalGradientImageFilter< TInputImage, TOutputImage, TKernel >
::Modified() const
{
  Superclass::Modified();
  m_BasicDilateFilter->Modified();
  m_BasicErodeFilter->Modified();
  m_HistogramFilter->Modified();
  m_AnchorDilateFilter->Modified();
  m_AnchorErodeFilter->Modified();
  m_VanHerkGilWermanDilateFilter->Modified();
  m_VanHerkGilWermanErodeFilter->Modified();
}

template< typename TInputImage, typename TOutputImage, typename TKernel >
void
MorphologicalGradientImageFilter< TInputImage, TOutputImage, TKernel >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Algorithm: " << m_Algorithm << std::endl;
}
} // end namespace itk
#endif
