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
#ifndef itkGrayscaleMorphologicalOpeningImageFilter_hxx
#define itkGrayscaleMorphologicalOpeningImageFilter_hxx

#include "itkGrayscaleMorphologicalOpeningImageFilter.h"
#include "itkNumericTraits.h"
#include "itkProgressAccumulator.h"
#include <string>
#include "itkCropImageFilter.h"
#include "itkConstantPadImageFilter.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage, typename TKernel >
GrayscaleMorphologicalOpeningImageFilter< TInputImage, TOutputImage, TKernel >
::GrayscaleMorphologicalOpeningImageFilter() :
  m_HistogramDilateFilter(HistogramDilateFilterType::New()),
  m_HistogramErodeFilter(HistogramErodeFilterType::New()),
  m_BasicDilateFilter(BasicDilateFilterType::New()),
  m_BasicErodeFilter(BasicErodeFilterType::New()),
  m_VanHerkGilWermanDilateFilter(VanHerkGilWermanDilateFilterType::New()),
  m_VanHerkGilWermanErodeFilter(VanHerkGilWermanErodeFilterType::New()),
  m_AnchorFilter(AnchorFilterType::New()),
  m_Algorithm(HISTO),
  m_SafeBorder(true)
{
}

template< typename TInputImage, typename TOutputImage, typename TKernel >
void
GrayscaleMorphologicalOpeningImageFilter< TInputImage, TOutputImage, TKernel >
::SetKernel(const KernelType & kernel)
{
  const FlatKernelType *flatKernel = dynamic_cast< const FlatKernelType * >( &kernel );

  if ( flatKernel != ITK_NULLPTR && flatKernel->GetDecomposable() )
    {
    m_AnchorFilter->SetKernel(*flatKernel);
    m_Algorithm = ANCHOR;
    }
  else if ( m_HistogramDilateFilter->GetUseVectorBasedAlgorithm() )
    {
    // histogram based filter is as least as good as the basic one, so always
    // use it
    m_Algorithm = HISTO;
    m_HistogramDilateFilter->SetKernel(kernel);
    m_HistogramErodeFilter->SetKernel(kernel);
    }
  else
    {
    // basic filter can be better than the histogram based one
    // apply a poor heuristic to find the best one. What is very important is to
    // select the histogram for large kernels

    // we need to set the kernel on the histogram filter to compare basic and
    // histogram algorithm
    m_HistogramDilateFilter->SetKernel(kernel);

    if ( this->GetKernel().Size() < m_HistogramDilateFilter->GetPixelsPerTranslation() * 4.0 )
      {
      m_BasicDilateFilter->SetKernel(kernel);
      m_BasicErodeFilter->SetKernel(kernel);
      m_Algorithm = BASIC;
      }
    else
      {
      m_HistogramErodeFilter->SetKernel(kernel);
      m_Algorithm = HISTO;
      }
    }

  Superclass::SetKernel(kernel);
}

template< typename TInputImage, typename TOutputImage, typename TKernel >
void
GrayscaleMorphologicalOpeningImageFilter< TInputImage, TOutputImage, TKernel >
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
      m_HistogramDilateFilter->SetKernel( this->GetKernel() );
      m_HistogramErodeFilter->SetKernel( this->GetKernel() );
      }
    else if ( flatKernel != ITK_NULLPTR && flatKernel->GetDecomposable() && algo == ANCHOR )
      {
      m_AnchorFilter->SetKernel(*flatKernel);
      }
    else if ( flatKernel != ITK_NULLPTR && flatKernel->GetDecomposable() && algo == VHGW )
      {
      m_VanHerkGilWermanDilateFilter->SetKernel(*flatKernel);
      m_VanHerkGilWermanErodeFilter->SetKernel(*flatKernel);
      }
    else
                          { itkExceptionMacro(<< "Invalid algorithm"); }

    m_Algorithm = algo;
    this->Modified();
    }
}

template< typename TInputImage, typename TOutputImage, typename TKernel >
void
GrayscaleMorphologicalOpeningImageFilter< TInputImage, TOutputImage, TKernel >
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
    if ( m_SafeBorder )
      {
      typedef ConstantPadImageFilter< InputImageType, InputImageType > PadType;
      typename PadType::Pointer pad = PadType::New();
      pad->SetPadLowerBound(this->GetKernel().GetRadius());
      pad->SetPadUpperBound(this->GetKernel().GetRadius());
      pad->SetConstant( NumericTraits< typename InputImageType::PixelType >::max() );
      pad->SetInput( this->GetInput() );
      progress->RegisterInternalFilter(pad, 0.1f);

      m_BasicErodeFilter->SetInput( pad->GetOutput() );
      progress->RegisterInternalFilter(m_BasicErodeFilter, 0.4f);

      m_BasicDilateFilter->SetInput( m_BasicErodeFilter->GetOutput() );
      progress->RegisterInternalFilter(m_BasicDilateFilter, 0.4f);

      typedef CropImageFilter< TOutputImage, TOutputImage > CropType;
      typename CropType::Pointer crop = CropType::New();
      crop->SetInput( m_BasicDilateFilter->GetOutput() );
      crop->SetUpperBoundaryCropSize( this->GetKernel().GetRadius() );
      crop->SetLowerBoundaryCropSize( this->GetKernel().GetRadius() );
      progress->RegisterInternalFilter(crop, 0.1f);

      crop->GraftOutput( this->GetOutput() );
      crop->Update();
      this->GraftOutput( crop->GetOutput() );
      }
    else
      {
      m_BasicErodeFilter->SetInput( this->GetInput() );
      progress->RegisterInternalFilter(m_BasicErodeFilter, 0.5f);

      m_BasicDilateFilter->SetInput( m_BasicErodeFilter->GetOutput() );
      progress->RegisterInternalFilter(m_BasicDilateFilter, 0.5f);

      m_BasicDilateFilter->GraftOutput( this->GetOutput() );
      m_BasicDilateFilter->Update();
      this->GraftOutput( m_BasicDilateFilter->GetOutput() );
      }
    }
  else if ( m_Algorithm == HISTO )
    {
    // std::cout << "MovingHistogramDilateImageFilter" << std::endl;
    if ( m_SafeBorder )
      {
      typedef ConstantPadImageFilter< InputImageType, InputImageType > PadType;
      typename PadType::Pointer pad = PadType::New();
      pad->SetPadLowerBound(this->GetKernel().GetRadius());
      pad->SetPadUpperBound(this->GetKernel().GetRadius());
      pad->SetConstant( NumericTraits< typename InputImageType::PixelType >::max() );
      pad->SetInput( this->GetInput() );
      progress->RegisterInternalFilter(pad, 0.1f);

      m_HistogramErodeFilter->SetInput( pad->GetOutput() );
      progress->RegisterInternalFilter(m_HistogramErodeFilter, 0.4f);

      m_HistogramDilateFilter->SetInput( m_HistogramErodeFilter->GetOutput() );
      progress->RegisterInternalFilter(m_HistogramDilateFilter, 0.4f);

      typedef CropImageFilter< TOutputImage, TOutputImage > CropType;
      typename CropType::Pointer crop = CropType::New();
      crop->SetInput( m_HistogramDilateFilter->GetOutput() );
      crop->SetUpperBoundaryCropSize( this->GetKernel().GetRadius() );
      crop->SetLowerBoundaryCropSize( this->GetKernel().GetRadius() );
      progress->RegisterInternalFilter(crop, 0.1f);

      crop->GraftOutput( this->GetOutput() );
      crop->Update();
      this->GraftOutput( crop->GetOutput() );
      }
    else
      {
      m_HistogramErodeFilter->SetInput( this->GetInput() );
      progress->RegisterInternalFilter(m_HistogramErodeFilter, 0.5f);

      m_HistogramDilateFilter->SetInput( m_HistogramErodeFilter->GetOutput() );
      progress->RegisterInternalFilter(m_HistogramDilateFilter, 0.5f);

      m_HistogramDilateFilter->GraftOutput( this->GetOutput() );
      m_HistogramDilateFilter->Update();
      this->GraftOutput( m_HistogramDilateFilter->GetOutput() );
      }
    }
  else if ( m_Algorithm == VHGW )
    {
    // std::cout << "VanHerkGilWermanDilateImageFilter" << std::endl;
    if ( m_SafeBorder )
      {
      typedef ConstantPadImageFilter< InputImageType, InputImageType > PadType;
      typename PadType::Pointer pad = PadType::New();
      pad->SetPadLowerBound(this->GetKernel().GetRadius());
      pad->SetPadUpperBound(this->GetKernel().GetRadius());
      pad->SetConstant( NumericTraits< typename InputImageType::PixelType >::max() );
      pad->SetInput( this->GetInput() );
      progress->RegisterInternalFilter(pad, 0.1f);

      m_VanHerkGilWermanErodeFilter->SetInput( pad->GetOutput() );
      progress->RegisterInternalFilter(m_VanHerkGilWermanErodeFilter, 0.4f);

      m_VanHerkGilWermanDilateFilter->SetInput( m_VanHerkGilWermanErodeFilter->GetOutput() );
      progress->RegisterInternalFilter(m_VanHerkGilWermanDilateFilter, 0.4f);

      typedef CropImageFilter< TInputImage, TOutputImage > CropType;
      typename CropType::Pointer crop = CropType::New();
      crop->SetInput( m_VanHerkGilWermanDilateFilter->GetOutput() );
      crop->SetUpperBoundaryCropSize( this->GetKernel().GetRadius() );
      crop->SetLowerBoundaryCropSize( this->GetKernel().GetRadius() );
      progress->RegisterInternalFilter(crop, 0.1f);

      crop->GraftOutput( this->GetOutput() );
      crop->Update();
      this->GraftOutput( crop->GetOutput() );
      }
    else
      {
      m_VanHerkGilWermanErodeFilter->SetInput( this->GetInput() );
      progress->RegisterInternalFilter(m_VanHerkGilWermanErodeFilter, 0.5f);

      m_VanHerkGilWermanDilateFilter->SetInput( m_VanHerkGilWermanErodeFilter->GetOutput() );
      progress->RegisterInternalFilter(m_VanHerkGilWermanDilateFilter, 0.5f);

      m_VanHerkGilWermanDilateFilter->GraftOutput( this->GetOutput() );
      typedef CastImageFilter< TInputImage, TOutputImage > CastType;
      typename CastType::Pointer cast = CastType::New();
      cast->SetInput( m_VanHerkGilWermanDilateFilter->GetOutput() );
      progress->RegisterInternalFilter(cast, 0.1f);

      cast->GraftOutput( this->GetOutput() );
      cast->Update();
      this->GraftOutput( cast->GetOutput() );
      }
    }
  else if ( m_Algorithm == ANCHOR )
    {
//     std::cout << "AnchorDilateImageFilter" << std::endl;
    if ( m_SafeBorder )
      {
      typedef ConstantPadImageFilter< InputImageType, InputImageType > PadType;
      typename PadType::Pointer pad = PadType::New();
      pad->SetPadLowerBound(this->GetKernel().GetRadius());
      pad->SetPadUpperBound(this->GetKernel().GetRadius());
      pad->SetConstant( NumericTraits< typename InputImageType::PixelType >::max() );
      pad->SetInput( this->GetInput() );
      progress->RegisterInternalFilter(pad, 0.1f);

      m_AnchorFilter->SetInput( pad->GetOutput() );
      progress->RegisterInternalFilter(m_AnchorFilter, 0.8f);

      typedef CropImageFilter< TInputImage, TOutputImage > CropType;
      typename CropType::Pointer crop = CropType::New();
      crop->SetInput( m_AnchorFilter->GetOutput() );
      crop->SetUpperBoundaryCropSize( this->GetKernel().GetRadius() );
      crop->SetLowerBoundaryCropSize( this->GetKernel().GetRadius() );
      progress->RegisterInternalFilter(crop, 0.1f);

      crop->GraftOutput( this->GetOutput() );
      crop->Update();
      this->GraftOutput( crop->GetOutput() );
      }
    else
      {
      m_AnchorFilter->SetInput( this->GetInput() );
      progress->RegisterInternalFilter(m_AnchorFilter, 0.9f);

      typedef CastImageFilter< TInputImage, TOutputImage > CastType;
      typename CastType::Pointer cast = CastType::New();
      cast->SetInput( m_AnchorFilter->GetOutput() );
      progress->RegisterInternalFilter(cast, 0.1f);

      cast->GraftOutput( this->GetOutput() );
      cast->Update();
      this->GraftOutput( cast->GetOutput() );
      }
    }
}

template< typename TInputImage, typename TOutputImage, typename TKernel >
void
GrayscaleMorphologicalOpeningImageFilter< TInputImage, TOutputImage, TKernel >
::Modified() const
{
  Superclass::Modified();
  m_BasicDilateFilter->Modified();
  m_BasicErodeFilter->Modified();
  m_HistogramDilateFilter->Modified();
  m_HistogramErodeFilter->Modified();
  m_VanHerkGilWermanDilateFilter->Modified();
  m_VanHerkGilWermanErodeFilter->Modified();
  m_AnchorFilter->Modified();
}

template< typename TInputImage, typename TOutputImage, typename TKernel >
void
GrayscaleMorphologicalOpeningImageFilter< TInputImage, TOutputImage, TKernel >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Algorithm: " << m_Algorithm << std::endl;
  os << indent << "SafeBorder: " << m_SafeBorder << std::endl;
}
} // end namespace itk
#endif
