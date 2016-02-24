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
#ifndef itkMiniPipelineSeparableImageFilter_hxx
#define itkMiniPipelineSeparableImageFilter_hxx

#include "itkMiniPipelineSeparableImageFilter.h"
#include "itkProgressAccumulator.h"

/*
 *
 * This code was contributed in the Insight Journal paper:
 * "Efficient implementation of kernel filtering"
 * by Beare R., Lehmann G
 * https://hdl.handle.net/1926/555
 * http://www.insight-journal.org/browse/publication/160
 *
 */

namespace itk
{
template< typename TInputImage, typename TOutputImage, typename TFilter >
MiniPipelineSeparableImageFilter< TInputImage, TOutputImage, TFilter >
::MiniPipelineSeparableImageFilter()
{
  // create the pipeline
  for ( unsigned i = 0; i < ImageDimension; i++ )
    {
    m_Filters[i] = FilterType::New();
    m_Filters[i]->ReleaseDataFlagOn();
    if ( i > 0 )
      {
      m_Filters[i]->SetInput( m_Filters[i - 1]->GetOutput() );
      }
    }

  m_Cast = CastType::New();
  m_Cast->SetInput( m_Filters[ImageDimension - 1]->GetOutput() );
  m_Cast->SetInPlace(true);
}

template< typename TInputImage, typename TOutputImage, typename TFilter >
void
MiniPipelineSeparableImageFilter< TInputImage, TOutputImage, TFilter >
::Modified() const
{
  Superclass::Modified();
  for ( unsigned i = 0; i < ImageDimension; i++ )
    {
    m_Filters[i]->Modified();
    }
  m_Cast->Modified();
}

template< typename TInputImage, typename TOutputImage, typename TFilter >
void
MiniPipelineSeparableImageFilter< TInputImage, TOutputImage, TFilter >
::SetNumberOfThreads(ThreadIdType nb)
{
  Superclass::SetNumberOfThreads(nb);
  for ( unsigned i = 0; i < ImageDimension; i++ )
    {
    m_Filters[i]->SetNumberOfThreads(nb);
    }
  m_Cast->SetNumberOfThreads(nb);
}

template< typename TInputImage, typename TOutputImage, typename TFilter >
void
MiniPipelineSeparableImageFilter< TInputImage, TOutputImage, TFilter >
::SetRadius(const RadiusType & radius)
{
  Superclass::SetRadius(radius);

  // set up the kernels
  for ( unsigned i = 0; i < ImageDimension; i++ )
    {
    RadiusType rad;
    rad.Fill(0);
    rad[i] = radius[i];
    m_Filters[i]->SetRadius(rad);
    }
}

template< typename TInputImage, typename TOutputImage, typename TFilter >
void
MiniPipelineSeparableImageFilter< TInputImage, TOutputImage, TFilter >
::GenerateData()
{
  this->AllocateOutputs();

  // set up the pipeline
  m_Filters[0]->SetInput( this->GetInput() );

  // Create a process accumulator for tracking the progress of this minipipeline
  ProgressAccumulator::Pointer progress = ProgressAccumulator::New();
  progress->SetMiniPipelineFilter(this);
  for ( unsigned i = 0; i < ImageDimension; i++ )
    {
    progress->RegisterInternalFilter(m_Filters[i], 1.0 / ImageDimension);
    }

  m_Cast->GraftOutput( this->GetOutput() );
  m_Cast->Update();
  this->GraftOutput( m_Cast->GetOutput() );
}
}

#endif
