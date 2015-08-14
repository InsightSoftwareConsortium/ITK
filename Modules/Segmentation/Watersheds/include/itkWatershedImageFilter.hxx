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
#ifndef itkWatershedImageFilter_hxx
#define itkWatershedImageFilter_hxx
#include "itkWatershedImageFilter.h"

namespace itk
{
template< typename TInputImage >
void
WatershedImageFilter< TInputImage >
::SetThreshold(double val)
{
  if ( val < 0.0 )
    {
    val = 0.0;
    }
  else if ( val > 1.0 )
    {
    val = 1.0;
    }

CLANG_PRAGMA_PUSH
CLANG_SUPPRESS_Wfloat_equal
  if ( val != m_Threshold )
CLANG_PRAGMA_POP
    {
    m_Threshold = val;
    m_Segmenter->SetThreshold(m_Threshold);

    m_ThresholdChanged = true;
    this->Modified();
    }
}

template< typename TInputImage >
void
WatershedImageFilter< TInputImage >
::SetLevel(double val)
{
  if ( val < 0.0 )
    {
    val = 0.0;
    }
  else if ( val > 1.0 )
    {
    val = 1.0;
    }

CLANG_PRAGMA_PUSH    \
CLANG_SUPPRESS_Wfloat_equal   \
  if ( val != m_Level )
CLANG_PRAGMA_POP    \
    {
    m_Level = val;
    m_TreeGenerator->SetFloodLevel(m_Level);
    m_Relabeler->SetFloodLevel(m_Level);

    m_LevelChanged = true;
    this->Modified();
    }
}

template< typename TInputImage >
WatershedImageFilter< TInputImage >
::WatershedImageFilter():m_Threshold(0.0), m_Level(0.0)
{
  // Set up the mini-pipeline for the first execution.
  m_Segmenter    = watershed::Segmenter< InputImageType >::New();
  m_TreeGenerator = watershed::SegmentTreeGenerator< ScalarType >::New();
  m_Relabeler    = watershed::Relabeler< ScalarType, ImageDimension >::New();

  m_Segmenter->SetDoBoundaryAnalysis(false);
  m_Segmenter->SetSortEdgeLists(true);
  m_Segmenter->SetThreshold( this->GetThreshold() );

  m_TreeGenerator->SetInputSegmentTable( m_Segmenter->GetSegmentTable() );
  m_TreeGenerator->SetMerge(false);
  m_TreeGenerator->SetFloodLevel( this->GetLevel() );

  m_Relabeler->SetInputSegmentTree( m_TreeGenerator->GetOutputSegmentTree() );
  m_Relabeler->SetInputImage( m_Segmenter->GetOutputImage() );
  m_Relabeler->SetFloodLevel( this->GetLevel() );

  WatershedMiniPipelineProgressCommand::Pointer c =
    WatershedMiniPipelineProgressCommand::New();
  c->SetFilter(this);
  c->SetNumberOfFilters(3);

  m_Segmenter->AddObserver(ProgressEvent(), c);
  m_ObserverTag = m_TreeGenerator->AddObserver(ProgressEvent(), c);
  m_Relabeler->AddObserver(ProgressEvent(), c);

  m_InputChanged = true;
  m_LevelChanged = true;
  m_ThresholdChanged = true;
}

template< typename TInputImage >
void
WatershedImageFilter< TInputImage >
::EnlargeOutputRequestedRegion(DataObject *data)
{
  Superclass::EnlargeOutputRequestedRegion(data);
  data->SetRequestedRegionToLargestPossibleRegion();
}

template< typename TInputImage >
void
WatershedImageFilter< TInputImage >
::PrepareOutputs()
{
  // call the superclass' method to clear out the outputs
  Superclass::PrepareOutputs();

  // clear out the temporary storage of the mini-pipeline as necessary
  //
  //

  // If input changed, then Segmenter + Tree Generator + Relabeler need
  // to re-execute.  Plus, the HighestCalculatedFloodLevel must be reset
  // on the Tree Generator.
  //
  // If the threshold changed, then Segmenter + Tree Generator +
  // Relabeler need to re-execute.  Plus, the
  // HighestCalculatedFloodLevel must be reset on the Tree Generator
  //
  if ( m_InputChanged
       || ( this->GetInput()->GetPipelineMTime() > m_GenerateDataMTime )
       || m_ThresholdChanged )
    {
    m_Segmenter->PrepareOutputs();
    m_TreeGenerator->PrepareOutputs();
    m_Relabeler->PrepareOutputs();

    m_TreeGenerator->SetHighestCalculatedFloodLevel(0.0);
    }

  // If the flood level changed but is below the Tree
  // Generator::HighestCalculatedFloodLevel, then only the Relabeler
  // must execute.
  //
  // If the flood level changed and is above the Tree
  // Generator::HighestCalculatedFloodLevel, then the Tree Generator +
  // Relabeler must execute.
  //
  if ( m_LevelChanged )
    {
    if ( m_Level <= m_TreeGenerator->GetHighestCalculatedFloodLevel() )
      {
      m_Relabeler->PrepareOutputs();
      }
    else
      {
      m_TreeGenerator->PrepareOutputs();
      m_Relabeler->PrepareOutputs();
      }
    }
}

template< typename TInputImage >
void
WatershedImageFilter< TInputImage >
::GenerateData()
{
  // Set the largest possible region in the segmenter
  m_Segmenter->SetLargestPossibleRegion( this->GetInput()
                                         ->GetLargestPossibleRegion() );
  m_Segmenter->GetOutputImage()
  ->SetRequestedRegion( this->GetInput()->GetLargestPossibleRegion() );

  // Setup the progress command
  WatershedMiniPipelineProgressCommand::Pointer c =
    dynamic_cast< WatershedMiniPipelineProgressCommand * >(
      m_TreeGenerator->GetCommand(m_ObserverTag) );
  c->SetCount(0.0);
  c->SetNumberOfFilters(3);

  // Graft our output on the relabeler
  m_Relabeler->GraftOutput( this->GetOutput() );

  // Update the mini-pipeline
  m_Relabeler->Update();

  // Graft the output of the relabeler back on this filter
  this->GraftOutput( m_Relabeler->GetOutputImage() );

  // Keep track of when we last executed
  m_GenerateDataMTime.Modified();

  // Clear flags
  m_InputChanged = false;
  m_LevelChanged = false;
  m_ThresholdChanged = false;
}

template< typename TInputImage >
void
WatershedImageFilter< TInputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Threshold: " << m_Threshold << std::endl;
  os << indent << "Level: " << m_Level << std::endl;
}
} // end namespace itk

#endif
