/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWatershedImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef _itkWatershedImageFilter_txx
#define _itkWatershedImageFilter_txx

namespace itk
{

template< class TInputImage >
void
WatershedImageFilter<TInputImage>
::SetThreshold(double val)
{
  if (val != m_Threshold) { this->Modified(); }
  
  if ( m_Threshold < 0.0 )      { m_Threshold = 0.0; }
  else if ( m_Threshold > 1.0 ) { m_Threshold = 1.0; }
  else { m_Threshold = val; }

  if (m_Segmenter->GetThreshold() != m_Threshold)
    {  m_Segmenter->SetThreshold( m_Threshold ); }
}

template< class TInputImage >
void
WatershedImageFilter<TInputImage>
::SetLevel(double val)
{
  if (val != m_Level) { this->Modified(); }
  
  if ( m_Level < 0.0 )      { m_Level = 0.0; }
  else if ( m_Level > 1.0 ) { m_Level = 1.0; }
  else { m_Level = val; }

  if (m_TreeGenerator->GetFloodLevel() != m_Level)
    {  m_TreeGenerator->SetFloodLevel( m_Level ); }
  
  if (m_Relabeler->GetFloodLevel() != m_Level)
    {  m_Relabeler->SetFloodLevel( m_Level ); }
}
  
template< class TInputImage >
WatershedImageFilter<TInputImage>
::WatershedImageFilter() :  m_Threshold(0.0), m_Level(0.0)
{
  // Set up the mini-pipeline for the first execution.
  m_Segmenter    = watershed::Segmenter<InputImageType>::New();
  m_TreeGenerator= watershed::SegmentTreeGenerator<ScalarType>::New();
  m_Relabeler    = watershed::Relabeler<ScalarType, ImageDimension>::New();
    
  m_Segmenter->SetDoBoundaryAnalysis( false );
  m_Segmenter->SetSortEdgeLists(true);
  m_Segmenter->SetThreshold( this->GetThreshold() );

  m_TreeGenerator->SetInputSegmentTable( m_Segmenter->GetSegmentTable() );
  m_TreeGenerator->SetMerge( false );
  m_TreeGenerator->SetFloodLevel( this->GetLevel() );

  m_Relabeler->SetInputSegmentTree( m_TreeGenerator->GetOutputSegmentTree() );
  m_Relabeler->SetInputImage( m_Segmenter->GetOutputImage() );
  m_Relabeler->SetFloodLevel( this->GetLevel() );
}
  
template< class TInputImage >
void
WatershedImageFilter<TInputImage>
::GenerateData()
{
  WatershedMiniPipelineProgressCommand::Pointer c =
    WatershedMiniPipelineProgressCommand::New();
  c->SetFilter(this);
  c->SetNumberOfFilters(3.0);

  m_Segmenter->AddObserver(ProgressEvent(), c);
  m_TreeGenerator->AddObserver(ProgressEvent(), c);
  m_Relabeler->AddObserver(ProgressEvent(), c);
  
  // Allocate the output image.
  OutputImageType::Pointer output = this->GetOutput();
  output->SetBufferedRegion(output->GetRequestedRegion());
  output->Allocate();

  // Complete any necessary set up of the mini-pipeline.  We have to be careful 
  // not to cause time stamps to be updated unneccessarily.  Specifically, we
  // don't want the SegmentTreeGenerator to execute unless it is really
  // needed, since it is the bottleneck.
  if (this->GetThreshold() != m_Segmenter->GetThreshold() )
    {
      m_Segmenter->SetThreshold( this->GetThreshold() ) ;
    }
  
  if (this->GetLevel()  > m_TreeGenerator->GetFloodLevel() )
    {
      // SegmentTreeGenerator::SetFloodLevel has logic to determine
      // whether or not its Modified() method will be called.
      m_TreeGenerator->SetFloodLevel( this->GetLevel() );
    }

  m_Relabeler->SetFloodLevel( this->GetLevel() );
  m_Relabeler->GraftOutput( this->GetOutput() );
  m_Relabeler->Update();

  this->GraftOutput( m_Relabeler->GetOutputImage() );
}

template<class TInputImage>
void 
WatershedImageFilter<TInputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
}

void WatershedMiniPipelineProgressCommand
::Execute(Object *caller, const EventObject &event)
{
  ProcessObject *po = dynamic_cast<ProcessObject *>(caller);
  if (! po) return;
  
  if( typeid(event) == typeid ( ProgressEvent)  )
    {
      m_Filter->UpdateProgress( (m_Count + po->GetProgress()) /
                                  m_NumberOfFilters);
      if (po->GetProgress() == 1.0) m_Count += 1.0;
    }
}

void WatershedMiniPipelineProgressCommand
::Execute(const Object *caller, const EventObject &event)
{
  ProcessObject *po = dynamic_cast<ProcessObject *>(const_cast<Object *>(caller));
  if (! po) return;
  
  if( typeid(event) == typeid ( ProgressEvent)  )
    {
      m_Filter->UpdateProgress( (m_Count + po->GetProgress()) /
                                  m_NumberOfFilters);
      if (po->GetProgress() == 1.0) m_Count += 1.0;
    }
}

} // end namespace itk

#endif
