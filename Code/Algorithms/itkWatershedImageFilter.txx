/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWatershedImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkWatershedImageFilter_txx
#define _itkWatershedImageFilter_txx
#include "itkWatershedImageFilter.h"

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
::WatershedImageFilter() :  m_Threshold(0.0), m_Level(0.0), m_FirstExecution(true)
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

  WatershedMiniPipelineProgressCommand::Pointer c =
    WatershedMiniPipelineProgressCommand::New();
  c->SetFilter(this);
  c->SetNumberOfFilters(3);

  m_Segmenter->AddObserver(ProgressEvent(), c);
  m_ObserverTag = m_TreeGenerator->AddObserver(ProgressEvent(), c);
  m_Relabeler->AddObserver(ProgressEvent(), c);
}
  
template< class TInputImage >
void
WatershedImageFilter<TInputImage>
::EnlargeOutputRequestedRegion(DataObject *data)
{
  Superclass::EnlargeOutputRequestedRegion(data);
  data->SetRequestedRegionToLargestPossibleRegion();
}

template< class TInputImage >
void
WatershedImageFilter<TInputImage>
::GenerateData()
{
  // Allocate the output image.
  typename OutputImageType::Pointer output = this->GetOutput();
  output->SetBufferedRegion(output->GetRequestedRegion());
  output->Allocate();

  // Set the largest possible region in the segmenter
  m_Segmenter->SetLargestPossibleRegion(this->GetInput()->GetLargestPossibleRegion());
  m_Segmenter->GetOutputImage()->SetRequestedRegion(this->GetInput()->GetLargestPossibleRegion());
  
  // Fiddle with the update command to accomodate filters that will and will
  // not execute.  This logic is not guaranteed to cover all update cases.
  //if (m_FirstExecution == false)
  //    {      
  //      unsigned long filtercount = 0;
  WatershedMiniPipelineProgressCommand::Pointer c =
    dynamic_cast<WatershedMiniPipelineProgressCommand *>(
      m_TreeGenerator->GetCommand(m_ObserverTag) ); 
  c->SetCount(0.0);
  c->SetNumberOfFilters(3);
      
  //      if (m_Segmenter->GetMTime() > this->GetMTime() )
  //        { filtercount++; }
  //      if (m_TreeGenerator->GetMTime() > this->GetMTime() )
  //        { filtercount++; }
  //      if (m_Relabeler->GetMTime() > this->GetMTime() )
  //        { filtercount++; }
  //      c->SetNumberOfFilters(filtercount);
  //    }
  //  else m_FirstExecution = false;
  
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
  os << indent << "Threshold: " << m_Threshold << std::endl;
  os << indent << "Level: " << m_Level << std::endl;
}

} // end namespace itk

#endif
