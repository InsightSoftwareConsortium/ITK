/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkProgressAccumulator.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkProgressAccumulator.h"

namespace itk {

ProgressAccumulator
::ProgressAccumulator()
{
  m_MiniPipelineFilter = 0;

  // Initialize the progress values
  this->ResetProgress();

  // Create a member command
  m_CallbackCommand = CommandType::New();
  m_CallbackCommand->SetCallbackFunction( this, & Self::ReportProgress );
}

ProgressAccumulator
::~ProgressAccumulator()
{
  UnregisterAllFilters();
}

void
ProgressAccumulator
::RegisterInternalFilter(GenericFilterType *filter,float weight)
{
  // Observe the filter
  unsigned long tag = 
    filter->AddObserver(ProgressEvent(), m_CallbackCommand);
  
  // Create a record for the filter
  struct FilterRecord record;
  record.Filter = filter;
  record.Weight = weight;
  record.ObserverTag = tag;
  record.Progress = 0.0f;

  // Add the record to the list
  m_FilterRecord.push_back(record);
}

void 
ProgressAccumulator
::UnregisterAllFilters()
{
  // The filters should no longer be observing us
  FilterRecordVector::iterator it;
  for(it = m_FilterRecord.begin();it!=m_FilterRecord.end();++it)
    {
    it->Filter->RemoveObserver(it->ObserverTag);
    }

  // Clear the filter array
  m_FilterRecord.clear();

  // Reset the progress meter
  ResetProgress();
}

void 
ProgressAccumulator
::ResetProgress()
{
  // Reset the accumulated progress
  m_AccumulatedProgress = 0.0f;
  
  // Reset each of the individial progress meters 
  FilterRecordVector::iterator it;
  for(it = m_FilterRecord.begin();it!=m_FilterRecord.end();++it)
    {
    it->Progress = 0.0f;
    }
}

void 
ProgressAccumulator
::ReportProgress(Object *object, const EventObject &event)
{
  const ProcessObject * internalFilter = 
    dynamic_cast<const ProcessObject *>( object );

  ProgressEvent p;
  if( typeid( event ) == typeid( p ) )
    {
    // Add up the progress from different filters
    m_AccumulatedProgress = 0.0f;

    FilterRecordVector::iterator it;
    for(it = m_FilterRecord.begin();it!=m_FilterRecord.end();++it)
      {
      // Record the progress if it's from one of the registered filters
      if(it->Filter == internalFilter)
        {
        it->Progress = internalFilter->GetProgress();
        }
      
      m_AccumulatedProgress += it->Progress * it->Weight;
      }

    // Update the progress of the client mini-pipeline filter
    m_MiniPipelineFilter->UpdateProgress(m_AccumulatedProgress);
    }
}

void ProgressAccumulator
::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  if (m_MiniPipelineFilter)
    {
    os << indent << m_MiniPipelineFilter << std::endl;
    }
  os << indent << m_AccumulatedProgress << std::endl;
}

} // End namespace itk


