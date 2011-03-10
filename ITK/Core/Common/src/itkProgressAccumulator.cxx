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
#include "itkProgressAccumulator.h"

namespace itk
{
ProgressAccumulator
::ProgressAccumulator()
{
  m_MiniPipelineFilter = 0;

  // Initialize the progress values
  this->ResetProgress();

  // Create a member command
  m_CallbackCommand = CommandType::New();
  m_CallbackCommand->SetCallbackFunction(this, &Self::ReportProgress);
}

ProgressAccumulator
::~ProgressAccumulator()
{
  UnregisterAllFilters();
}

void
ProgressAccumulator
::RegisterInternalFilter(GenericFilterType *filter, float weight)
{
  // Observe the filter
  unsigned long progressTag =
    filter->AddObserver(ProgressEvent(), m_CallbackCommand);
  unsigned long iterationTag =
    filter->AddObserver(IterationEvent(), m_CallbackCommand);

  // Create a record for the filter
  struct FilterRecord record;

  record.Filter = filter;
  record.Weight = weight;
  record.ProgressObserverTag = progressTag;
  record.IterationObserverTag = iterationTag;
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

  for ( it = m_FilterRecord.begin(); it != m_FilterRecord.end(); ++it )
    {
    it->Filter->RemoveObserver(it->ProgressObserverTag);
    it->Filter->RemoveObserver(it->IterationObserverTag);
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
  m_AccumulatedProgress     = 0.0f;
  m_BaseAccumulatedProgress = 0.0f;

  // Reset each of the individial progress meters
  FilterRecordVector::iterator it;
  for ( it = m_FilterRecord.begin(); it != m_FilterRecord.end(); ++it )
    {
    it->Progress = 0.0f;
    it->Filter->SetProgress(0.0f);
    }
}

void
ProgressAccumulator
::ResetFilterProgressAndKeepAccumulatedProgress()
{
  m_BaseAccumulatedProgress = m_AccumulatedProgress;
  // Reset each of the individial progress meters
  FilterRecordVector::iterator it;
  for ( it = m_FilterRecord.begin(); it != m_FilterRecord.end(); ++it )
    {
    it->Progress = 0.0f;
    it->Filter->SetProgress(0.0f);
    }
}

void
ProgressAccumulator
::ReportProgress(Object *who, const EventObject & event)
{
  ProgressEvent  pe;
  IterationEvent ie;

  if ( typeid( event ) == typeid( pe ) )
    {
    // Add up the progress from different filters
    m_AccumulatedProgress = m_BaseAccumulatedProgress;

    FilterRecordVector::iterator it;
    for ( it = m_FilterRecord.begin(); it != m_FilterRecord.end(); ++it )
      {
      m_AccumulatedProgress += it->Filter->GetProgress() * it->Weight;
      }

    // Update the progress of the client mini-pipeline filter
    m_MiniPipelineFilter->UpdateProgress(m_AccumulatedProgress);

    // check for abort
    if ( m_MiniPipelineFilter->GetAbortGenerateData() )
      {
      // Abort the filter that is reporting progress
      FilterRecordVector::iterator fit;
      for ( fit = m_FilterRecord.begin(); fit != m_FilterRecord.end(); ++fit )
        {
        if ( who == fit->Filter )
          {
          fit->Filter->AbortGenerateDataOn();
          }
        }
      }
    }
  else if ( typeid( event ) == typeid( ie ) )
          {}
}

void ProgressAccumulator
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  if ( m_MiniPipelineFilter )
    {
    os << indent << m_MiniPipelineFilter << std::endl;
    }
  os << indent << m_AccumulatedProgress     << std::endl;
  os << indent << m_BaseAccumulatedProgress << std::endl;
}
} // End namespace itk
