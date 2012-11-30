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
#include "itkProgressReporter.h"

namespace itk
{
//----------------------------------------------------------------------------
ProgressReporter::ProgressReporter(ProcessObject *filter, ThreadIdType threadId,
                                   SizeValueType numberOfPixels,
                                   SizeValueType numberOfUpdates,
                                   float initialProgress,
                                   float progressWeight):
  m_Filter(filter),
  m_ThreadId(threadId),
  m_CurrentPixel(0),
  m_InitialProgress(initialProgress),
  m_ProgressWeight(progressWeight)
{
  // Make sure we have at least one pixel.
  const float numPixels = (numberOfPixels > 0) ? static_cast<float>(numberOfPixels) : 1.0F;
  // We cannot update more times than there are pixels.
  const float numUpdates = (numberOfUpdates>numberOfPixels) ? numPixels : static_cast<float>(numberOfUpdates);

  // Calculate the interval for updates.
  m_PixelsPerUpdate = static_cast< SizeValueType >( numPixels / numUpdates );
  m_InverseNumberOfPixels = 1.0f / numPixels;

  // Only thread 0 should update progress. (But all threads need to
  // count pixels so they can check the abort flag.)
  if ( m_ThreadId == 0 )
    {
    // Set the progress to initial progress.  The filter is just starting.
    m_Filter->UpdateProgress(m_InitialProgress);
    }

  m_PixelsBeforeUpdate = m_PixelsPerUpdate;
}

//----------------------------------------------------------------------------
ProgressReporter::~ProgressReporter()
{
  // Only thread 0 should update progress.
  if ( m_ThreadId == 0 )
    {
    // Set the progress to the end of its current range.  The filter has
    // finished.
    m_Filter->UpdateProgress(m_InitialProgress + m_ProgressWeight);
    }
}
} // end namespace itk
