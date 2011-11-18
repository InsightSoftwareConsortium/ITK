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
#include "itkIterationReporter.h"

namespace itk
{
//----------------------------------------------------------------------------
IterationReporter::IterationReporter(ProcessObject *filter, ThreadIdType threadId,
                                     unsigned long stepsPerUpdate):
  m_Filter(filter),
  m_ThreadId(threadId),
  m_StepsPerUpdate(stepsPerUpdate)
{
  // Only thread 0 should update progress.
  m_StepsBeforeUpdate = m_StepsPerUpdate;
}
} // end namespace itk
