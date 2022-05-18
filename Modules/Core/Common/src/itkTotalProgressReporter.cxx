/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#include "itkTotalProgressReporter.h"
#include "itkMultiThreaderBase.h"

namespace itk
{
//----------------------------------------------------------------------------
TotalProgressReporter::TotalProgressReporter(ProcessObject * filter,
                                             SizeValueType   totalNumberOfPixels,
                                             SizeValueType   numberOfUpdates,
                                             float           progressWeight)
  : m_Filter(filter)
  , m_CurrentPixel(0)
  , m_ProgressWeight(progressWeight)
{
  // Make sure we have at least one pixel.
  const float numPixels = std::max(static_cast<float>(totalNumberOfPixels), 1.0f);

  // We cannot update more times than there are pixels.
  const float numUpdates = std::min(numPixels, static_cast<float>(numberOfUpdates));

  // Calculate the interval for updates.
  m_PixelsPerUpdate = static_cast<SizeValueType>(numPixels / numUpdates);
  m_InverseNumberOfPixels = 1.0f / numPixels;

  m_PixelsBeforeUpdate = m_PixelsPerUpdate;

  if (m_Filter)
  {
    // this class will report progress, avoid double reporting
    m_Filter->GetMultiThreader()->SetUpdateProgress(false);
  }
}

//----------------------------------------------------------------------------
TotalProgressReporter::~TotalProgressReporter()
{
  SizeValueType pixelRemnants = m_PixelsPerUpdate - m_PixelsBeforeUpdate;

  if (pixelRemnants != 0 && m_Filter)
  {
    m_Filter->IncrementProgress(pixelRemnants * m_InverseNumberOfPixels * m_ProgressWeight);
  }

  if (m_Filter)
  {
    // reset the original state of ThreaderUpdateProgress
    m_Filter->GetMultiThreader()->SetUpdateProgress(m_Filter->GetThreaderUpdateProgress());
  }
}
} // end namespace itk
