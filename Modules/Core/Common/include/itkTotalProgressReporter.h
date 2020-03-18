/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkTotalProgressReporter_h
#define itkTotalProgressReporter_h

#include "itkIntTypes.h"
#include "itkProcessObject.h"

namespace itk
{
/** \class TotalProgressReporter
 *  \brief A progress reporter for concurrent threads.
 *
 * Each thread should construct their own instance of the class. The ProcessObject::IncrementProgress method will be
 * called to update the progress from all threads. The ProcessObject's method will automatically create ProgressEvents
 * when the pipeline invoking thread updates the progress.
 *
 * All threads concurrently contribute parts of the progress to the *total* number of pixels for all threads. This
 * report will update the progress after a sufficient number of pixel to meet the numberOfUpdates requirement between
 * all threads. Also when the object is deconstructed, all remaining pixels will increment the progress.
 *
 *
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT TotalProgressReporter
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(TotalProgressReporter);

  /** \brief Construct a TotalProgressReporter
   *
   * @param filter - the ProcessObject which whose progress will be updated. If nullptr then no updates will occur.
   * @param totalNumberOfPixels - the number of pixels between all threads and chunks that will be updated
   * @param numberOfUpdates - controls how often the ProcessObject's progress will be incremented
   * @param progressWeight - A percentage of the filters progress, this total number of pixels will contribute
   */
  TotalProgressReporter(ProcessObject * filter,
                        SizeValueType   totalNumberOfPixels,
                        SizeValueType   numberOfUpdates = 100,
                        float           progressWeight = 1.0f);

  /** Destructor sets progress to 1 because the filter has finished.  */
  ~TotalProgressReporter();

  /** Check if the filter has the ProcessObject::AbortGenerateData
   * flag set. If true, then a ProcessAborted exception will be thrown.
   */
  void
  CheckAbortGenerateData()
  {
    // all threads needs to check the abort flag
    if (m_Filter && m_Filter->GetAbortGenerateData())
    {
      std::string    msg;
      ProcessAborted e(__FILE__, __LINE__);
      msg += "Object " + std::string(m_Filter->GetNameOfClass()) + ": AbortGenerateDataOn";
      e.SetDescription(msg);
      throw e;
    }
  }

  /** Called by a filter once per pixel.  */
  void
  CompletedPixel()
  {
    // Inline implementation for efficiency.
    if (--m_PixelsBeforeUpdate == 0)
    {
      m_PixelsBeforeUpdate = m_PixelsPerUpdate;
      m_CurrentPixel += m_PixelsPerUpdate;

      if (m_Filter)
      {
        m_Filter->IncrementProgress(m_PixelsPerUpdate * m_InverseNumberOfPixels * m_ProgressWeight);

        this->CheckAbortGenerateData();
      }
    }
  }


  /** Called by a filter when a chunk, region, scan-line, etc. is completed. */
  void
  Completed(SizeValueType count)
  {

    if (count >= m_PixelsBeforeUpdate)
    {
      SizeValueType total = static_cast<SizeValueType>(m_PixelsPerUpdate - m_PixelsBeforeUpdate) + count;
      SizeValueType numberOfUpdates = total / m_PixelsPerUpdate;

      m_PixelsBeforeUpdate = m_PixelsPerUpdate - total % m_PixelsPerUpdate;
      m_CurrentPixel += numberOfUpdates * m_PixelsPerUpdate;

      if (m_Filter)
      {
        m_Filter->IncrementProgress(numberOfUpdates * m_PixelsPerUpdate * m_InverseNumberOfPixels * m_ProgressWeight);

        this->CheckAbortGenerateData();
      }
    }
    else
    {
      m_PixelsBeforeUpdate -= count;
    }
  }

protected:
  ProcessObject * m_Filter;
  float           m_InverseNumberOfPixels;
  SizeValueType   m_CurrentPixel;
  SizeValueType   m_PixelsPerUpdate;
  SizeValueType   m_PixelsBeforeUpdate;
  float           m_ProgressWeight;
};
} // end namespace itk

#endif
