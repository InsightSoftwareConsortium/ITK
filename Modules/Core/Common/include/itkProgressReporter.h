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
#ifndef itkProgressReporter_h
#define itkProgressReporter_h

#include "itkIntTypes.h"
#include "itkProcessObject.h"

namespace itk
{
/** \class ProgressReporter
 * \brief Implements progress tracking for a filter.
 *
 * This is a utility class for use by filter implementations in
 * GenerateData() and ThreadedGenerateData().
 *
 * The class constructor sets the progress to 0, and the destructor
 * sets it to 1.  In between, there should be one call to
 * CompletedPixel() per pixel.  The reporter will automatically update
 * the filter's progress at an interval resulting in the specified
 * number of updates.  The default number of updates is 100.
 *
 * Example usage:
 *
 * \code
 *   ProgressReporter progress(this, threadId,
 *                             threadRegion.GetNumberOfPixels(), 100);
 *   for( each pixel )
 *     {
 *     ...
 *     progress.CompletedPixel();
 *     }
 * \endcode
 *
 * When used in a non-threaded filter, the threadId argument should be 0.
 *
 * \sa
 * This class is a tool for filter implementers to equip a filter to
 * report on its progress.  For information on how to acquire this
 * progress information, see:
 *  - ProcessObject::ReportProgress()
 *  - Object::AddObserver()
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT ProgressReporter
{
public:
  /** Constructor sets progress to 0 because the filter is starting.  */
  ProgressReporter(ProcessObject *filter, ThreadIdType threadId,
                   SizeValueType numberOfPixels,
                   SizeValueType numberOfUpdates = 100,
                   float initialProgress = 0.0f,
                   float progressWeight  = 1.0f);

  /** Destructor sets progress to 1 because the filter has finished.  */
  ~ProgressReporter();

  /** Called by a filter once per pixel.  */
  void CompletedPixel()
  {
    // Inline implementation for efficiency.
    if ( --m_PixelsBeforeUpdate == 0 )
      {
      m_PixelsBeforeUpdate = m_PixelsPerUpdate;
      m_CurrentPixel += m_PixelsPerUpdate;
      // only thread 0 should update the progress of the filter
      if ( m_ThreadId == 0 )
        {
        m_Filter->UpdateProgress(
          static_cast<float>(m_CurrentPixel) * m_InverseNumberOfPixels * m_ProgressWeight + m_InitialProgress);
        }
      // all threads needs to check the abort flag
      if ( m_Filter->GetAbortGenerateData() )
        {
        std::string    msg;
        ProcessAborted e(__FILE__, __LINE__);
        msg += "Object " + std::string( m_Filter->GetNameOfClass() ) + ": AbortGenerateDataOn";
        e.SetDescription(msg);
        throw e;
        }
      }
  }

protected:
  ProcessObject *m_Filter;
  ThreadIdType   m_ThreadId;
  float          m_InverseNumberOfPixels;
  SizeValueType  m_CurrentPixel;
  SizeValueType  m_PixelsPerUpdate;
  SizeValueType  m_PixelsBeforeUpdate;
  float          m_InitialProgress;
  float          m_ProgressWeight;

private:
  ProgressReporter() ITK_DELETED_FUNCTION;
};
} // end namespace itk

#endif
