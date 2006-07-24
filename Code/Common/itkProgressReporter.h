/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkProgressReporter.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkProgressReporter_h
#define __itkProgressReporter_h

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
 */
class ITKCommon_EXPORT ProgressReporter
{
public:
  /** Constructor sets progress to 0 because the filter is starting.  */
  ProgressReporter(ProcessObject* filter, int threadId,
                   unsigned long numberOfPixels,
                   unsigned long numberOfUpdates = 100,
                   float initialProgress = 0.0f,
                   float progressWeight  = 1.0f );
  
  /** Destructor sets progress to 1 because the filter has finished.  */
  ~ProgressReporter();
  
  /** Called by a filter once per pixel.  */
  void CompletedPixel()
    {
    // Inline implementation for efficiency.
    if(--m_PixelsBeforeUpdate == 0)
      {
      m_PixelsBeforeUpdate = m_PixelsPerUpdate;
      m_CurrentPixel += m_PixelsPerUpdate;
      // only thread 0 should update the progress of the filter
      if (m_ThreadId == 0)
        {
        m_Filter->UpdateProgress(
          m_CurrentPixel * m_InverseNumberOfPixels * m_ProgressWeight + m_InitialProgress);
        }
      // all threads needs to check the abort flag
      if( m_Filter->GetAbortGenerateData() )
        {
        std::string msg;
        ProcessAborted e(__FILE__, __LINE__);
        msg += "Object " + std::string(m_Filter->GetNameOfClass()) + ": AbortGenerateDataOn";
        e.SetDescription(msg);
        throw e;
        }
      }
    }

protected:
  ProcessObject* m_Filter;
  int m_ThreadId;
  float m_InverseNumberOfPixels;
  unsigned long m_CurrentPixel;
  unsigned long m_PixelsPerUpdate;
  unsigned long m_PixelsBeforeUpdate;
  float  m_InitialProgress;
  float  m_ProgressWeight;

private:
  ProgressReporter(); //purposely not implemented

};

} // end namespace itk

#endif
