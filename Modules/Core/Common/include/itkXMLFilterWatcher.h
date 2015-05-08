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
#ifndef itkXMLFilterWatcher_h
#define itkXMLFilterWatcher_h

#include "itkSimpleFilterWatcher.h"

namespace itk
{
/** \class XMLFilterWatcher
 * \brief Simple mechanism for monitoring the pipeline events of a
 * filter and reporting these events to std::cout. Formats reports
 * with xml.
 * \ingroup ITKCommon
 */
class  ITKCommon_EXPORT XMLFilterWatcher
  : public SimpleFilterWatcher
{
public:
  XMLFilterWatcher(ProcessObject *o, const char *comment = ""):
    SimpleFilterWatcher(o, comment) {}

protected:

  /** Callback method to show the ProgressEvent */
  virtual void ShowProgress();

  /** Callback method to show the StartEvent */
  virtual void StartFilter();

  /** Callback method to show the EndEvent */
  virtual void EndFilter();
};
} // end namespace itk

#endif
