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
#include "itkEventObject.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"
namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkEventObjectGroup);
  namespace wrappers
  {
    typedef itk::EventObject itkEventObject;
    typedef itk::NoEvent itkNoEvent;
    typedef itk::AnyEvent itkAnyEvent;
    typedef itk::DeleteEvent itkDeleteEvent;
    typedef itk::StartEvent itkStartEvent;
    typedef itk::EndEvent itkEndEvent;
    typedef itk::ProgressEvent itkProgressEvent;
    typedef itk::ExitEvent itkExitEvent;
    typedef itk::ModifiedEvent itkModifiedEvent;
    typedef itk::IterationEvent itkIterationEvent;
    typedef itk::PickEvent itkPickEvent;
    typedef itk::StartPickEvent itkStartPickEvent;
    typedef itk::EndPickEvent itkEndPickEvent;
    typedef itk::AbortCheckEvent itkAbortCheckEvent;
    typedef itk::UserEvent itkUserEvent;
  }
}
#endif
