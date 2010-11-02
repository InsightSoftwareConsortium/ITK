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
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkCommand.h"
#include "itkDirectory.h"
#include "itkLightProcessObject.h"
#include "itkProcessObject.h"
#include "itkOutputWindow.h"
#include "itkVersion.h"
#include "itkTimeStamp.h"
#include "itkStringStream.h"
#include "itkDynamicLoader.h"

#ifdef CABLE_CONFIGURATION
#include "itkCSwigMacros.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(ITKCommonBase);
  namespace wrappers
  {
    ITK_WRAP_OBJECT(Command);
    ITK_WRAP_OBJECT(DataObject);
    ITK_WRAP_OBJECT(Directory);
    ITK_WRAP_OBJECT(DynamicLoader);
    ITK_WRAP_OBJECT(LightObject);
    ITK_WRAP_OBJECT(Object);
    ITK_WRAP_OBJECT(ObjectFactoryBase);
    ITK_WRAP_OBJECT(LightProcessObject);
    ITK_WRAP_OBJECT(ProcessObject);
    ITK_WRAP_OBJECT(OutputWindow);
    ITK_WRAP_OBJECT(Version);
    typedef itk::TimeStamp itkTimeStamp;
    typedef itk::StringStream itkStringStream;
  }
}


#endif
