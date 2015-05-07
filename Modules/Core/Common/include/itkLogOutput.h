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
#ifndef itkLogOutput_h
#define itkLogOutput_h

#include "itkMacro.h"
#include "itkObject.h"
#include "itkObjectFactory.h"

#include <string>

namespace itk
{
/** \class LogOutput
 * \brief Represents an output stream.
 *
 *  \author Hee-Su Kim, Compute Science Dept. Kyungpook National University,
 *                      ISIS Center, Georgetown University.
 *
 * \ingroup OSSystemObjects LoggingObjects
 * \ingroup ITKCommon
 */

class ITKCommon_EXPORT LogOutput:public Object
{
public:

  typedef LogOutput                  Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** flush a buffer */
  virtual void Flush() = 0;

  /** Write to a buffer */
  virtual void Write(double timestamp) = 0;

  /** Write to a buffer */
  virtual void Write(const std::string & content) = 0;

  /** Write to a buffer */
  virtual void Write(const std::string & content, double timestamp) = 0;

protected:

  /** Destructor */
  LogOutput();

  /** Destructor */
  virtual ~LogOutput();
};
}

#endif //itkLogOutput_h
