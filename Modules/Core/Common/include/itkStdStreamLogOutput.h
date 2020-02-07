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
#ifndef itkStdStreamLogOutput_h
#define itkStdStreamLogOutput_h

#include <iostream>
#include <string>

#include <mutex>
#include "itkLogOutput.h"

namespace itk
{
/** \class StdStreamLogOutput
 *  \brief Represents a standard stream output stream.
 *
 *  This class provides thread safety for the standard stream output stream.
 *
 * \author Hee-Su Kim, Compute Science Dept. Kyungpook National University,
 *                     ISIS Center, Georgetown University.
 *
 *
 *  \ingroup OSSystemObjects LoggingObjects
 * \ingroup ITKCommon
 */

class ITKCommon_EXPORT StdStreamLogOutput : public LogOutput
{
public:
  using Self = StdStreamLogOutput;
  using Superclass = LogOutput;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using StreamType = std::ostream;

  using StreamPointerType = std::ostream *;

  itkTypeMacro(StdStreamLogOutput, LogOutput);

  itkNewMacro(StdStreamLogOutput);

  itkGetConstMacro(Stream, StreamPointerType);

  /** Set a standard stream pointer */
  void
  SetStream(StreamType & Stream);

  /** flush a buffer */
  void
  Flush() override;

  /** Write to multiple outputs */
  void
  Write(double timestamp) override;

  /** Write to a buffer */
  void
  Write(std::string const & content) override;

  /** Write to a buffer */
  void
  Write(std::string const & content, double timestamp) override;

protected:
  /** Constructor */
  StdStreamLogOutput();

  /** Destructor */
  ~StdStreamLogOutput() override;

  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  StreamPointerType m_Stream;

  std::mutex m_Mutex;
};
} // namespace itk

#endif // itkStdStreamLogOutput_h
