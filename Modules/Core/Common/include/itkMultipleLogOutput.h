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
#ifndef itkMultipleLogOutput_h
#define itkMultipleLogOutput_h

#include "itkLogOutput.h"

#include <fstream>
#include <set>

namespace itk
{
/** \class MultipleLogOutput
 * \brief Allows writing simultaneously to multiple streams. Note that the
 *        class derives from std::streambuf and contains a
 *        std::set<> of LogOutput.
 *
 * \author Hee-Su Kim, Compute Science Dept. Kyungpook National University,
 *                     ISIS Center, Georgetown University.
 *
 * \ingroup OSSystemObjects LoggingObjects
 * \ingroup ITKCommon
 */

class ITKCommon_EXPORT MultipleLogOutput : public LogOutput
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(MultipleLogOutput);

  using Self = MultipleLogOutput;
  using Superclass = LogOutput;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  using OutputType = LogOutput;

  itkTypeMacro(MultipleLogOutput, LogOutput);
  itkNewMacro(MultipleLogOutput);

public:
  /** Register a additional output stream into the list of LogOutputs to write
   * to. The messages will be sent to the streams in the same order that the
   * streams have been added here.  */
  void
  AddLogOutput(OutputType * output);

  /** Broadcast a flush operation to all the output streams */
  void
  Flush() override;

  /** Write to multiple outputs */
  void
  Write(double timestamp) override;

  /** Write to multiple outputs */
  void
  Write(const std::string & content) override;

  /** Write to a buffer */
  void
  Write(const std::string & content, double timestamp) override;

protected:
  /** Constructor */
  MultipleLogOutput();

  /** Destructor */
  ~MultipleLogOutput() override;

private:
  using ContainerType = std::set<OutputType::Pointer>;

  ContainerType m_Output;
};
} // namespace itk

#endif // itkMultipleLogOutput_h
