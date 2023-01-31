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

#include "itkMultipleLogOutput.h"

namespace itk
{
MultipleLogOutput::MultipleLogOutput() = default;

MultipleLogOutput::~MultipleLogOutput() = default;

/** Adds an output stream to the MultipleLogOutput for writing. */
void
MultipleLogOutput::AddLogOutput(OutputType * output)
{
  this->m_Output.insert(output); // insert the address
}

/** The Flush method flushes all the streams. */
void
MultipleLogOutput::Flush()
{
  for (const auto & output : m_Output)
  {
    output->Flush();
  }
}

/** Write to multiple outputs */
void
MultipleLogOutput::Write(double timestamp)
{
  for (const auto & output : m_Output)
  {
    output->Write(timestamp);
  }
}

/** Write to multiple outputs */
void
MultipleLogOutput::Write(const std::string & content)
{
  for (const auto & output : m_Output)
  {
    output->Write(content);
  }
}

/** Write to a buffer */
void
MultipleLogOutput::Write(const std::string & content, double timestamp)
{
  for (const auto & output : m_Output)
  {
    output->Write(content, timestamp);
  }
}
} // namespace itk
