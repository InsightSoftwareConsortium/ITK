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
#include <iostream>
#include "itkStdStreamLogOutput.h"

namespace itk
{
/** Constructor */
StdStreamLogOutput::StdStreamLogOutput()
{
  this->m_Stream = nullptr;
}

/** Destructor */
StdStreamLogOutput::~StdStreamLogOutput()
{
  if (this->m_Stream)
  {
    this->m_Stream->flush();
  }
}

/** Set file stream */
void
StdStreamLogOutput::SetStream(StreamType & Stream)
{
  this->m_Stream = &Stream;
  this->m_Stream->precision(30);
}

/** flush a buffer */
void
StdStreamLogOutput::Flush()
{
  const std::lock_guard<std::mutex> lockGuard(m_Mutex);
  if (this->m_Stream)
  {
    this->m_Stream->flush();
  }
}

/** Write to a buffer */
void
StdStreamLogOutput::Write(double timestamp)
{
  const std::lock_guard<std::mutex> lockGuard(m_Mutex);
  if (this->m_Stream)
  {
    (*this->m_Stream) << timestamp;
  }
}

/** Write to a buffer */
void
StdStreamLogOutput::Write(std::string const & content)
{
  const std::lock_guard<std::mutex> lockGuard(m_Mutex);
  if (this->m_Stream)
  {
    (*this->m_Stream) << content;
  }
}

/** Write to a buffer */
void
StdStreamLogOutput::Write(std::string const & content, double timestamp)
{
  const std::lock_guard<std::mutex> lockGuard(m_Mutex);
  if (this->m_Stream)
  {
    (*this->m_Stream) << timestamp << "  :  " << content;
  }
}

void
StdStreamLogOutput::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Stream: " << m_Stream << std::endl;
}
} // namespace itk
