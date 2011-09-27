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

#include "itkMultipleLogOutput.h"

namespace itk
{
MultipleLogOutput::MultipleLogOutput()
{
  this->m_Output.clear();
}

MultipleLogOutput::~MultipleLogOutput()
{
//  this->Flush();
}

/** Adds an output stream to the MultipleLogOutput for writing. */
void
MultipleLogOutput::AddLogOutput(OutputType *output)
{
  this->m_Output.insert(output);   // insert the address
}

/** The Flush method flushes all the streams. */
void
MultipleLogOutput::Flush(void)
{
  ContainerType::iterator itr = m_Output.begin();
  ContainerType::iterator end = m_Output.end();

  while ( itr != end )
    {
    ( *itr )->Flush();
    ++itr;
    }
}

/** Write to multiple outputs */
void MultipleLogOutput::Write(double timestamp)
{
  ContainerType::iterator itr = m_Output.begin();
  ContainerType::iterator end = m_Output.end();

  while ( itr != end )
    {
    ( *itr )->Write(timestamp);
    ++itr;
    }
}

/** Write to multiple outputs */
void MultipleLogOutput::Write(const std::string & content)
{
  ContainerType::iterator itr = m_Output.begin();
  ContainerType::iterator end = m_Output.end();

  while ( itr != end )
    {
    ( *itr )->Write(content);
    ++itr;
    }
}

/** Write to a buffer */
void MultipleLogOutput::Write(const std::string & content, double timestamp)
{
  ContainerType::iterator itr = m_Output.begin();
  ContainerType::iterator end = m_Output.end();

  while ( itr != end )
    {
    ( *itr )->Write(content, timestamp);
    ++itr;
    }
}
}
