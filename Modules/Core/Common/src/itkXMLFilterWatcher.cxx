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
#include "itkXMLFilterWatcher.h"

namespace itk
{
void
XMLFilterWatcher::ShowProgress()
{
  if (this->GetProcess())
  {
    int steps = this->GetSteps();
    ++steps;
    this->SetSteps(steps);
    if (!this->GetQuiet())
    {
      std::cout << "<filter-progress>" << this->GetProcess()->GetProgress() << "</filter-progress>" << '\n'
                << std::flush;
    }
  }
}

void
XMLFilterWatcher::StartFilter()
{
  this->SetSteps(0);
  this->SetIterations(0);
  this->GetTimeProbe().Start();
  if (!this->GetQuiet())
  {
    std::cout << "<filter-start>" << '\n'
              << "<filter-name>" << (this->GetProcess() ? this->GetProcess()->GetNameOfClass() : "None")
              << "</filter-name>" << '\n';
    std::cout << "<filter-comment>"
              << " \"" << this->GetComment() << "\" "
              << "</filter-comment>" << '\n';
    std::cout << "</filter-start>" << '\n' << std::flush;
  }
}

void
XMLFilterWatcher::EndFilter()
{}

} // namespace itk
