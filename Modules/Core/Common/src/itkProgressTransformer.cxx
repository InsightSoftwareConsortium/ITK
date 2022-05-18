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

#include "itkProgressTransformer.h"
#include <algorithm>

namespace itk
{
namespace // Anonymous, limits exposure of symbols
{
class DummyProcess : public itk::ProcessObject
{
public:
  /** Standard class type aliases. */
  using Self = DummyProcess;
  using Superclass = ProcessObject;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;

  itkNewMacro(DummyProcess);
  itkTypeMacro(DummyProcess, ProcessObject);
};
} // namespace

ProgressTransformer::ProgressTransformer(float start, float end, ProcessObject * targetFilter)
  : m_TargetFilter(targetFilter)
  , m_ProgressTag(0)
{
  m_Start = std::max(start, 0.0f);
  m_Start = std::min(m_Start, 1.0f);
  m_End = std::max(end, 0.0f);
  m_End = std::min(m_End, 1.0f);
  m_Dummy = DummyProcess::New();

  m_ProgressCommand = CommandType::New();
  m_ProgressCommand->SetCallbackFunction(this, &ProgressTransformer::UpdateProgress);
  m_ProgressTag = m_Dummy->AddObserver(ProgressEvent(), m_ProgressCommand);
}

void
ProgressTransformer::UpdateProgress()
{
  float progress = m_Dummy->GetProgress();
  progress = std::max(progress, 0.0f);
  progress = std::min(progress, 1.0f);

  progress = m_Start + progress * (m_End - m_Start); // transform progress
  m_TargetFilter->UpdateProgress(progress);
}

ProgressTransformer::~ProgressTransformer()
{
  if (m_ProgressCommand)
  {
    m_Dummy->RemoveObserver(m_ProgressTag);
  }
}
} // namespace itk
