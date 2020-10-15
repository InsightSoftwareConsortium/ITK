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
#ifndef itkProgressTransformer_h
#define itkProgressTransformer_h

#include "itkProcessObject.h"
#include "itkCommand.h"

namespace itk
{
/**
 * \class ProgressTransformer
 * \brief Transforms progress updates from [0%, 100%] to desired scale.
 *
 * This class is designed to be allocated on the stack. Code sample:
 *
 *   ProgressTransformer progress2( 0.55f, 0.6f, this );
 *   multiThreader->ParallelizeArray(0, m_SomeArray.size(),
 *     [this]( SizeValueType index ) { this->ComputeInParallel( index ); },
 *     progress2.GetProcessObject() );
 *
 *
 * \sa ProgressReporter, ProcessObject
 * \ingroup ITKCommon
 */
class ITKCommon_EXPORT ProgressTransformer
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(ProgressTransformer);

  ProgressTransformer(float start, float end, ProcessObject * targetFilter);

  ProcessObject *
  GetProcessObject()
  {
    return m_Dummy.GetPointer();
  }

  void
  UpdateProgress();

  ~ProgressTransformer();

private:
  float m_Start;
  float m_End;

  ProcessObject * m_TargetFilter;

  ProcessObject::Pointer m_Dummy;

  using CommandType = SimpleMemberCommand<ProgressTransformer>;
  CommandType::Pointer m_ProgressCommand;

  unsigned long m_ProgressTag;
};
} // end namespace itk
#endif // itkProgressTransformer_h
