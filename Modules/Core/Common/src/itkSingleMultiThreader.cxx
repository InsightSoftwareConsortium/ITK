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
#include "itkSingleMultiThreader.h"
#include "itkNumericTraits.h"
#include "itkProcessObject.h"
#include "itkTotalProgressReporter.h"

namespace itk
{

SingleMultiThreader::SingleMultiThreader()
{
  m_NumberOfWorkUnits = 1;
  m_MaximumNumberOfThreads = 1;
}

SingleMultiThreader::~SingleMultiThreader() = default;

void
SingleMultiThreader::SetSingleMethod(ThreadFunctionType f, void * data)
{
  m_SingleMethod = std::move(f);
  m_SingleData = data;
}

void
SingleMultiThreader::SetMaximumNumberOfThreads(ThreadIdType)
{
  m_MaximumNumberOfThreads = 1;
}

void
SingleMultiThreader::SetNumberOfWorkUnits(ThreadIdType)
{
  m_NumberOfWorkUnits = 1;
}

void
SingleMultiThreader::SingleMethodExecute()
{
  if (!m_SingleMethod)
  {
    itkExceptionStringMacro("No single method set!");
  }

  WorkUnitInfo workUnitInfo;
  workUnitInfo.WorkUnitID = 0;
  workUnitInfo.NumberOfWorkUnits = 1;
  workUnitInfo.UserData = m_SingleData;
  workUnitInfo.ThreadFunction = m_SingleMethod;
  workUnitInfo.ThreadExitCode = WorkUnitInfo::ThreadExitCodeEnum::SUCCESS;

  // Invoke the method directly on the calling thread.
  // Since no thread boundary is crossed, exceptions propagate naturally
  // to the caller, preserving the original exception type and message.
  m_SingleMethod(&workUnitInfo);
}

void
SingleMultiThreader::ParallelizeArray(SizeValueType             firstIndex,
                                      SizeValueType             lastIndexPlus1,
                                      ArrayThreadingFunctorType aFunc,
                                      ProcessObject *           filter)
{
  if (!this->GetUpdateProgress())
  {
    filter = nullptr;
  }

  if (firstIndex + 1 < lastIndexPlus1)
  {
    const SizeValueType   count = lastIndexPlus1 - firstIndex;
    TotalProgressReporter reporter(filter, count);
    for (SizeValueType i = firstIndex; i < lastIndexPlus1; ++i)
    {
      reporter.CheckAbortGenerateData();
      aFunc(i);
      reporter.CompletedPixel();
    }
  }
  else if (firstIndex + 1 == lastIndexPlus1)
  {
    // Match MultiThreaderBase::ParallelizeArray: progress reaches 1.0 on RAII exit.
    const ProgressReporter progress(filter, 0, 1);
    aFunc(firstIndex);
  }
  // else: empty or reversed range — nothing to execute
}

void
SingleMultiThreader::ParallelizeImageRegion(unsigned int         dimension,
                                            const IndexValueType index[],
                                            const SizeValueType  size[],
                                            ThreadingFunctorType funcP,
                                            ProcessObject *      filter)
{
  (void)dimension; // The full region is passed directly; dimension is implicit in index/size.
  if (!this->GetUpdateProgress())
  {
    filter = nullptr;
  }

  const ProgressReporter progress(filter, 0, 1);

  funcP(index, size);
}

void
SingleMultiThreader::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

} // namespace itk
