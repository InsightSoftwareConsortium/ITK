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
#ifndef itkSingleMultiThreader_h
#define itkSingleMultiThreader_h

#include "itkMultiThreaderBase.h"

namespace itk
{
/** \class SingleMultiThreader
 * \brief A class for performing multithreaded execution using only the
 * calling thread.
 *
 * All work is executed synchronously on the calling thread; no additional
 * threads are created. This is efficient when the application layer is
 * already managing parallelism (e.g., calling ITK filters from multiple
 * threads) and internal ITK threading overhead should be avoided.
 *
 * \ingroup OSSystemObjects
 *
 * \ingroup ITKCommon
 */

class ITKCommon_EXPORT SingleMultiThreader : public MultiThreaderBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(SingleMultiThreader);

  /** Standard class type aliases. */
  using Self = SingleMultiThreader;
  using Superclass = MultiThreaderBase;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(SingleMultiThreader);

  /** Execute the SingleMethod (as defined by SetSingleMethod) on the
   * calling thread. NumberOfWorkUnits is always 1. */
  void
  SingleMethodExecute() override;

  /** Set the SingleMethod to f() and the UserData field of the
   * WorkUnitInfo that is passed to it will be data. */
  void
  SetSingleMethod(ThreadFunctionType, void * data) override;

  /** Parallelize an operation over an array by executing it serially on
   * the calling thread. */
  void
  ParallelizeArray(SizeValueType             firstIndex,
                   SizeValueType             lastIndexPlus1,
                   ArrayThreadingFunctorType aFunc,
                   ProcessObject *           filter) override;

  /** Break up region into smaller chunks. SingleThreader processes
   * the entire region in a single call on the calling thread. */
  void
  ParallelizeImageRegion(unsigned int         dimension,
                         const IndexValueType index[],
                         const SizeValueType  size[],
                         ThreadingFunctorType funcP,
                         ProcessObject *      filter) override;

  /** SingleThreader always uses exactly one thread. */
  void
  SetMaximumNumberOfThreads(ThreadIdType numberOfThreads) override;

  /** SingleThreader always uses exactly one work unit. */
  void
  SetNumberOfWorkUnits(ThreadIdType numberOfWorkUnits) override;

protected:
  SingleMultiThreader();
  ~SingleMultiThreader() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  /** Friends of Multithreader.
   * ProcessObject is a friend so that it can call PrintSelf() on its
   * Multithreader. */
  friend class ProcessObject;
};
} // end namespace itk
#endif // itkSingleMultiThreader_h
