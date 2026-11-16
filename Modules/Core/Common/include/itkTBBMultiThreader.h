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

#ifndef itkTBBMultiThreader_h
#define itkTBBMultiThreader_h

#include "itkMultiThreaderBase.h"

namespace itk
{
/** \class TBBMultiThreader
 * \brief A class for performing multithreaded execution with a thread
 * pool back end, uses the Intel Threading Building Blocks (TBB) library.
 *
 * This multi-threader implementation generates dynamically-sized
 * thread-regions, and is expected to provide best performance.
 *
 * SetMaximumNumberOfThreads() clamps to [1, GetGlobalMaximumNumberOfThreads()]
 * without changing the work-unit count.
 *
 * \ingroup OSSystemObjects
 *
 * \ingroup ITKCommon
 */

class ITKCommon_EXPORT TBBMultiThreader : public MultiThreaderBase
{
public:
  ITK_DISALLOW_COPY_AND_MOVE(TBBMultiThreader);

  /** Standard class type aliases. */
  using Self = TBBMultiThreader;
  using Superclass = MultiThreaderBase;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(TBBMultiThreader);

  /** Set the number of work units, clamped to at least one, without changing
   * the thread count. This sets the callback count for SingleMethodExecute()
   * and the target partition granularity for ParallelizeArray() and
   * ParallelizeImageRegion(). */
  void
  SetNumberOfWorkUnits(ThreadIdType numberOfWorkUnits) override;

  /** Set work units to GetMaximumNumberOfThreads() times the ratio, without
   * changing the thread count. The ratio is clamped to
   * [1, NumericTraits<ThreadIdType>::max() / GetMaximumNumberOfThreads()]
   * so the product cannot overflow.
   * \sa SetNumberOfWorkUnits */
  void
  SetNumberOfWorkUnitsFromThreadRatio(ThreadIdType ratio) override;

  /** Execute the SingleMethod (as defined by SetSingleMethod) once per work unit. */
  void
  SingleMethodExecute() override;

  /** Set the SingleMethod to f() and the UserData field of the
   * WorkUnitInfo that is passed to it will be data.
   * This method must be of type itkThreadFunctionType and
   * must take a single argument of type void. */
  void
  SetSingleMethod(ThreadFunctionType, void * data) override;

  /** Parallelize an operation over an array. If filter argument is not nullptr,
   * this function will update its progress as each index is completed. */
  void
  ParallelizeArray(SizeValueType             firstIndex,
                   SizeValueType             lastIndexPlus1,
                   ArrayThreadingFunctorType aFunc,
                   ProcessObject *           filter) override;

  /** Break up region into smaller chunks, and call the function with chunks as parameters. */
  void
  ParallelizeImageRegion(unsigned int         dimension,
                         const IndexValueType index[],
                         const SizeValueType  size[],
                         ThreadingFunctorType funcP,
                         ProcessObject *      filter) override;

protected:
  TBBMultiThreader();
  ~TBBMultiThreader() override;
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  /** ProcessObject is a friend so that it can call PrintSelf() on its Multithreader. */
  friend class ProcessObject;
};

} // end namespace itk
#endif
