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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkMultiThreaderBase_h
#define itkMultiThreaderBase_h

#include "itkObject.h"
#include "itkThreadSupport.h"
#include "itkObjectFactory.h"
#include "itkIntTypes.h"
#include "itkImageRegion.h"
#include "itkImageIORegion.h"
#include <functional>
#include <thread>


namespace itk
{
/** \class MultiThreaderBase
 * \brief A class for performing multithreaded execution
 *
 * Multithreader is a class that provides support for multithreaded
 * execution using Windows or POSIX threads.
 * This class can be used to execute a single
 * method on multiple threads, or to specify a method per thread.
 *
 * \ingroup OSSystemObjects
 *
 * \ingroup ITKCommon
 */

struct MultiThreaderBaseGlobals;

class ProcessObject;

class ITKCommon_EXPORT MultiThreaderBase : public Object
{
public:
  ITK_DISALLOW_COPY_AND_ASSIGN(MultiThreaderBase);

  /** Standard class type aliases. */
  using Self = MultiThreaderBase;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Method for creation through the object factory. */
  static Pointer New(void);

  /** Run-time type information (and related methods). */
  itkTypeMacro(MultiThreaderBase, Object);

  /** Get/Set the number of threads to create. It will be clamped to the range
   * [ 1, m_GlobalMaximumNumberOfThreads ], so the caller of this method should
   * check that the requested number of threads was accepted. */
  virtual void SetNumberOfThreads(ThreadIdType numberOfThreads);
  itkGetConstMacro(NumberOfThreads, ThreadIdType);

  /** Set/Get the maximum number of threads to use when multithreading.  It
   * will be clamped to the range [ 1, ITK_MAX_THREADS ] because several arrays
   * are already statically allocated using the ITK_MAX_THREADS number.
   * Therefore the caller of this method should check that the requested number
   * of threads was accepted. */
  static void SetGlobalMaximumNumberOfThreads(ThreadIdType val);
  static ThreadIdType  GetGlobalMaximumNumberOfThreads();

  /** Set/Get whether to use the to use the thread pool
   * implementation or the spawing implementation of
   * starting threads.
   */
  static void SetGlobalDefaultUseThreadPool( const bool GlobalDefaultUseThreadPool );
  static bool GetGlobalDefaultUseThreadPool( );

  /** Set/Get the value which is used to initialize the NumberOfThreads in the
   * constructor.  It will be clamped to the range [1, m_GlobalMaximumNumberOfThreads ].
   * Therefore the caller of this method should check that the requested number
   * of threads was accepted. */
  static void SetGlobalDefaultNumberOfThreads(ThreadIdType val);

  static ThreadIdType  GetGlobalDefaultNumberOfThreads();

  /** This is the structure that is passed to the thread that is
   * created from the SingleMethodExecute. It is passed in as a void *,
   * and it is up to the method to cast correctly and extract the information.
   * The ThreadID is a number between 0 and NumberOfThreads-1 that
   * indicates the id of this thread. The UserData is the
   * (void *)arg passed into the SetSingleMethod. */
  struct ThreadInfoStruct
    {
    ThreadIdType ThreadID;
    ThreadIdType NumberOfThreads;
    void* UserData;
    ThreadFunctionType ThreadFunction;
    enum { SUCCESS, ITK_EXCEPTION, ITK_PROCESS_ABORTED_EXCEPTION, STD_EXCEPTION, UNKNOWN } ThreadExitCode;
    };

  /** Execute the SingleMethod (as define by SetSingleMethod) using
   * m_NumberOfThreads threads. As a side effect the m_NumberOfThreads will be
   * checked against the current m_GlobalMaximumNumberOfThreads and clamped if
   * necessary. */
  virtual void SingleMethodExecute() = 0;

  /** Set the SingleMethod to f() and the UserData field of the
   * ThreadInfoStruct that is passed to it will be data.
   * This method (and all the methods passed to SetMultipleMethod)
   * must be of type itkThreadFunctionType and must take a single argument of
   * type void *. */
  virtual void SetSingleMethod(ThreadFunctionType, void *data) = 0;

  template <unsigned int VDimension>
  using TemplatedThreadingFunctorType = std::function<void(const ImageRegion<VDimension> &)>;
  using ThreadingFunctorType = std::function<void(
      const IndexValueType index[],
      const SizeValueType size[])>;

  /** Break up region into smaller chunks, and call the function with chunks as parameters.
   * If filter argument is not nullptr, this function will update its progress
   * as each work unit is completed. */
  template<unsigned int VDimension>
  void ParallelizeImageRegion(const ImageRegion<VDimension> & requestedRegion, TemplatedThreadingFunctorType<VDimension> funcP, ProcessObject* filter)
  {
    this->ParallelizeImageRegion(
        VDimension,
        requestedRegion.GetIndex().m_InternalArray,
        requestedRegion.GetSize().m_InternalArray,
        [funcP](const IndexValueType index[], const SizeValueType size[])
    {
      ImageRegion<VDimension> region;
      for (unsigned d = 0; d < VDimension; d++)
        {
        region.SetIndex(d, index[d]);
        region.SetSize(d, size[d]);
        }
      funcP(region);
        },
        filter);
  }

  /** Break up region into smaller chunks, and call the function with chunks as parameters.
   *  This overload does the actual work and should be implemented by derived classes. */
  virtual void ParallelizeImageRegion(
      unsigned int dimension,
      const IndexValueType index[],
      const SizeValueType size[],
      ThreadingFunctorType funcP,
      ProcessObject* filter);

  /** Set/Get the pointer to MultiThreaderBaseGlobals.
   * Note that these functions are not part of the public API and should not be
   * used outside of ITK. They are an implementation detail and will be
   * removed in the future. Also note that SetMultiThreaderBaseGlobals is not
   * concurrent thread safe. */
  static MultiThreaderBaseGlobals *GetMultiThreaderBaseGlobals();
  static void SetMultiThreaderBaseGlobals(MultiThreaderBaseGlobals * multiThreaderBaseGlobals);

protected:
  MultiThreaderBase();
  ~MultiThreaderBase() override;
  void PrintSelf(std::ostream & os, Indent indent) const override;

  struct RegionAndCallback
  {
    ThreadingFunctorType functor;
    unsigned int dimension;
    const IndexValueType* index;
    const SizeValueType* size;
    ProcessObject* filter;
    std::thread::id callingThread;
    SizeValueType pixelCount;
    std::atomic<SizeValueType> pixelProgress;
  };

  static ITK_THREAD_RETURN_TYPE ParallelizeImageRegionHelper(void *arg);

  /** The number of threads to use.
   *  The m_NumberOfThreads must always be less than or equal to
   *  the m_GlobalMaximumNumberOfThreads before it is used during the execution
   *  of a threaded method. Its value is clamped in the SingleMethodExecute()
   *  and MultipleMethodExecute(). Its value is initialized to
   *  m_GlobalDefaultNumberOfThreads at construction time. Its value is clamped
   *  to the current m_GlobalMaximumNumberOfThreads in the
   *  SingleMethodExecute() and MultipleMethodExecute() methods.
   */
  ThreadIdType m_NumberOfThreads;

  /** Static function used as a "proxy callback" by multi-threaders.  The
  * threading library will call this routine for each thread, which
  * will delegate the control to the prescribed SingleMethod. This
  * routine acts as an intermediary between the multi-threaders and the
  * user supplied callback (SingleMethod) in order to catch any
  * exceptions thrown by the threads. */
  static ITK_THREAD_RETURN_TYPE SingleMethodProxy(void *arg);

  /** The method to invoke. */
  ThreadFunctionType m_SingleMethod;

  /** The data to be passed as argument. */
  void *m_SingleData;

private:
  static MultiThreaderBaseGlobals * m_MultiThreaderBaseGlobals;
  /** Friends of Multithreader.
   * ProcessObject is a friend so that it can call PrintSelf() on its
   * Multithreader. */
  friend class ProcessObject;
};
}  // end namespace itk
#endif
