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
#ifndef itkImageSource_hxx
#define itkImageSource_hxx
#include "itkImageSource.h"

#include "itkOutputDataObjectIterator.h"
#include "itkImageRegionSplitterBase.h"
#include "itkMultiThreaderBase.h"

#include "itkMath.h"

namespace itk
{
template <typename TOutputImage>
ImageSource<TOutputImage>::ImageSource()
{
  // Create the output. We use static_cast<> here because we know the default
  // output must be of type TOutputImage
  typename TOutputImage::Pointer output = static_cast<TOutputImage *>(this->MakeOutput(0).GetPointer());
  this->ProcessObject::SetNumberOfRequiredOutputs(1);
  this->ProcessObject::SetNthOutput(0, output.GetPointer());

#if defined(ITKV4_COMPATIBILITY)
  m_DynamicMultiThreading = false;
#else
  m_DynamicMultiThreading = true;
#endif

  // Set the default behavior of an image source to NOT release its
  // output bulk data prior to GenerateData() in case that bulk data
  // can be reused (an thus avoid a costly deallocate/allocate cycle).
  this->ReleaseDataBeforeUpdateFlagOff();
}

template <typename TOutputImage>
ProcessObject::DataObjectPointer ImageSource<TOutputImage>::MakeOutput(ProcessObject::DataObjectPointerArraySizeType)
{
  return TOutputImage::New().GetPointer();
}

template <typename TOutputImage>
ProcessObject::DataObjectPointer
ImageSource<TOutputImage>::MakeOutput(const ProcessObject::DataObjectIdentifierType &)
{
  return TOutputImage::New().GetPointer();
}

template <typename TOutputImage>
typename ImageSource<TOutputImage>::OutputImageType *
ImageSource<TOutputImage>::GetOutput()
{
  // we assume that the first output is of the templated type
  return itkDynamicCastInDebugMode<TOutputImage *>(this->GetPrimaryOutput());
}

template <typename TOutputImage>
const typename ImageSource<TOutputImage>::OutputImageType *
ImageSource<TOutputImage>::GetOutput() const
{
  // we assume that the first output is of the templated type
  return itkDynamicCastInDebugMode<const TOutputImage *>(this->GetPrimaryOutput());
}

template <typename TOutputImage>
typename ImageSource<TOutputImage>::OutputImageType *
ImageSource<TOutputImage>::GetOutput(unsigned int idx)
{
  auto * out = dynamic_cast<TOutputImage *>(this->ProcessObject::GetOutput(idx));

  if (out == nullptr && this->ProcessObject::GetOutput(idx) != nullptr)
  {
    itkWarningMacro(<< "Unable to convert output number " << idx << " to type " << typeid(OutputImageType).name());
  }
  return out;
}

template <typename TOutputImage>
void
ImageSource<TOutputImage>::GraftOutput(DataObject * graft)
{
  this->GraftNthOutput(0, graft);
}

template <typename TOutputImage>
void
ImageSource<TOutputImage>::GraftOutput(const DataObjectIdentifierType & key, DataObject * graft)
{
  if (!graft)
  {
    itkExceptionMacro(<< "Requested to graft output that is a nullptr pointer");
  }

  // we use the process object method since all out output may not be
  // of the same type
  DataObject * output = this->ProcessObject::GetOutput(key);

  // Call GraftImage to copy meta-information, regions, and the pixel container
  output->Graft(graft);
}

template <typename TOutputImage>
void
ImageSource<TOutputImage>::GraftNthOutput(unsigned int idx, DataObject * graft)
{
  if (idx >= this->GetNumberOfIndexedOutputs())
  {
    itkExceptionMacro(<< "Requested to graft output " << idx << " but this filter only has "
                      << this->GetNumberOfIndexedOutputs() << " indexed Outputs.");
  }
  this->GraftOutput(this->MakeNameFromOutputIndex(idx), graft);
}


//----------------------------------------------------------------------------
template <typename TOutputImage>
const ImageRegionSplitterBase *
ImageSource<TOutputImage>::GetImageRegionSplitter() const
{
  return this->GetGlobalDefaultSplitter();
}

//----------------------------------------------------------------------------
template <typename TOutputImage>
unsigned int
ImageSource<TOutputImage>::SplitRequestedRegion(unsigned int            i,
                                                unsigned int            pieces,
                                                OutputImageRegionType & splitRegion)
{
  const ImageRegionSplitterBase * splitter = this->GetImageRegionSplitter();

  // Get the output pointer
  OutputImageType * outputPtr = this->GetOutput();

  splitRegion = outputPtr->GetRequestedRegion();
  return splitter->GetSplit(i, pieces, splitRegion);
}

//----------------------------------------------------------------------------
template <typename TOutputImage>
void
ImageSource<TOutputImage>::AllocateOutputs()
{
  using ImageBaseType = ImageBase<OutputImageDimension>;
  typename ImageBaseType::Pointer outputPtr;

  // Allocate the output memory
  for (OutputDataObjectIterator it(this); !it.IsAtEnd(); ++it)
  {
    // Check whether the output is an image of the appropriate
    // dimension (use ProcessObject's version of the GetInput()
    // method since it returns the input as a pointer to a
    // DataObject as opposed to the subclass version which
    // static_casts the input to an TInputImage).
    outputPtr = dynamic_cast<ImageBaseType *>(it.GetOutput());

    if (outputPtr)
    {
      outputPtr->SetBufferedRegion(outputPtr->GetRequestedRegion());
      outputPtr->Allocate();
    }
  }
}

//----------------------------------------------------------------------------
template <typename TOutputImage>
void
ImageSource<TOutputImage>::ClassicMultiThread(ThreadFunctionType callbackFunction)
{
  ThreadStruct str;
  str.Filter = this;

  const OutputImageType *         outputPtr = this->GetOutput();
  const ImageRegionSplitterBase * splitter = this->GetImageRegionSplitter();
  const unsigned int              validThreads =
    splitter->GetNumberOfSplits(outputPtr->GetRequestedRegion(), this->GetNumberOfWorkUnits());

  this->GetMultiThreader()->SetNumberOfWorkUnits(validThreads);
  this->GetMultiThreader()->SetUpdateProgress(false);
  this->GetMultiThreader()->SetSingleMethod(callbackFunction, &str);

  this->GetMultiThreader()->SingleMethodExecute();
}

template <typename TOutputImage>
void
ImageSource<TOutputImage>::GenerateData()
{
  // Call a method that can be overriden by a subclass to allocate
  // memory for the filter's outputs
  this->AllocateOutputs();

  // Call a method that can be overridden by a subclass to perform
  // some calculations prior to splitting the main computations into
  // separate threads
  this->BeforeThreadedGenerateData();

  if (!m_DynamicMultiThreading)
  {
    this->ClassicMultiThread(this->ThreaderCallback);
  }
  else
  {
    this->GetMultiThreader()->SetNumberOfWorkUnits(this->GetNumberOfWorkUnits());
    this->GetMultiThreader()->SetUpdateProgress(this->GetThreaderUpdateProgress());
    this->GetMultiThreader()->template ParallelizeImageRegion<OutputImageDimension>(
      this->GetOutput()->GetRequestedRegion(),
      [this](const OutputImageRegionType & outputRegionForThread) {
        this->DynamicThreadedGenerateData(outputRegionForThread);
      },
      this);
  }

  // Call a method that can be overridden by a subclass to perform
  // some calculations after all the threads have completed
  this->AfterThreadedGenerateData();
}

//----------------------------------------------------------------------------
// The execute method created by the subclass.
template <typename TOutputImage>
void
ImageSource<TOutputImage>::ThreadedGenerateData(const OutputImageRegionType &
#if !defined(ITK_LEGACY_REMOVE)
                                                  region
#endif
                                                ,
                                                ThreadIdType)
{
#if !defined(ITK_LEGACY_REMOVE)
  this->DynamicThreadedGenerateData(region);
#else
  itkExceptionMacro("With DynamicMultiThreadingOff subclass should override this method. \
The signature of ThreadedGenerateData() has been changed in ITK v4 to use the new ThreadIdType.");
#endif
}

// The execute method created by the subclass.
template <typename TOutputImage>
void
ImageSource<TOutputImage>::DynamicThreadedGenerateData(const OutputImageRegionType &)
{
  itkExceptionMacro("Subclass should override this method!!! \
If old behavior is desired invoke this->DynamicMultiThreadingOff(); \
before Update() is called. The best place is in class constructor.");
}

// Callback routine used by the classic threading library. This routine just calls
// the ThreadedGenerateData method after invoking (possibly) overloaded split region routine.
template <typename TOutputImage>
ITK_THREAD_RETURN_FUNCTION_CALL_CONVENTION
ImageSource<TOutputImage>::ThreaderCallback(void * arg)
{
  using ThreadInfo = MultiThreaderBase::WorkUnitInfo;
  auto *       threadInfo = static_cast<ThreadInfo *>(arg);
  ThreadIdType threadId = threadInfo->WorkUnitID;
  ThreadIdType threadCount = threadInfo->NumberOfWorkUnits;
  auto *       str = (ThreadStruct *)(threadInfo->UserData);

  // execute the actual method with appropriate output region
  // first find out how many pieces extent can be split into.
  typename TOutputImage::RegionType splitRegion;
  ThreadIdType                      total = str->Filter->SplitRequestedRegion(threadId, threadCount, splitRegion);

  if (threadId < total)
  {
    str->Filter->ThreadedGenerateData(splitRegion, threadId);
#if defined(ITKV4_COMPATIBILITY)
    if (str->Filter->GetAbortGenerateData())
    {
      std::string    msg;
      ProcessAborted e(__FILE__, __LINE__);
      msg += "Object " + std::string(str->Filter->GetNameOfClass()) + ": AbortGenerateData was set!";
      e.SetDescription(msg);
      throw e;
    }
    else if (!str->Filter->GetDynamicMultiThreading() // progress reporting is not done in MultiThreaders
             && str->Filter->GetProgress() == 0.0f) // and progress was not set after at least the first chunk finished
    {
      str->Filter->UpdateProgress(float(threadId + 1) / total); // this will be the only progress update
    }
#endif
  }
  // else don't use this thread. Threads were not split conveniently.
  return ITK_THREAD_RETURN_DEFAULT_VALUE;
}

template <typename TOutputImage>
void
ImageSource<TOutputImage>::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "DynamicMultiThreading: " << (m_DynamicMultiThreading ? "On" : "Off") << std::endl;
}

} // end namespace itk

#endif
