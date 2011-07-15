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
#ifndef __itkImageSource_hxx
#define __itkImageSource_hxx
#include "itkImageSource.h"

#include "vnl/vnl_math.h"

namespace itk
{
/**
 *
 */
template< class TOutputImage >
ImageSource< TOutputImage >
::ImageSource()
{
  // Create the output. We use static_cast<> here because we know the default
  // output must be of type TOutputImage
  typename TOutputImage::Pointer output =
    static_cast< TOutputImage * >( this->MakeOutput(0).GetPointer() );
  this->ProcessObject::SetNumberOfRequiredOutputs(1);
  this->ProcessObject::SetNthOutput( 0, output.GetPointer() );

  // Set the default behavior of an image source to NOT release its
  // output bulk data prior to GenerateData() in case that bulk data
  // can be reused (an thus avoid a costly deallocate/allocate cycle).
  this->ReleaseDataBeforeUpdateFlagOff();
}

/**
 *
 */
template< class TOutputImage >
typename ImageSource< TOutputImage >::DataObjectPointer
ImageSource< TOutputImage >
::MakeOutput(unsigned int)
{
  return static_cast< DataObject * >( TOutputImage::New().GetPointer() );
}

/**
 *
 */
template< class TOutputImage >
typename ImageSource< TOutputImage >::OutputImageType *
ImageSource< TOutputImage >
::GetOutput()
{
  if ( this->GetNumberOfOutputs() < 1 )
    {
    return 0;
    }

  // we assume that the first output is of the templated type
  return static_cast< TOutputImage * >
         ( this->ProcessObject::GetOutput(0) );
}

/**
 *
 */
template< class TOutputImage >
typename ImageSource< TOutputImage >::OutputImageType *
ImageSource< TOutputImage >
::GetOutput(unsigned int idx)
{
  TOutputImage *out = dynamic_cast< TOutputImage * >
                      ( this->ProcessObject::GetOutput(idx) );

  if ( out == NULL )
    {
    itkWarningMacro (<< "dynamic_cast to output type failed");
    }
  return out;
}

/**
 *
 */
template< class TOutputImage >
void
ImageSource< TOutputImage >
::GraftOutput(DataObject *graft)
{
  this->GraftNthOutput(0, graft);
}

/**
 *
 */
template< class TOutputImage >
void
ImageSource< TOutputImage >
::GraftNthOutput(unsigned int idx, DataObject *graft)
{
  if ( idx >= this->GetNumberOfOutputs() )
    {
    itkExceptionMacro(<< "Requested to graft output " << idx
                      << " but this filter only has " << this->GetNumberOfOutputs() << " Outputs.");
    }

  if ( !graft )
    {
    itkExceptionMacro(<< "Requested to graft output that is a NULL pointer");
    }

  // we use the process object method since all out output may not be
  // of the same type
  DataObject *output = this->ProcessObject::GetOutput(idx);

  // Call GraftImage to copy meta-information, regions, and the pixel container
  output->Graft(graft);
}

//----------------------------------------------------------------------------
template< class TOutputImage >
unsigned int
ImageSource< TOutputImage >
::SplitRequestedRegion(unsigned int i, unsigned int num, OutputImageRegionType & splitRegion)
{
  // Get the output pointer
  OutputImageType *outputPtr = this->GetOutput();

  const typename TOutputImage::SizeType & requestedRegionSize =
    outputPtr->GetRequestedRegion().GetSize();

  int splitAxis;
  typename TOutputImage::IndexType splitIndex;
  typename TOutputImage::SizeType splitSize;

  // Initialize the splitRegion to the output requested region
  splitRegion = outputPtr->GetRequestedRegion();
  splitIndex = splitRegion.GetIndex();
  splitSize = splitRegion.GetSize();

  // split on the outermost dimension available
  splitAxis = outputPtr->GetImageDimension() - 1;
  while ( requestedRegionSize[splitAxis] == 1 )
    {
    --splitAxis;
    if ( splitAxis < 0 )
      { // cannot split
      itkDebugMacro("  Cannot Split");
      return 1;
      }
    }

  // determine the actual number of pieces that will be generated
  typename TOutputImage::SizeType::SizeValueType range = requestedRegionSize[splitAxis];
  unsigned int valuesPerThread = Math::Ceil< unsigned int >(range / (double)num);
  unsigned int maxThreadIdUsed = Math::Ceil< unsigned int >(range / (double)valuesPerThread) - 1;

  // Split the region
  if ( i < maxThreadIdUsed )
    {
    splitIndex[splitAxis] += i * valuesPerThread;
    splitSize[splitAxis] = valuesPerThread;
    }
  if ( i == maxThreadIdUsed )
    {
    splitIndex[splitAxis] += i * valuesPerThread;
    // last thread needs to process the "rest" dimension being split
    splitSize[splitAxis] = splitSize[splitAxis] - i * valuesPerThread;
    }

  // set the split region ivars
  splitRegion.SetIndex(splitIndex);
  splitRegion.SetSize(splitSize);

  itkDebugMacro("  Split Piece: " << splitRegion);

  return maxThreadIdUsed + 1;
}

//----------------------------------------------------------------------------
template< class TOutputImage >
void
ImageSource< TOutputImage >
::AllocateOutputs()
{
  typedef ImageBase< OutputImageDimension > ImageBaseType;
  typename ImageBaseType::Pointer outputPtr;

  // Allocate the output memory
  for ( unsigned int i = 0; i < this->GetNumberOfOutputs(); i++ )
    {
    // Check whether the output is an image of the appropriate
    // dimension (use ProcessObject's version of the GetInput()
    // method since it returns the input as a pointer to a
    // DataObject as opposed to the subclass version which
    // static_casts the input to an TInputImage).
    outputPtr = dynamic_cast< ImageBaseType * >( this->ProcessObject::GetOutput(i) );

    if ( outputPtr )
      {
      outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
      outputPtr->Allocate();
      }
    }
}

//----------------------------------------------------------------------------
template< class TOutputImage >
void
ImageSource< TOutputImage >
::GenerateData()
{
  // Call a method that can be overriden by a subclass to allocate
  // memory for the filter's outputs
  this->AllocateOutputs();

  // Call a method that can be overridden by a subclass to perform
  // some calculations prior to splitting the main computations into
  // separate threads
  this->BeforeThreadedGenerateData();

  // Set up the multithreaded processing
  ThreadStruct str;
  str.Filter = this;

  this->GetMultiThreader()->SetNumberOfThreads( this->GetNumberOfThreads() );
  this->GetMultiThreader()->SetSingleMethod(this->ThreaderCallback, &str);

  // multithread the execution
  this->GetMultiThreader()->SingleMethodExecute();

  // Call a method that can be overridden by a subclass to perform
  // some calculations after all the threads have completed
  this->AfterThreadedGenerateData();
}

//----------------------------------------------------------------------------
// The execute method created by the subclass.
template< class TOutputImage >
void
ImageSource< TOutputImage >
::ThreadedGenerateData(const OutputImageRegionType &,
                       ThreadIdType)
{
// The following code is equivalent to:
// itkExceptionMacro("subclass should override this method!!!");
// The ExceptionMacro is not used because gcc warns that a
// 'noreturn' function does return
  std::ostringstream message;

  message << "itk::ERROR: " << this->GetNameOfClass()
          << "(" << this << "): " << "Subclass should override this method!!!" << std::endl
          << "The signature of ThreadedGenerateData() has been changed in ITK v4 to use the new ThreadIdType." << std::endl
          << this->GetNameOfClass() << "::ThreadedGenerateData() might need to be updated to used it.";
  ExceptionObject e_(__FILE__, __LINE__, message.str().c_str(), ITK_LOCATION);
  throw e_;
}

// Callback routine used by the threading library. This routine just calls
// the ThreadedGenerateData method after setting the correct region for this
// thread.
template< class TOutputImage >
ITK_THREAD_RETURN_TYPE
ImageSource< TOutputImage >
::ThreaderCallback(void *arg)
{
  ThreadStruct *str;
  ThreadIdType  total, threadId, threadCount;

  threadId = ( (MultiThreader::ThreadInfoStruct *)( arg ) )->ThreadID;
  threadCount = ( (MultiThreader::ThreadInfoStruct *)( arg ) )->NumberOfThreads;

  str = (ThreadStruct *)( ( (MultiThreader::ThreadInfoStruct *)( arg ) )->UserData );

  // execute the actual method with appropriate output region
  // first find out how many pieces extent can be split into.
  typename TOutputImage::RegionType splitRegion;
  total = str->Filter->SplitRequestedRegion(threadId, threadCount,
                                            splitRegion);

  if ( threadId < total )
    {
    str->Filter->ThreadedGenerateData(splitRegion, threadId);
    }
  // else
  //   {
  //   otherwise don't use this thread. Sometimes the threads dont
  //   break up very well and it is just as efficient to leave a
  //   few threads idle.
  //   }

  return ITK_THREAD_RETURN_VALUE;
}
} // end namespace itk

#endif
