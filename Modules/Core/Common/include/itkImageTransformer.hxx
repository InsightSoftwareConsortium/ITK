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
#ifndef itkImageTransformer_hxx
#define itkImageTransformer_hxx
#include "itkImageTransformer.h"

#include "itkMath.h"

namespace itk
{
/**
 *
 */
template< typename TInputImage >
ImageTransformer< TInputImage >
::ImageTransformer()
{
  // Set the default behavior of an image source to NOT release its
  // output bulk data prior to GenerateData() in case that bulk data
  // can be reused (an thus avoid a costly deallocate/allocate cycle).
  this->ReleaseDataBeforeUpdateFlagOff();
}

/**
 *
 */
template< typename TInputImage >
void
ImageTransformer< TInputImage >
::SetInput(const InputImageType *input)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput( 0,
                                    const_cast< InputImageType * >( input ) );
}

/**
 * Connect one of the operands for pixel-wise addition
 */
template< typename TInputImage >
void
ImageTransformer< TInputImage >
::SetInput(unsigned int index, const TInputImage *image)
{
  // Process object is not const-correct so the const_cast is required here
  this->ProcessObject::SetNthInput( index,
                                    const_cast< TInputImage * >( image ) );
}

/**
 *
 */
template< typename TInputImage >
const typename ImageTransformer< TInputImage >::InputImageType *
ImageTransformer< TInputImage >
::GetInput(void) const
{
  if ( this->GetNumberOfInputs() < 1 )
    {
    return 0;
    }

  return itkDynamicCastInDebugMode< const TInputImage * >
         ( this->ProcessObject::GetInput(0) );
}

/**
 *
 */
template< typename TInputImage >
typename ImageTransformer< TInputImage >::InputImageType *
ImageTransformer< TInputImage >
::GetInput(void)
{
  if ( this->GetNumberOfInputs() < 1 )
    {
    return ITK_NULLPTR;
    }

  return itkDynamicCastInDebugMode< TInputImage * >
         ( this->ProcessObject::GetInput(0) );
}

/**
 *
 */
template< typename TInputImage >
const typename ImageTransformer< TInputImage >::InputImageType *
ImageTransformer< TInputImage >
::GetInput(unsigned int idx) const
{
  return itkDynamicCastInDebugMode< const TInputImage * >
         ( this->ProcessObject::GetInput(idx) );
}

template< typename TInputImage >
void
ImageTransformer< TInputImage >
::PushBackInput(const InputImageType *input)
{
  // Forward to the protected method in the superclass
  this->ProcessObject::PushBackInput(input);
}

template< typename TInputImage >
void
ImageTransformer< TInputImage >
::PopBackInput()
{
  // Forward to the protected method in the superclass
  this->ProcessObject::PopBackInput();
}

template< typename TInputImage >
void
ImageTransformer< TInputImage >
::PushFrontInput(const InputImageType *input)
{
  // Forward to the protected method in the superclass
  this->ProcessObject::PushFrontInput(input);
}

template< typename TInputImage >
void
ImageTransformer< TInputImage >
::PopFrontInput()
{
  // Forward to the protected method in the superclass
  this->ProcessObject::PopFrontInput();
}

//-----------------------------------------------------------------------
//
template< typename TInputImage >
void
ImageTransformer< TInputImage >
::GenerateInputRequestedRegion()
{
  Superclass::GenerateInputRequestedRegion();

  for ( unsigned int idx = 0; idx < this->GetNumberOfInputs(); ++idx )
    {
    if ( this->GetInput(idx) )
      {
      // Check whether the input is an image of the appropriate
      // dimension (use ProcessObject's version of the GetInput()
      // method since it returns the input as a pointer to a
      // DataObject as opposed to the subclass version which
      // static_casts the input to an TInputImage).
      typedef ImageBase< InputImageDimension > ImageBaseType;
      typename ImageBaseType::ConstPointer constInput =
        dynamic_cast< ImageBaseType const * >( this->ProcessObject::GetInput(idx) );

      // If not an image, skip it, and let a subclass of
      // ImageToImageFilter handle this input.
      if ( constInput.IsNull() )
        {
        continue;
        }

      // Input is an image, cast away the constness so we can set
      // the requested region.
      InputImagePointer input =
        const_cast< TInputImage * >( this->GetInput(idx) );

      // transform is assumed to need the whole image
      input->SetRequestedRegion(input->GetLargestPossibleRegion());
      }
    }
}

//----------------------------------------------------------------------------
template< typename TInputImage >
unsigned int
ImageTransformer< TInputImage >
::SplitRequestedRegion(unsigned int i, unsigned int num, InputImageRegionType & splitRegion)
{
  // Get the input pointer
  InputImageType *inputPtr = this->GetInput();

  const typename TInputImage::SizeType & requestedRegionSize =
    inputPtr->GetRequestedRegion().GetSize();

  int splitAxis;
  typename TInputImage::IndexType splitIndex;
  typename TInputImage::SizeType splitSize;

  // Initialize the splitRegion to the input requested region
  splitRegion = inputPtr->GetRequestedRegion();
  splitIndex = splitRegion.GetIndex();
  splitSize = splitRegion.GetSize();

  // split on the outermost dimension available
  splitAxis = inputPtr->GetImageDimension() - 1;
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
  typename TInputImage::SizeType::SizeValueType range = requestedRegionSize[splitAxis];
  if ( num != 0 && range !=0 )
      {
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
  else
      {
      itkDebugMacro( "Division by zero: num/range = 0." );
      return 1;
      }
}

//----------------------------------------------------------------------------
template< typename TInputImage >
void
ImageTransformer< TInputImage >
::AllocateOutputs()
{
}

//----------------------------------------------------------------------------
template< typename TInputImage >
void
ImageTransformer< TInputImage >
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
template< typename TInputImage >
void
ImageTransformer< TInputImage >
::ThreadedGenerateData(const InputImageRegionType &,
                       ThreadIdType)
{
// The following code is equivalent to:
// itkExceptionMacro("subclass should override this method!!!");
// The ExceptionMacro is not used because gcc warns that a
// 'noreturn' function does return
  std::ostringstream message;

  message << "itk::ERROR: " << this->GetNameOfClass()
          << "(" << this << "): " << "Subclass should override this method!!!";
  ExceptionObject e_(__FILE__, __LINE__, message.str().c_str(), ITK_LOCATION);
  throw e_;
}

// Callback routine used by the threading library. This routine just calls
// the ThreadedGenerateData method after setting the correct region for this
// thread.
template< typename TInputImage >
ITK_THREAD_RETURN_TYPE
ImageTransformer< TInputImage >
::ThreaderCallback(void *arg)
{
  ThreadStruct *str;
  ThreadIdType  total, threadId, threadCount;

  threadId = ( (MultiThreader::ThreadInfoStruct *)( arg ) )->ThreadID;
  threadCount = ( (MultiThreader::ThreadInfoStruct *)( arg ) )->NumberOfThreads;

  str = (ThreadStruct *)( ( (MultiThreader::ThreadInfoStruct *)( arg ) )->UserData );

  // execute the actual method with appropriate output region
  // first find out how many pieces extent can be split into.
  typename TInputImage::RegionType splitRegion;
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
