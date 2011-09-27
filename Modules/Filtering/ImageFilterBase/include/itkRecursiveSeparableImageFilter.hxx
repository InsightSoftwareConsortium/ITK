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
#ifndef __itkRecursiveSeparableImageFilter_hxx
#define __itkRecursiveSeparableImageFilter_hxx

#include "itkRecursiveSeparableImageFilter.h"
#include "itkObjectFactory.h"
#include "itkImageLinearIteratorWithIndex.h"
#include "itkProgressReporter.h"
#include <new>

namespace itk
{
template< typename TInputImage, typename TOutputImage >
RecursiveSeparableImageFilter< TInputImage, TOutputImage >
::RecursiveSeparableImageFilter()
{
  m_Direction = 0;
  this->SetNumberOfRequiredOutputs(1);
  this->SetNumberOfRequiredInputs(1);

  this->InPlaceOff();
}

/**
 * Set Input Image
 */
template< typename TInputImage, typename TOutputImage >
void
RecursiveSeparableImageFilter< TInputImage, TOutputImage >
::SetInputImage(const TInputImage *input)
{
  // ProcessObject is not const_correct so this const_cast is required
  ProcessObject::SetNthInput( 0,
                              const_cast< TInputImage * >( input ) );
}

/**
 * Get Input Image
 */
template< typename TInputImage, typename TOutputImage >
const TInputImage *
RecursiveSeparableImageFilter< TInputImage, TOutputImage >
::GetInputImage(void)
{
  return dynamic_cast< const TInputImage * >(
           ( ProcessObject::GetInput(0) ) );
}

/**
 * Apply Recursive Filter
 */
template< typename TInputImage, typename TOutputImage >
void
RecursiveSeparableImageFilter< TInputImage, TOutputImage >
::FilterDataArray(RealType *outs, const RealType *data,
                  RealType *scratch, unsigned int ln)
{
  /**
   * Causal direction pass
   */

  // this value is assumed to exist from the border to infinity.
  const RealType outV1 = data[0];

  /**
   * Initialize borders
   */
  scratch[0] = RealType(outV1   * m_N0 +   outV1 * m_N1 + outV1   * m_N2 + outV1 * m_N3);
  scratch[1] = RealType(data[1] * m_N0 +   outV1 * m_N1 + outV1   * m_N2 + outV1 * m_N3);
  scratch[2] = RealType(data[2] * m_N0 + data[1] * m_N1 + outV1   * m_N2 + outV1 * m_N3);
  scratch[3] = RealType(data[3] * m_N0 + data[2] * m_N1 + data[1] * m_N2 + outV1 * m_N3);

  // note that the outV1 value is multiplied by the Boundary coefficients m_BNi
  scratch[0] -= RealType(outV1      * m_BN1 + outV1      * m_BN2 + outV1      * m_BN3 + outV1 * m_BN4);
  scratch[1] -= RealType(scratch[0] * m_D1  + outV1      * m_BN2 + outV1      * m_BN3  + outV1 * m_BN4);
  scratch[2] -= RealType(scratch[1] * m_D1  + scratch[0] * m_D2  + outV1      * m_BN3  + outV1 * m_BN4);
  scratch[3] -= RealType(scratch[2] * m_D1  + scratch[1] * m_D2  + scratch[0] * m_D3   + outV1 * m_BN4);

  /**
   * Recursively filter the rest
   */
  for ( unsigned int i = 4; i < ln; i++ )
    {
    scratch[i]  = RealType(data[i]      * m_N0 + data[i - 1]    * m_N1 + data[i - 2]    * m_N2 + data[i - 3]    * m_N3);
    scratch[i] -= RealType(
      scratch[i - 1] * m_D1 + scratch[i - 2] * m_D2 + scratch[i - 3] * m_D3 + scratch[i - 4] * m_D4);
    }

  /**
   * Store the causal result
   */
  for ( unsigned int i = 0; i < ln; i++ )
    {
    outs[i] = scratch[i];
    }

  /**
   * AntiCausal direction pass
   */

  // this value is assumed to exist from the border to infinity.
  const RealType outV2 = data[ln - 1];

  /**
   * Initialize borders
   */
  scratch[ln - 1] = RealType(outV2      * m_M1 + outV2      * m_M2 + outV2      * m_M3 + outV2 * m_M4);
  scratch[ln - 2] = RealType(data[ln - 1] * m_M1 + outV2      * m_M2 + outV2      * m_M3 + outV2 * m_M4);
  scratch[ln - 3] = RealType(data[ln - 2] * m_M1 + data[ln - 1] * m_M2 + outV2      * m_M3 + outV2 * m_M4);
  scratch[ln - 4] = RealType(data[ln - 3] * m_M1 + data[ln - 2] * m_M2 + data[ln - 1] * m_M3 + outV2 * m_M4);

  // note that the outV2value is multiplied by the Boundary coefficients m_BMi
  scratch[ln - 1] -= RealType(outV2         * m_BM1 + outV2         * m_BM2 + outV2         * m_BM3 + outV2 * m_BM4);
  scratch[ln - 2] -= RealType(scratch[ln - 1] * m_D1  + outV2         * m_BM2 + outV2         * m_BM3 + outV2 * m_BM4);
  scratch[ln - 3] -= RealType(scratch[ln - 2] * m_D1  + scratch[ln - 1] * m_D2  + outV2         * m_BM3 + outV2 * m_BM4);
  scratch[ln - 4] -= RealType(
    scratch[ln - 3] * m_D1  + scratch[ln - 2] * m_D2  + scratch[ln - 1] * m_D3  + outV2 * m_BM4);

  /**
   * Recursively filter the rest
   */
  for ( unsigned int i = ln - 4; i > 0; i-- )
    {
    scratch[i - 1]  = RealType(
      data[i]    * m_M1 + data[i + 1]    * m_M2 + data[i + 2]    * m_M3 + data[i + 3]    * m_M4);
    scratch[i - 1] -= RealType(
      scratch[i] * m_D1 + scratch[i + 1] * m_D2 + scratch[i + 2] * m_D3 + scratch[i + 3] * m_D4);
    }

  /**
   * Roll the antiCausal part into the output
   */
  for ( unsigned int i = 0; i < ln; i++ )
    {
    outs[i] += scratch[i];
    }
}

//
// we need all of the image in just the "Direction" we are separated into
//
template< typename TInputImage, typename TOutputImage >
void
RecursiveSeparableImageFilter< TInputImage, TOutputImage >
::EnlargeOutputRequestedRegion(DataObject *output)
{
  TOutputImage *out = dynamic_cast< TOutputImage * >( output );

  if ( out )
    {
    OutputImageRegionType         outputRegion = out->GetRequestedRegion();
    const OutputImageRegionType & largestOutputRegion = out->GetLargestPossibleRegion();

    // verify sane parameter
    if ( this->m_Direction >=  outputRegion.GetImageDimension() )
      {
      itkExceptionMacro("Direction selected for filtering is greater than ImageDimension")
      }

    // expand output region to match largest in the "Direction" dimension
    outputRegion.SetIndex( m_Direction, largestOutputRegion.GetIndex(m_Direction) );
    outputRegion.SetSize( m_Direction, largestOutputRegion.GetSize(m_Direction) );

    out->SetRequestedRegion(outputRegion);
    }
}

template< typename TInputImage, typename TOutputImage >
unsigned int
RecursiveSeparableImageFilter< TInputImage, TOutputImage >
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
  // and avoid the current dimension
  splitAxis = outputPtr->GetImageDimension() - 1;
  while ( requestedRegionSize[splitAxis] == 1 || splitAxis == (int)m_Direction )
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
  unsigned int valuesPerThread = (unsigned int)vcl_ceil(range / (double)num);
  unsigned int maxThreadIdUsed = (unsigned int)vcl_ceil(range / (double)valuesPerThread) - 1;

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

template< typename TInputImage, typename TOutputImage >
void
RecursiveSeparableImageFilter< TInputImage, TOutputImage >
::BeforeThreadedGenerateData()
{
  typedef ImageRegion< TInputImage::ImageDimension > RegionType;

  typename TInputImage::ConstPointer inputImage( this->GetInputImage () );
  typename TOutputImage::Pointer     outputImage( this->GetOutput() );

  const unsigned int imageDimension = inputImage->GetImageDimension();

  if ( this->m_Direction >= imageDimension )
    {
    itkExceptionMacro("Direction selected for filtering is greater than ImageDimension");
    }

  const typename InputImageType::SpacingType & pixelSize =
    inputImage->GetSpacing();

  this->SetUp(pixelSize[m_Direction]);

  RegionType region = outputImage->GetRequestedRegion();

  const unsigned int ln = region.GetSize()[this->m_Direction];

  if ( ln < 4 )
    {
    itkExceptionMacro(
      "The number of pixels along direction " << this->m_Direction
                                              <<
      " is less than 4. This filter requires a minimum of four pixels along the dimension to be processed.");
    }
}

/**
 * Compute Recursive filter
 * line by line in one of the dimensions
 */
template< typename TInputImage, typename TOutputImage >
void
RecursiveSeparableImageFilter< TInputImage, TOutputImage >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread, ThreadIdType threadId)
{
  typedef typename TOutputImage::PixelType OutputPixelType;

  typedef ImageLinearConstIteratorWithIndex< TInputImage > InputConstIteratorType;
  typedef ImageLinearIteratorWithIndex< TOutputImage >     OutputIteratorType;

  typedef ImageRegion< TInputImage::ImageDimension > RegionType;

  typename TInputImage::ConstPointer inputImage( this->GetInputImage () );
  typename TOutputImage::Pointer     outputImage( this->GetOutput() );

  RegionType region = outputRegionForThread;

  InputConstIteratorType inputIterator(inputImage,  region);
  OutputIteratorType     outputIterator(outputImage, region);

  inputIterator.SetDirection(this->m_Direction);
  outputIterator.SetDirection(this->m_Direction);

  const unsigned int ln = region.GetSize()[this->m_Direction];

  RealType *inps = 0;
  RealType *outs = 0;
  RealType *scratch = 0;

  try
    {
    inps = new RealType[ln];
    }
  catch ( std::bad_alloc & )
    {
    itkExceptionMacro("Problem allocating memory for internal computations");
    }

  try
    {
    outs = new RealType[ln];
    }
  catch ( std::bad_alloc & )
    {
    delete[] inps;
    itkExceptionMacro("Problem allocating memory for internal computations");
    }

  try
    {
    scratch = new RealType[ln];
    }
  catch ( std::bad_alloc & )
    {
    delete[] inps;
    delete[] outs;
    itkExceptionMacro("Problem allocating memory for internal computations");
    }

  inputIterator.GoToBegin();
  outputIterator.GoToBegin();

  const typename TInputImage::OffsetValueType * offsetTable = inputImage->GetOffsetTable();

  const unsigned int numberOfLinesToProcess = offsetTable[TInputImage::ImageDimension] / ln;
  ProgressReporter   progress(this, threadId, numberOfLinesToProcess, 10);

  try  // this try is intended to catch an eventual AbortException.
    {
    while ( !inputIterator.IsAtEnd() && !outputIterator.IsAtEnd() )
      {
      unsigned int i = 0;
      while ( !inputIterator.IsAtEndOfLine() )
        {
        inps[i++]      = inputIterator.Get();
        ++inputIterator;
        }

      this->FilterDataArray(outs, inps, scratch, ln);

      unsigned int j = 0;
      while ( !outputIterator.IsAtEndOfLine() )
        {
        outputIterator.Set( static_cast< OutputPixelType >( outs[j++] ) );
        ++outputIterator;
        }

      inputIterator.NextLine();
      outputIterator.NextLine();

      // Although the method name is CompletedPixel(),
      // this is being called after each line is processed
      progress.CompletedPixel();
      }
    }
  catch ( ProcessAborted  & )
    {
    // User aborted filter excecution Here we catch an exception thrown by the
    // progress reporter and rethrow it with the correct line number and file
    // name. We also invoke AbortEvent in case some observer was interested on
    // it.
    // release locally allocated memory
    delete[] outs;
    delete[] inps;
    delete[] scratch;
    // Throw the final exception.
    ProcessAborted e(__FILE__, __LINE__);
    e.SetDescription("Process aborted.");
    e.SetLocation(ITK_LOCATION);
    throw e;
    }

  delete[] outs;
  delete[] inps;
  delete[] scratch;
}

template< typename TInputImage, typename TOutputImage >
void
RecursiveSeparableImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Direction: " << m_Direction << std::endl;
}
} // end namespace itk

#endif
