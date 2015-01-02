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
#ifndef itkRecursiveSeparableImageFilter_hxx
#define itkRecursiveSeparableImageFilter_hxx

#include "itkRecursiveSeparableImageFilter.h"
#include "itkObjectFactory.h"
#include "itkImageLinearIteratorWithIndex.h"
#include "itkProgressReporter.h"
#include <new>

namespace itk
{
template< typename TInputImage, typename TOutputImage >
RecursiveSeparableImageFilter< TInputImage, TOutputImage >
::RecursiveSeparableImageFilter():
  m_N0( 1.0 ),
  m_N1( 1.0 ),
  m_N2( 1.0 ),
  m_N3( 1.0 ),
  m_D1( 0.0 ),
  m_D2( 0.0 ),
  m_D3( 0.0 ),
  m_D4( 0.0 ),
  m_M1( 0.0 ),
  m_M2( 0.0 ),
  m_M3( 0.0 ),
  m_M4( 0.0 ),
  m_BN1( 0.0 ),
  m_BN2( 0.0 ),
  m_BN3( 0.0 ),
  m_BN4( 0.0 ),
  m_BM1( 0.0 ),
  m_BM2( 0.0 ),
  m_BM3( 0.0 ),
  m_BM4( 0.0 ),
  m_Direction( 0 ),
  m_ImageRegionSplitter(ImageRegionSplitterDirection::New())
{
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
                  RealType *scratch, SizeValueType ln)
{

  RealType * scratch1 = outs;
  RealType * scratch2 = scratch;
  /**
   * Causal direction pass
   */

  // this value is assumed to exist from the border to infinity.
  const RealType &outV1 = data[0];

  /**
   * Initialize borders
   */

  MathEMAMAMAM( scratch1[0], outV1  , m_N0,   outV1, m_N1, outV1  , m_N2, outV1, m_N3 );
  MathEMAMAMAM( scratch1[1], data[1], m_N0,   outV1, m_N1, outV1  , m_N2, outV1, m_N3 );
  MathEMAMAMAM( scratch1[2], data[2], m_N0, data[1], m_N1, outV1  , m_N2, outV1, m_N3 );
  MathEMAMAMAM( scratch1[3], data[3], m_N0, data[2], m_N1, data[1], m_N2, outV1, m_N3 );

  // note that the outV1 value is multiplied by the Boundary coefficients m_BNi
  MathSMAMAMAM( scratch1[0], outV1     , m_BN1, outV1     , m_BN2, outV1     , m_BN3, outV1, m_BN4);
  MathSMAMAMAM( scratch1[1], scratch1[0], m_D1 , outV1     , m_BN2, outV1     , m_BN3 , outV1, m_BN4);
  MathSMAMAMAM( scratch1[2], scratch1[1], m_D1 , scratch1[0], m_D2 , outV1     , m_BN3 , outV1, m_BN4);
  MathSMAMAMAM( scratch1[3], scratch1[2], m_D1 , scratch1[1], m_D2 , scratch1[0], m_D3  , outV1, m_BN4);

  /**
   * Recursively filter the rest
   */
  for ( unsigned int i = 4; i < ln; i++ )
    {
    MathEMAMAMAM( scratch1[i], data[i], m_N0, data[i - 1]   , m_N1, data[i - 2]   , m_N2, data[i - 3]   , m_N3);
    MathSMAMAMAM( scratch1[i], scratch1[i - 1], m_D1, scratch1[i - 2], m_D2, scratch1[i - 3], m_D3, scratch1[i - 4], m_D4);
    }

  /**
   * Store the causal result: outs = scratch already done via alias
   *
   */

  /**
   * AntiCausal direction pass
   */

  // this value is assumed to exist from the border to infinity.
  const RealType &outV2 = data[ln - 1];

  /**
   * Initialize borders
   */
  MathEMAMAMAM( scratch2[ln - 1], outV2       , m_M1, outV2     , m_M2, outV2     , m_M3, outV2, m_M4);
  MathEMAMAMAM( scratch2[ln - 2], data[ln - 1], m_M1, outV2     , m_M2, outV2     , m_M3, outV2, m_M4);
  MathEMAMAMAM( scratch2[ln - 3], data[ln - 2], m_M1, data[ln - 1], m_M2, outV2     , m_M3, outV2, m_M4);
  MathEMAMAMAM( scratch2[ln - 4], data[ln - 3], m_M1, data[ln - 2], m_M2, data[ln - 1], m_M3, outV2, m_M4);

  // note that the outV2value is multiplied by the Boundary coefficients m_BMi
  MathSMAMAMAM( scratch2[ln - 1], outV2          , m_BM1, outV2        , m_BM2, outV2        , m_BM3, outV2, m_BM4);
  MathSMAMAMAM( scratch2[ln - 2], scratch2[ln - 1], m_D1 , outV2        , m_BM2, outV2        , m_BM3, outV2, m_BM4);
  MathSMAMAMAM( scratch2[ln - 3], scratch2[ln - 2], m_D1 , scratch2[ln - 1], m_D2 , outV2        , m_BM3, outV2, m_BM4);
  MathSMAMAMAM( scratch2[ln - 4], scratch2[ln - 3], m_D1 , scratch2[ln - 2], m_D2 , scratch2[ln - 1], m_D3 , outV2, m_BM4);

  /**
   * Recursively filter the rest
   */
  for ( unsigned int i = ln - 4; i > 0; i-- )
    {
    MathEMAMAMAM( scratch2[i - 1], data[i]   , m_M1, data[i + 1]   , m_M2, data[i + 2]   , m_M3, data[i + 3]   , m_M4);
    MathSMAMAMAM( scratch2[i - 1], scratch2[i], m_D1, scratch2[i + 1], m_D2, scratch2[i + 2], m_D3, scratch2[i + 3], m_D4);
    }

  /**
   * Roll the antiCausal part into the output
   */
  for ( unsigned int i = 0; i < ln; i++ )
    {
    outs[i] += scratch2[i];
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
const ImageRegionSplitterBase*
RecursiveSeparableImageFilter< TInputImage, TOutputImage >
::GetImageRegionSplitter(void) const
{
  return this->m_ImageRegionSplitter;
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


  this->m_ImageRegionSplitter->SetDirection(m_Direction);

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

  const SizeValueType ln = region.GetSize(this->m_Direction);

  RealType *inps = ITK_NULLPTR;
  RealType *outs = ITK_NULLPTR;
  RealType *scratch = ITK_NULLPTR;

  try
    {
    inps = new RealType[ln];
    outs = new RealType[ln];
    scratch = new RealType[ln];

    inputIterator.GoToBegin();
    outputIterator.GoToBegin();

    const SizeValueType numberOfLinesToProcess = outputRegionForThread.GetNumberOfPixels() / outputRegionForThread.GetSize(this->m_Direction);
    ProgressReporter   progress(this, threadId, numberOfLinesToProcess, 10);

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
  catch (...)
    {
    // Consider cases where memory allocation may fail or the process
    // is aborted.

    // release locally allocated memory, if memory allocation fails
    // then we will delete a ITK_NULLPTR pointer, which is a valid operation
    delete[] outs;
    delete[] inps;
    delete[] scratch;

    // rethrow same exception
    throw;
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
