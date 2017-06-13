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
#ifndef itkBSplineResampleImageFilterBase_hxx
#define itkBSplineResampleImageFilterBase_hxx

#include "itkBSplineResampleImageFilterBase.h"

namespace itk
{
/**
 * Constructor
 */
template< typename TInputImage, typename TOutputImage >
BSplineResampleImageFilterBase< TInputImage, TOutputImage >
::BSplineResampleImageFilterBase()
{
  m_SplineOrder = -1;
  int SplineOrder = 0;
  // Because of inheritance the user must explicitly set this for m_SplineOrder
  // != 0.
  this->SetSplineOrder(SplineOrder);
}

/**
 * Standard "PrintSelf" method
 */
template< typename TInputImage, typename TOutputImage >
void
BSplineResampleImageFilterBase< TInputImage, TOutputImage >
::PrintSelf(
  std::ostream & os,
  Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Spline Order: " << m_SplineOrder << std::endl;
}

/**
 * Intializes the Pyramid Spline Filter parameters for an "l2" filter
 */
template< typename TInputImage, typename TOutputImage >
void BSplineResampleImageFilterBase< TInputImage, TOutputImage >
::InitializePyramidSplineFilter(int SplineOrder)
{
  switch ( SplineOrder )
    {
    case 0:
      m_GSize = 1;
      m_HSize = 1;
      break;

    case 1:
      m_GSize = 9;
      m_HSize = 2;
      m_G.resize(m_GSize);
      m_H.resize(m_HSize);
      m_G[0]  =  0.707107;
      m_G[1]  =  0.292893;
      m_G[2]  = -0.12132;
      m_G[3]  = -0.0502525;
      m_G[4]  =  0.0208153;
      m_G[5]  =  0.00862197;
      m_G[6]  = -0.00357134;
      m_G[7]  = -0.0014793;
      m_G[8]  =  0.000612745;
      m_H[0]  = 1.;
      m_H[1]  = 0.5;
      break;
    case 2:
      m_GSize = 16;
      m_HSize = 10;
      m_G.resize(m_GSize);
      m_H.resize(m_HSize);
      m_G[0]  =  0.617317;
      m_G[1]  =  0.310754;
      m_G[2]  = -0.0949641;
      m_G[3]  = -0.0858654;
      m_G[4]  =  0.0529153;
      m_G[5]  =  0.0362437;
      m_G[6]  = -0.0240408;
      m_G[7]  = -0.0160987;
      m_G[8]  =  0.0107498;
      m_G[9]  =  0.00718418;
      m_G[10] = -0.00480004;
      m_G[11] = -0.00320734;
      m_G[12] =  0.00214306;
      m_G[13] =  0.00143195;
      m_G[14] = -0.0009568;
      m_G[15] = -0.000639312;
      m_H[0]  =  1.;
      m_H[1]  =  0.585786;
      m_H[2]  =  0;
      m_H[3]  = -0.100505;
      m_H[4]  =  0;
      m_H[5]  =  0.0172439;
      m_H[6]  =  0;
      m_H[7]  = -0.00295859;
      m_H[8]  =  0;
      m_H[9]  =  0.000507614;
      break;
    case 3:
      m_GSize = 20;
      m_HSize = 12;
      m_G.resize(m_GSize);
      m_H.resize(m_HSize);
      m_G[0]  =  0.596797;
      m_G[1]  =  0.313287;
      m_G[2]  = -0.0827691;
      m_G[3]  = -0.0921993;
      m_G[4]  =  0.0540288;
      m_G[5]  =  0.0436996;
      m_G[6]  = -0.0302508;
      m_G[7]  = -0.0225552;
      m_G[8]  =  0.0162251;
      m_G[9]  =  0.0118738;
      m_G[10] = -0.00861788;
      m_G[11] = -0.00627964;
      m_G[12] =  0.00456713;
      m_G[13] =  0.00332464;
      m_G[14] = -0.00241916;
      m_G[15] = -0.00176059;
      m_G[16] =  0.00128128;
      m_G[17] =  0.000932349;
      m_G[18] = -0.000678643;
      m_G[19] = -0.000493682;
      m_H[0]  =  1.;
      m_H[1]  =  0.600481;
      m_H[2]  =  0;
      m_H[3]  = -0.127405;
      m_H[4]  =  0;
      m_H[5]  =  0.034138;
      m_H[6]  =  0;
      m_H[7]  = -0.00914725;
      m_H[8]  =  0;
      m_H[9]  =  0.002451;
      m_H[10] =  0;
      m_H[11] = -0.000656743;
      break;
    default:
      // Throw an exception
      ExceptionObject err(__FILE__, __LINE__);
      err.SetLocation(ITK_LOCATION);
      err.SetDescription(
        "SplineOrder for l2 pyramid filter must be between 1 and 3. Requested spline order has not been implemented.");
      throw err;
      break;
    }
}

template< typename TInputImage, typename TOutputImage >
void BSplineResampleImageFilterBase< TInputImage, TOutputImage >
::SetSplineOrder(int splineOrder)
{
  if ( splineOrder == m_SplineOrder )
    {
    return;
    }
  m_SplineOrder = splineOrder;

  this->InitializePyramidSplineFilter(m_SplineOrder);
  this->Modified();
}

/** Reduce1DImage - reduces the vector of data (in) by a
 *     factor of 2 and writes the results to the location specified
 *     by the Iterator (out).  inTraverseSize is the size of the in vector.
 */
template< typename TInputImage, typename TOutputImage >
void BSplineResampleImageFilterBase< TInputImage, TOutputImage >
::Reduce1DImage(const std::vector< double > & in,   OutputImageIterator & out,
                unsigned int inTraverseSize, ProgressReporter & progress)
{
  int i1, i2;

  unsigned int outK, inK;
  unsigned int outTraverseSize = inTraverseSize / 2;

  inTraverseSize = outTraverseSize * 2; // ensures that an even number is used.
  unsigned int inModK;                  // number for modulus math of in
  inModK = inTraverseSize - 1;

  double outVal;

  //TODO:  m_GSize < 2 has not been tested.
  if ( m_GSize < 2 )
    {
    for ( outK = 0; outK < outTraverseSize; outK++ )
      {
      inK = 2 * outK;
      i2 = inK + 1;
      if ( i2 > (int)inModK )
        {
        //Original was
        //i2=inModK-i2;
        //I don't think this is correct since this would be negative
        i2 = inModK - ( i2 % inModK );  // Should I use this always instead of
                                        // the if statement?
        }
      out.Set( static_cast< OutputImagePixelType >( ( in[inK] + in[i2] ) / 2.0 ) );
      ++out;
      progress.CompletedPixel();
      }
    }

  else
    {
    for ( outK = 0; outK < outTraverseSize; outK++ )
      {
      inK = 2L * outK;

      outVal = in[inK] * m_G[0];

      for ( int i = 1; i < m_GSize; i++ )
        {
        // Calculate indices for left and right of symmetrical filter.
        i1 = inK - i;
        i2 = inK + i;
        // reflect at boundaries if necessary
        if ( i1 < 0 )
          {
          i1 = ( -i1 ) % inModK;
          // Removed because i1 can never be greater than inModK, right?
          //if (i1 > inModK)
          //i1=inModK-i1;  //TODO: I don't think this is correct.
          }
        if ( i2 > (int)inModK )
          {
          if (inModK)
            {
            i2 = i2 % inModK;
            }
          // Removed because i1 can never be greater than inModK, right?
          //if (i2 > inModK)
          //i2=inModK-i2;  //TODO: I don't think this is correct.
          }
        outVal = outVal + m_G[i] * ( in[i1] + in[i2] );
        }
      out.Set( static_cast< OutputImagePixelType >( outVal ) );
      ++out;
      progress.CompletedPixel();
      }
    }
}

/** Expand1DImage - expands the vector of data (in) by a
 *     factor of 2 and writes the results to the location specified
 *     by the Iterator (out).  inTraverseSize is the size of the in vector.
 */
template< typename TInputImage, typename TOutputImage >
void BSplineResampleImageFilterBase< TInputImage, TOutputImage >
::Expand1DImage(const std::vector< double > & in, OutputImageIterator & out,
                unsigned int inTraverseSize, ProgressReporter & progress)
{
  int i1, i2;

  int          outK;
  unsigned int inK;
  unsigned int outTraverseSize = inTraverseSize * 2;
  //inTraverseSize = outTraverseSize/2;  // ensures that an even number is used.
  int inModK; // number for modulus math of in

  inModK = inTraverseSize - 1;

  double outVal;

  //TODO:  m_GSize < 2 has not been tested.
  if ( m_HSize < 2 )
    {
    for ( inK = 0; inK < inTraverseSize; inK++ )
      {
      out.Set( static_cast< OutputImagePixelType >( in[inK] ) );
      ++out;
      out.Set( static_cast< OutputImagePixelType >( in[inK] ) );
      ++out;
      }
    progress.CompletedPixel();
    }

  else
    {
    for ( outK = 0; outK < (int)outTraverseSize; outK++ )
      {
      outVal = 0.0;
      for ( int k = ( outK % 2 ); k < (int)m_HSize; k += 2 )
        {
        i1 = ( outK - k ) / 2;
        if ( i1 < 0 )
          {
          i1 = ( -i1 ) % inModK;
          // The following could never happen therefore removed.
          //if (i1 > inModK)
          //i1 - inModK - i1;
          }
        outVal = outVal + m_H[k] * in[i1];
        }
      for ( int k = 2 - ( outK % 2 ); k < (int)m_HSize; k += 2 )
        {
        i2 = ( outK + k ) / 2;
        if ( i2 > inModK )
          {
          i2 = i2 % inModK;
          i2 = inModK - i2;
          // The following could never happen therefore removed.
          //if (i2 > inModK)
          //i2 - inModK - i2;
          }
        outVal += m_H[k] * in[i2];
        }

      out.Set( static_cast< OutputImagePixelType >( outVal ) );
      ++out;
      progress.CompletedPixel();
      }
    }
}

/**  Reduce an Image by a factor of 2 in each dimension.
 */
template< typename TInputImage, typename TOutputImage >
void BSplineResampleImageFilterBase< TInputImage, TOutputImage >
::ReduceNDImage(OutputImageIterator & outItr)
{
  // Set up variables for waking the image regions.
  RegionType validRegion;
  SizeType   startSize;
  SizeType   currentSize;

  // Does not support streaming
  typename Superclass::InputImagePointer inputPtr = const_cast< TInputImage * >( this->GetInput() );
  startSize = inputPtr->GetBufferedRegion().GetSize();

  // Initialize scratchImage space and allocate memory
  InitializeScratch(startSize);
  typename TOutputImage::Pointer scratchImage;
  scratchImage =  TOutputImage::New();
  scratchImage->CopyInformation(inputPtr);
  RegionType scratchRegion;
  scratchRegion = inputPtr->GetBufferedRegion();
  currentSize = startSize;
  // scratchImage only needs the 1/2 the space of the original
  // image for the first dimension.
  // TODO: Is dividing by 2 correct or do I need something more complicated for
  // handling odd dimensioned
  //       images?  i.e. is the rounding handled correctly?
  currentSize[0] = currentSize[0] / 2;
  scratchRegion.SetSize(currentSize);
  scratchImage->SetRegions(scratchRegion);
  scratchImage->Allocate();

  currentSize = startSize;
  validRegion.SetSize(currentSize);
  validRegion.SetIndex ( inputPtr->GetBufferedRegion().GetIndex() );

  /** The data is handled in this routine to minimize data copying.
   * Alternative methods could be used which may permit the use of
   * streaming. On the first dimension the inIterator points to the
   * TInputImage and the outIterator points to the ScratchImage.  After
   * the first dimension the inIterator points to the ScratchImage (the
   * outIterator points to ScratchImage also).  The variable m_Scratch is
   * used to pass a single line for processing so that overwriting does
   * not occur.  On the final iteration outIterator points to the
   * OutputImage for direct writing into the final variable. */

  // The first time through the loop our input image is inputPtr
  typename TInputImage::ConstPointer workingImage;
  workingImage = inputPtr;

  unsigned int     count = scratchRegion.GetNumberOfPixels() * ImageDimension;
  ProgressReporter progress(this, 0, count, 10);
  for ( unsigned int n = 0; n < ImageDimension; n++ )
    {
    // Setup iterators for input image.
    ConstInputImageIterator  inIterator1(workingImage, validRegion);
    ConstOutputImageIterator inIterator2(scratchImage, scratchRegion);

    if ( n == 0 )
      {
      // First time through the loop we use the InputImage
      inIterator1.GoToBegin();
      inIterator1.SetDirection(n);
      }
    else
      {
      // After first time through the loop we use the scratch image which is of
      // the output type.
      inIterator2.GoToBegin();
      inIterator2.SetDirection(n);
      }

    // Setup iterators and bounds for output image.
    currentSize[n] = currentSize[n] / 2;  // reduce by a factor of 2
    validRegion.SetSize(currentSize);
    // TODO:  Is there a way to put this in the else statement below?
    OutputImageIterator outIterator(scratchImage, validRegion);
    if ( n == ( ImageDimension - 1 ) )
      {
      // Last time through the loop write directly to the output
      outIterator = outItr;
      }

    outIterator.GoToBegin();
    outIterator.SetDirection(n);

    if ( n == 0 )
      {
      while ( !inIterator1.IsAtEnd() )
        {
        // Copies one line of input to m_Scratch
        this->CopyInputLineToScratch(inIterator1);

        this->Reduce1DImage(m_Scratch, outIterator,  startSize[n], progress);
        inIterator1.NextLine();
        outIterator.NextLine();
        }
      }
    else
      {
      while ( !inIterator2.IsAtEnd() )
        {
        // Copies one line of input to m_Scratch
        this->CopyOutputLineToScratch(inIterator2);

        this->Reduce1DImage(m_Scratch, outIterator,  startSize[n], progress);
        inIterator2.NextLine();
        outIterator.NextLine();
        }
      }

    // After first loop the input image is scratchImage
    //workingImage = scratchImage;
    }
}

/**  Expand an Image by a factor of 2 in each dimension.
*/
template< typename TInputImage, typename TOutputImage >
void BSplineResampleImageFilterBase< TInputImage, TOutputImage >
::ExpandNDImage(OutputImageIterator & outItr)
{
  // Set up variables for waking the image regions.
  RegionType validRegion;
  SizeType   startSize;
  SizeType   currentSize;

  // Does not support streaming
  typename Superclass::InputImagePointer inputPtr = const_cast< TInputImage * >( this->GetInput() );
  startSize = inputPtr->GetBufferedRegion().GetSize();

  // Initialize scratchImage space and allocate memory
  InitializeScratch(startSize);
  typename TOutputImage::Pointer scratchImage;
  scratchImage =  TOutputImage::New();
  scratchImage->CopyInformation(inputPtr);
  RegionType scratchRegion;
  scratchRegion = inputPtr->GetBufferedRegion();
  currentSize = startSize;

  // scratchImage 2 times the space of the original image .

  for ( unsigned int n = 0; n < ImageDimension; n++ )
    {
    currentSize[n] = currentSize[n] * 2;
    }
  scratchRegion.SetSize(currentSize);
  scratchImage->SetBufferedRegion(scratchRegion);
  scratchImage->Allocate();

  currentSize = startSize;
  validRegion.SetSize(currentSize);
  validRegion.SetIndex ( inputPtr->GetBufferedRegion().GetIndex() );

  /** The data is handled in this routine to minimize data copying.  Alternative
      methods could be used which may permit the use of streaming. On the first
      dimension the inIterator points to the TInputImage and the outIterator
      points to the ScratchImage.  After the first dimension the inIterator points
      to the ScratchImage (the outIterator points to ScratchImage also).  The
      variable m_Scratch is used to pass a single line for processing so that
      overwriting does not occur.  On the final iteration outIterator points to
      the OutputImage for direct writing into the final variable.
  **/

  // The first time through the loop our input image is m_Image
  typename TInputImage::ConstPointer workingImage;
  workingImage = inputPtr;

  RegionType workingRegion = validRegion;

  unsigned int     count = scratchRegion.GetNumberOfPixels() * ImageDimension;
  ProgressReporter progress(this, 0, count, 10);
  for ( unsigned int n = 0; n < ImageDimension; n++ )
    {
    // Setup iterators for input image.
    ConstInputImageIterator  inIterator1(workingImage, workingRegion);
    ConstOutputImageIterator inIterator2(scratchImage, validRegion);
    if ( n == 0 )
      {
      // First time through the loop we use the InputImage
      inIterator1.GoToBegin();
      inIterator1.SetDirection(n);
      }
    else
      {
      // After first time through the loop we use the scratch image which is of
      // the output type.
      inIterator2.GoToBegin();
      inIterator2.SetDirection(n);
      }

    // Setup iterators and bounds for output image.
    currentSize[n] = currentSize[n] * 2;  // expand by a factor of 2
    validRegion.SetSize(currentSize);

    OutputImageIterator outIterator(scratchImage, validRegion);
    if ( n == ( ImageDimension - 1 ) )
      {
      // Last time through the loop write directly to the output
      outIterator = outItr;
      }

    outIterator.GoToBegin();
    outIterator.SetDirection(n);

    if ( n == 0 )
      {
      while ( !inIterator1.IsAtEnd() )
        {
        // Copies one line of input to m_Scratch
        this->CopyInputLineToScratch(inIterator1);

        this->Expand1DImage(m_Scratch, outIterator,  startSize[n], progress);
        inIterator1.NextLine();
        outIterator.NextLine();
        }
      }
    else
      {
      while ( !inIterator2.IsAtEnd() )
        {
        // Copies one line of input to m_Scratch
        this->CopyOutputLineToScratch(inIterator2);

        this->Expand1DImage(m_Scratch, outIterator,  startSize[n], progress);
        inIterator2.NextLine();
        outIterator.NextLine();
        }
      }
    }
}

// Allocate scratch space
template< typename TInputImage, typename TOutputImage >
void BSplineResampleImageFilterBase< TInputImage, TOutputImage >
::InitializeScratch(SizeType DataLength)
{
  unsigned int maxLength = 0;

  for ( unsigned int n = 0; n < ImageDimension; n++ )
    {
    if ( DataLength[n] > maxLength )
      {
      maxLength = DataLength[n];
      }
    }
  m_Scratch.resize(maxLength);
}

template< typename TInputImage, typename TOutputImage >
void BSplineResampleImageFilterBase< TInputImage, TOutputImage >
::CopyLineToScratch(ConstInputImageIterator & Iter)
{
  unsigned int j = 0;

  while ( !Iter.IsAtEndOfLine() )
    {
    m_Scratch[j] = static_cast< double >( Iter.Get() );
    ++Iter;
    ++j;
    }
}

template< typename TInputImage, typename TOutputImage >
void BSplineResampleImageFilterBase< TInputImage, TOutputImage >
::CopyInputLineToScratch(ConstInputImageIterator & Iter)
{
  unsigned int j = 0;

  while ( !Iter.IsAtEndOfLine() )
    {
    m_Scratch[j] = static_cast< double >( Iter.Get() );
    ++Iter;
    ++j;
    }
}

template< typename TInputImage, typename TOutputImage >
void BSplineResampleImageFilterBase< TInputImage, TOutputImage >
::CopyOutputLineToScratch(ConstOutputImageIterator & Iter)
{
  unsigned int j = 0;

  while ( !Iter.IsAtEndOfLine() )
    {
    m_Scratch[j] = static_cast< double >( Iter.Get() );
    ++Iter;
    ++j;
    }
}
} // namespace itk

#endif
