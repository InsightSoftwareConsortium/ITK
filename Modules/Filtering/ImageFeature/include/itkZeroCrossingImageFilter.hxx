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
#ifndef itkZeroCrossingImageFilter_hxx
#define itkZeroCrossingImageFilter_hxx

#include "itkConstNeighborhoodIterator.h"
#include "itkZeroCrossingImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkFixedArray.h"
#include "itkProgressReporter.h"
#include "itkMath.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage >
void
ZeroCrossingImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // get pointers to the input and output
  typename Superclass::InputImagePointer inputPtr =
    const_cast< TInputImage * >( this->GetInput() );
  typename Superclass::OutputImagePointer outputPtr = this->GetOutput();

  if ( !inputPtr || !outputPtr )
    {
    return;
    }

  // Build an operator so that we can determine the kernel size
  SizeValueType radius = NumericTraits< SizeValueType >::ZeroValue();

  // get a copy of the input requested region (should equal the output
  // requested region)
  typename TInputImage::RegionType inputRequestedRegion;
  inputRequestedRegion = inputPtr->GetRequestedRegion();

  // pad the input requested region by the operator radius
  inputRequestedRegion.PadByRadius(radius);

  // crop the input requested region at the input's largest possible region
  if ( inputRequestedRegion.Crop( inputPtr->GetLargestPossibleRegion() ) )
    {
    inputPtr->SetRequestedRegion(inputRequestedRegion);
    return;
    }
  else
    {
    // Couldn't crop the region (requested region is outside the largest
    // possible region).  Throw an exception.

    // store what we tried to request (prior to trying to crop)
    inputPtr->SetRequestedRegion(inputRequestedRegion);

    // build an exception
    InvalidRequestedRegionError e(__FILE__, __LINE__);
    e.SetLocation(ITK_LOCATION);
    e.SetDescription("Requested region is (at least partially) outside the largest possible region.");
    e.SetDataObject(inputPtr);
    throw e;
    }
}

template< typename TInputImage, typename TOutputImage >
void
ZeroCrossingImageFilter< TInputImage, TOutputImage >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  unsigned int i;

  ZeroFluxNeumannBoundaryCondition< TInputImage > nbc;

  ConstNeighborhoodIterator< TInputImage > bit;
  ImageRegionIterator< TOutputImage >      it;

  typename OutputImageType::Pointer output = this->GetOutput();
  typename  InputImageType::ConstPointer input  = this->GetInput();

  // Calculate iterator radius
  Size< ImageDimension > radius;
  radius.Fill(1);

  // Find the data-set boundary "faces"
  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< TInputImage >::
  FaceListType faceList;
  NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< TInputImage > bC;
  faceList = bC(input, outputRegionForThread, radius);

  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< TInputImage >::
  FaceListType::iterator fit;

  // support progress methods/callbacks
  ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels() );

  InputImagePixelType this_one, that, abs_this_one, abs_that;
  InputImagePixelType zero = NumericTraits< InputImagePixelType >::ZeroValue();

  FixedArray< OffsetValueType, 2 *ImageDimension > offset;

  bit = ConstNeighborhoodIterator< InputImageType >( radius,
                                                     input,
                                                     *faceList.begin() );
  //Set the offset of the neighbors to the center pixel.
  for ( i = 0; i < ImageDimension; i++ )
    {
    offset[i] = -1 * static_cast< OffsetValueType >( bit.GetStride(i) );
    offset[i + ImageDimension] =  bit.GetStride(i);
    }

  // Process each of the boundary faces.  These are N-d regions which border
  // the edge of the buffer.
  for ( fit = faceList.begin(); fit != faceList.end(); ++fit )
    {
    bit = ConstNeighborhoodIterator< InputImageType >(radius,
                                                      input, *fit);
    it = ImageRegionIterator< OutputImageType >(output, *fit);
    bit.OverrideBoundaryCondition(&nbc);
    bit.GoToBegin();

    const SizeValueType center = bit.Size() / 2;
    while ( !bit.IsAtEnd() )
      {
      this_one = bit.GetPixel(center);
      it.Set(m_BackgroundValue);
      for ( i = 0; i < ImageDimension * 2; i++ )
        {
        that = bit.GetPixel(center + offset[i]);
        if ( ( ( this_one < zero ) && ( that > zero ) )
             || ( ( this_one > zero ) && ( that < zero ) )
             || ( ( Math::ExactlyEquals(this_one, zero) ) && ( Math::NotExactlyEquals(that, zero) ) )
             || ( ( Math::NotExactlyEquals(this_one, zero) ) && ( Math::ExactlyEquals(that, zero) ) ) )
          {
          abs_this_one =  itk::Math::abs(this_one);
          abs_that = itk::Math::abs(that);
          if ( abs_this_one < abs_that )
            {
            it.Set(m_ForegroundValue);
            break;
            }
          else if ( Math::ExactlyEquals(abs_this_one, abs_that) && i >= ImageDimension )
            {
            it.Set(m_ForegroundValue);
            break;
            }
          }
        }
      ++bit;
      ++it;
      progress.CompletedPixel();
      }
    }
}

template< typename TInputImage, typename TOutputImage >
void
ZeroCrossingImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "ForegroundValue: "
     << static_cast< typename NumericTraits< OutputImagePixelType >::PrintType >( m_ForegroundValue )
     << std::endl;
  os << indent << "BackgroundValue: "
     << static_cast< typename NumericTraits< OutputImagePixelType >::PrintType >( m_BackgroundValue )
     << std::endl;
}
} //end of itk namespace

#endif
