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
#ifndef itkInterpolateImagePointsFilter_hxx
#define itkInterpolateImagePointsFilter_hxx

#include "itkInterpolateImagePointsFilter.h"
#include "itkProgressReporter.h"

namespace itk
{

template< typename TInputImage, typename TOutputImage, typename TCoordType, typename InterpolatorType >
InterpolateImagePointsFilter< TInputImage, TOutputImage, TCoordType, InterpolatorType >
::InterpolateImagePointsFilter()
{
  m_Interpolator = InterpolatorType::New();
  m_DefaultPixelValue = 0;

  this->SetNumberOfRequiredInputs(ImageDimension+1);
}

template< typename TInputImage, typename TOutputImage, typename TCoordType, typename InterpolatorType >
void
InterpolateImagePointsFilter< TInputImage, TOutputImage, TCoordType, InterpolatorType >
::SetInputImage(const TInputImage *inputImage)
{
  this->SetInput(0, inputImage); // This is a data filter input
  // ***TODO:  Where should this be done?  During the filter update?
  //  should be converted to a filter.
  m_Interpolator->SetInputImage(inputImage);
}

template< typename TInputImage, typename TOutputImage, typename TCoordType, typename InterpolatorType >
void
InterpolateImagePointsFilter< TInputImage, TOutputImage, TCoordType, InterpolatorType >
::SetInterpolationCoordinate(const CoordImageType *coordinate, unsigned int setDimension)
{
  // Set each coordinate as an input.  Note that Input '0' is for the image.
  this->SetInput(setDimension + 1, coordinate); // This is a data filter input
}

template< typename TInputImage, typename TOutputImage, typename TCoordType, typename InterpolatorType >
void
InterpolateImagePointsFilter< TInputImage, TOutputImage, TCoordType, InterpolatorType >
::BeforeThreadedGenerateData()
{
  itkDebugMacro(<< "Actually Executing");

  // ***TODO: What needs to be done here? Should we set the input image at this
  // time?
  //  ** Also, where is the output allocated?

  // OutputImagePointer outputPtr = this->GetOutput();

  // outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  // outputPtr->Allocate();
}

template< typename TInputImage, typename TOutputImage, typename TCoordType, typename InterpolatorType >
void
InterpolateImagePointsFilter< TInputImage, TOutputImage, TCoordType, InterpolatorType >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  itkDebugMacro(<< "Actually Executing");

  OutputImagePointer outputPtr = this->GetOutput();

  // There is a direct mapping between the desired outputRegion and the
  // CoordRegion
  // coordRegion is set to outputRegionForThread.
  RegionCopierType     regionCopier;
  CoordImageRegionType coordRegion;
  regionCopier(coordRegion, outputRegionForThread);

  // Setup iterator for the output
  OutputImageIterator outIter(outputPtr, outputRegionForThread);
  //outIter.GoToBegin();  //Not sure if this is needed or inappropriate for
  // threading.

  // Setup an interator for each of the coordinate inputs.
  CoordImageIterator coordIter[ImageDimension];
  for ( unsigned int j = 0; j < ImageDimension; j++ )
    {
    CoordImageIterator temp(this->GetInput(j + 1), coordRegion);
//    temp.GoToBegin();   //Not sure if this is needed or inappropriate for threading.
    coordIter[j] = temp;
    }

  // Support for progress methods/callbacks
  ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels() );

  // Loop through each pixel and calculate interpolated value.
  ContinuousIndexType index;
  while ( !outIter.IsAtEnd() )
    {
    for ( unsigned int j = 0; j < ImageDimension; j++ )
      {
      index[j] = ( coordIter[j] ).Value();
      ++( coordIter[j] );
      }
    if ( m_Interpolator->IsInsideBuffer(index) )
      {
      outIter.Set( m_Interpolator->EvaluateAtContinuousIndex(index) );
      // TODO: How can we modify the code so that it could also interpolate
      // from a set of point coordinates instead of Continuous Index
      // coordinates?
      // If this line is used instead of the above line then this will
      // calculate from points. ( PointType point must replace index throughout
      // this function.)
      // outIter.Set( m_Interpolator->Evaluate( point );
      }
    else
      {
      outIter.Set(m_DefaultPixelValue);
      }
    ++outIter;
    progress.CompletedPixel();
    }
}

template< typename TInputImage, typename TOutputImage, typename TCoordType, typename InterpolatorType >
void
InterpolateImagePointsFilter< TInputImage, TOutputImage, TCoordType, InterpolatorType >
::GenerateInputRequestedRegion()
{
  // Call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  if ( !this->GetInput(0) )
    {
    return;
    }

  // The coordinates may have arbitrary ordering so the entire image must be
  // available for the filter. The coordinates on the other hand have the default
  // requested region size.
  InputImagePointer inputPtr = const_cast< TInputImage * >( this->GetInput(0) );
  inputPtr->SetRequestedRegionToLargestPossibleRegion();
}

template< typename TInputImage, typename TOutputImage, typename TCoordType, typename InterpolatorType >
void
InterpolateImagePointsFilter< TInputImage, TOutputImage, TCoordType, InterpolatorType >
::GenerateOutputInformation()
{
  // Call the superclass' implementation of this method
  Superclass::GenerateOutputInformation();

  // This sets the largestPossibleRegion to ensure it is the same as
  // the coordinates. While this should be the default behavor, given the
  // multiple inputs it may incorrectly default to the image
  // largestPossibleRegion.
  // Thus this method was explicitly written.
  //
  CoordImageTypePointer xCoordPtr = const_cast< TInputImage * >( this->GetInput(1) );
  OutputImagePointer    outputPtr = this->GetOutput();

  // We need to compute the output spacing, the output image size, and the
  // output image start index. This is all determined by the coordinate data

  const typename TOutputImage::SpacingType &
  outputSpacing = xCoordPtr->GetSpacing();
  const typename TOutputImage::SizeType &   outputSize =
    xCoordPtr->GetLargestPossibleRegion().GetSize();
  const typename TOutputImage::IndexType &  outputStartIndex =
    xCoordPtr->GetLargestPossibleRegion().GetIndex();

  outputPtr->SetSpacing(outputSpacing);

  typename TOutputImage::RegionType outputLargestPossibleRegion;
  outputLargestPossibleRegion.SetSize(outputSize);
  outputLargestPossibleRegion.SetIndex(outputStartIndex);

  outputPtr->SetLargestPossibleRegion(outputLargestPossibleRegion);
}

template< typename TInputImage, typename TOutputImage, typename TCoordType, typename InterpolatorType >
void
InterpolateImagePointsFilter< TInputImage, TOutputImage, TCoordType, InterpolatorType >
::PrintSelf( std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );

  os << indent << "Default (background) pixel level: " << m_DefaultPixelValue << std::endl;
}
} // namespace itk

#endif
