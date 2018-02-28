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

#ifndef itkDescoteauxEigenToMeasureImageFilter_hxx
#define itkDescoteauxEigenToMeasureImageFilter_hxx

#include "itkDescoteauxEigenToMeasureImageFilter.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageRegionIterator.h"
#include "itkProgressReporter.h"

namespace itk {
template< typename TInputImage, typename TOutputImage, typename TInputSpatialObject >
DescoteauxEigenToMeasureImageFilter< TInputImage, TOutputImage, TInputSpatialObject >
::DescoteauxEigenToMeasureImageFilter() :
  Superclass(),
  m_EnhanceType(-1.0)
{}

template< typename TInputImage, typename TOutputImage, typename TInputSpatialObject >
void
DescoteauxEigenToMeasureImageFilter< TInputImage, TOutputImage, TInputSpatialObject >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread, ThreadIdType threadId)
{
  /* Get Inputs */
  ParameterArrayType parameters = this->GetParametersInput()->Get();
  InputImageConstPointer  inputPtr = this->GetInput(0);
  OutputImagePointer outputPtr = this->GetOutput(0);
  SpatialObjectConstPointer maskPointer = this->GetMaskingSpatialObject();
  typename InputImageType::PointType point;

  /* Test parameters */
  if (parameters.GetSize() != 3)
  {
    itkExceptionMacro(<< "Parameters must have size 3. Given array of size " << parameters.GetSize());
  }

  // Define the portion of the input to walk for this thread, using
  // the CallCopyOutputRegionToInputRegion method allows for the input
  // and output images to be different dimensions
  InputImageRegionType inputRegionForThread;

  this->CallCopyOutputRegionToInputRegion(inputRegionForThread, outputRegionForThread);

  // Define the iterators
  ImageRegionConstIteratorWithIndex< TInputImage >  inputIt(inputPtr, inputRegionForThread);
  ImageRegionIterator< TOutputImage >               outputIt(outputPtr, outputRegionForThread);

  ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels() );

  inputIt.GoToBegin();
  outputIt.GoToBegin();

  while ( !inputIt.IsAtEnd() )
  {
    inputPtr->TransformIndexToPhysicalPoint(inputIt.GetIndex(), point);
    if ( (!maskPointer) ||  (maskPointer->IsInside(point)) )
    {
      outputIt.Set( ProcessPixel( inputIt.Get(), parameters[0], parameters[1], parameters[2] ) );
    }
    else
    {
      outputIt.Set( NumericTraits< OutputImagePixelType >::Zero );
    }
    ++inputIt;
    ++outputIt;
    progress.CompletedPixel();  // potential exception thrown here
  }
}

template< typename TInputImage, typename TOutputImage, typename TInputSpatialObject >
typename DescoteauxEigenToMeasureImageFilter< TInputImage, TOutputImage, TInputSpatialObject >::OutputImagePixelType
DescoteauxEigenToMeasureImageFilter< TInputImage, TOutputImage, TInputSpatialObject >
::ProcessPixel(const InputImagePixelType& pixel, RealType alpha, RealType beta, RealType c)
{
  double sheetness = 0.0;
  double a1 = static_cast<double>( pixel[0] );
  double a2 = static_cast<double>( pixel[1] );
  double a3 = static_cast<double>( pixel[2] );
  double l1 = Math::abs(a1);
  double l2 = Math::abs(a2);
  double l3 = Math::abs(a3);

  /* Deal with l3 > 0 */
  if ( m_EnhanceType * a3 < 0 ) {
      return static_cast<OutputImagePixelType>( 0.0 );
  }

  /* Avoid divisions by zero (or close to zero) */
  if ( l3 < Math::eps) {
      return static_cast<OutputImagePixelType>( 0.0 );
  }

  /* Compute measures */
  const double Rsheet = l2 / l3;
  const double Rblob = Math::abs(2*l3 - l2 - l1) / l3;
  const double Rnoise = sqrt(l1*l1 + l2*l2 + l3*l3);

  /* Multiply together to get sheetness */
  sheetness = 1.0;
  sheetness *= vcl_exp(-(Rsheet * Rsheet) / (2 * alpha * alpha));
  sheetness *= (1.0 - vcl_exp(-(Rblob * Rblob) / (2 * beta * beta)));
  sheetness *= (1.0 - vcl_exp(-(Rnoise * Rnoise) / (2 * c * c)));

  return static_cast<OutputImagePixelType>( sheetness );
}

template< typename TInputImage, typename TOutputImage, typename TInputSpatialObject >
void
DescoteauxEigenToMeasureImageFilter< TInputImage, TOutputImage, TInputSpatialObject >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Direction: " << GetEnhanceType() << std::endl;
}

} /* end namespace */

#endif /* itkDescoteauxEigenToMeasureImageFilter_hxx */
