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

#ifndef itkKrcahEigenToMeasureImageFilter_hxx
#define itkKrcahEigenToMeasureImageFilter_hxx

#include "itkKrcahEigenToMeasureImageFilter.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageRegionIterator.h"

namespace itk {
template< typename TInputImage, typename TOutputImage, typename TInputSpatialObject >
KrcahEigenToMeasureImageFilter< TInputImage, TOutputImage, TInputSpatialObject >
::KrcahEigenToMeasureImageFilter() :
  Superclass(),
  m_EnhanceType(-1.0f)
{}

template< typename TInputImage, typename TOutputImage, typename TInputSpatialObject >
void
KrcahEigenToMeasureImageFilter< TInputImage, TOutputImage, TInputSpatialObject >
::DynamicThreadedGenerateData(const OutputImageRegionType & outputRegionForThread)
{
  /* Get Inputs */
  ParameterArrayType parameters = this->GetParametersInput()->Get();
  InputImageConstPointer inputPtr = this->GetInput(0);
  OutputImagePointer outputPtr = this->GetOutput(0);
  SpatialObjectConstPointer maskPointer = this->GetMaskingSpatialObject();
  typename InputImageType::PointType point;

  /* Set parameters */
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
  }
}

template< typename TInputImage, typename TOutputImage, typename TInputSpatialObject >
typename KrcahEigenToMeasureImageFilter< TInputImage, TOutputImage, TInputSpatialObject >::OutputImagePixelType
KrcahEigenToMeasureImageFilter< TInputImage, TOutputImage, TInputSpatialObject >
::ProcessPixel(const InputImagePixelType& pixel, const RealType& alpha, const RealType& beta, const RealType& gamma)
{
    double sheetness = 0.0;
    double a1 = static_cast<double>( pixel[0] );
    double a2 = static_cast<double>( pixel[1] );
    double a3 = static_cast<double>( pixel[2] );
    double l1 = Math::abs(a1);
    double l2 = Math::abs(a2);
    double l3 = Math::abs(a3);

    /* Avoid divisions by zero (or close to zero) */
    if (static_cast<double>( l3 ) < Math::eps || static_cast<double>( l2 ) < Math::eps) {
        return static_cast<OutputImagePixelType>( sheetness );
    }

    /**
     * Compute sheet, noise, and tube like measures. Note that the average trace of the
     * Hessian matrix is implicitly included in \f$ \gamma \f$ here.
     */
    const double Rsheet = l2 / l3;
    const double Rnoise = (l1 + l2 + l3); // T implicite in m_Gamma
    const double Rtube = l1 / (l2 * l3);

    /* Multiply together to get sheetness */
    sheetness = (m_EnhanceType*a3/l3);
    sheetness *= vcl_exp(-(Rsheet * Rsheet) / (alpha * alpha));
    sheetness *= vcl_exp(-(Rtube * Rtube) / (beta * beta));
    sheetness *= (1.0 - vcl_exp(-(Rnoise * Rnoise) / (gamma * gamma)));

    return static_cast<OutputImagePixelType>( sheetness );
}

template< typename TInputImage, typename TOutputImage, typename TInputSpatialObject >
void
KrcahEigenToMeasureImageFilter< TInputImage, TOutputImage, TInputSpatialObject >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Direction: " << GetEnhanceType() << std::endl;
}

} /* end namespace */

#endif /* itkKrcahEigenToMeasureImageFilter_hxx */
