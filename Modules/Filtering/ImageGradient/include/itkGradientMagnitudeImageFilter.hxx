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
#ifndef itkGradientMagnitudeImageFilter_hxx
#define itkGradientMagnitudeImageFilter_hxx
#include "itkGradientMagnitudeImageFilter.h"

#include "itkConstNeighborhoodIterator.h"
#include "itkNeighborhoodInnerProduct.h"
#include "itkImageRegionIterator.h"
#include "itkDerivativeOperator.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkProgressReporter.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage >
void
GradientMagnitudeImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "UseImageSpacing = " << m_UseImageSpacing << std::endl;
}

template< typename TInputImage, typename TOutputImage >
void
GradientMagnitudeImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();

  // get pointers to the input and output
  InputImagePointer inputPtr =
    const_cast< InputImageType * >( this->GetInput() );
  OutputImagePointer outputPtr = this->GetOutput();

  if ( !inputPtr || !outputPtr )
    {
    return;
    }

  // Build an operator so that we can determine the kernel size
  DerivativeOperator< RealType, ImageDimension > oper;
  oper.SetDirection(0);
  oper.SetOrder(1);
  oper.CreateDirectional();
  SizeValueType radius = oper.GetRadius()[0];

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
GradientMagnitudeImageFilter< TInputImage, TOutputImage >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  unsigned int i;

  ZeroFluxNeumannBoundaryCondition< TInputImage > nbc;

  ConstNeighborhoodIterator< TInputImage > nit;
  ConstNeighborhoodIterator< TInputImage > bit;
  ImageRegionIterator< TOutputImage >      it;

  NeighborhoodInnerProduct< TInputImage, RealType > SIP;

  // Allocate output
  typename OutputImageType::Pointer output = this->GetOutput();
  typename  InputImageType::ConstPointer input  = this->GetInput();

  // Set up operators
  DerivativeOperator< RealType, ImageDimension > op[ImageDimension];

  for ( i = 0; i < ImageDimension; i++ )
    {
    op[i].SetDirection(0);
    op[i].SetOrder(1);
    op[i].CreateDirectional();

    if ( m_UseImageSpacing == true )
      {
      if ( this->GetInput()->GetSpacing()[i] == 0.0 )
        {
        itkExceptionMacro(<< "Image spacing cannot be zero.");
        }
      else
        {
        op[i].ScaleCoefficients(1.0 / this->GetInput()->GetSpacing()[i]);
        }
      }
    }

  // Calculate iterator radius
  Size< ImageDimension > radius;
  for ( i = 0; i < ImageDimension; ++i )
    {
    radius[i]  = op[0].GetRadius()[0];
    }

  // Find the data-set boundary "faces"
  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< TInputImage >::
  FaceListType faceList;
  NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< TInputImage > bC;
  faceList = bC(input, outputRegionForThread, radius);

  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< TInputImage >::
  FaceListType::iterator fit;
  fit = faceList.begin();

  // support progress methods/callbacks
  ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels() );

  // Process non-boundary face
  nit = ConstNeighborhoodIterator< TInputImage >(radius, input, *fit);

  std::slice          x_slice[ImageDimension];
  const SizeValueType center = nit.Size() / 2;
  for ( i = 0; i < ImageDimension; ++i )
    {
    x_slice[i] = std::slice( center - nit.GetStride(i) * radius[i],
                             op[i].GetSize()[0], nit.GetStride(i) );
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

    while ( !bit.IsAtEnd() )
      {
      RealType a = NumericTraits< RealType >::ZeroValue();
      for ( i = 0; i < ImageDimension; ++i )
        {
        const RealType g = SIP(x_slice[i], bit, op[i]);
        a += g * g;
        }
      it.Value() = static_cast< OutputPixelType >( std::sqrt(a) );
      ++bit;
      ++it;
      progress.CompletedPixel();
      }
    }
}
} // end namespace itk

#endif
