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
#ifndef itkDisplacementFieldJacobianDeterminantFilter_hxx
#define itkDisplacementFieldJacobianDeterminantFilter_hxx

#include "itkDisplacementFieldJacobianDeterminantFilter.h"

#include "itkNeighborhoodAlgorithm.h"
#include "itkImageRegionIterator.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"
#include "itkProgressReporter.h"
#include "itkVectorCastImageFilter.h"

#include "itkMath.h"
#include "itkMath.h"

namespace itk
{
template< typename TInputImage, typename TRealType, typename TOutputImage >
DisplacementFieldJacobianDeterminantFilter< TInputImage, TRealType, TOutputImage >
::DisplacementFieldJacobianDeterminantFilter()
{
  m_UseImageSpacing = true;
  m_RequestedNumberOfThreads = this->GetNumberOfThreads();
  m_NeighborhoodRadius.Fill(1);
  m_DerivativeWeights.Fill(1.0);
  m_HalfDerivativeWeights.Fill(0.5);
}

template< typename TInputImage, typename TRealType, typename TOutputImage >
void
DisplacementFieldJacobianDeterminantFilter< TInputImage, TRealType, TOutputImage >
::SetDerivativeWeights(const WeightsType & data)
{
  //If the user provides their own derivative
  //weights, then it is assumed that the
  //user is accommodating image spacing
  //internal to their weight settings.
  m_UseImageSpacing = false;

  for ( unsigned int i = 0; i < ImageDimension; ++i )
    {
    if ( Math::NotExactlyEquals(m_DerivativeWeights[i], data[i]) )
      {
      this->Modified();
      m_DerivativeWeights[i] = data[i];
      m_HalfDerivativeWeights[i] = 0.5 * data[i];
      }
    }
}

template< typename TInputImage, typename TRealType, typename TOutputImage >
void
DisplacementFieldJacobianDeterminantFilter< TInputImage, TRealType, TOutputImage >
::SetUseImageSpacing(bool f)
{
  if ( m_UseImageSpacing == f )
    {
    return;
    }

  // Only reset the weights if they were previously set to the image spacing,
  // otherwise, the user may have provided their own weightings.
  if ( f == false && m_UseImageSpacing == true )
    {
    for ( unsigned int i = 0; i < ImageDimension; ++i )
      {
      m_DerivativeWeights[i] = static_cast< TRealType >( 1.0 );
      m_HalfDerivativeWeights[i] = static_cast< TRealType >( 0.5 );
      }
    }

  m_UseImageSpacing = f;
  this->Modified();
}

template< typename TInputImage, typename TRealType, typename TOutputImage >
void
DisplacementFieldJacobianDeterminantFilter< TInputImage, TRealType, TOutputImage >
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

  // get a copy of the input requested region (should equal the output
  // requested region)
  typename TInputImage::RegionType inputRequestedRegion;
  inputRequestedRegion = inputPtr->GetRequestedRegion();

  // pad the input requested region by the operator radius
  inputRequestedRegion.PadByRadius(m_NeighborhoodRadius);

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

template< typename TInputImage, typename TRealType, typename TOutputImage >
void
DisplacementFieldJacobianDeterminantFilter< TInputImage, TRealType, TOutputImage >
::BeforeThreadedGenerateData()
{
  Superclass::BeforeThreadedGenerateData();

  // Set the weights on the derivatives.
  // Are we using image spacing in the calculations?  If so we must update now
  // in case our input image has changed.
  if ( m_UseImageSpacing == true )
    {
    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      if ( static_cast< TRealType >( this->GetInput()->GetSpacing()[i] ) == 0.0 )
        {
        itkExceptionMacro(<< "Image spacing in dimension " << i << " is zero.");
        }
      m_DerivativeWeights[i] =
        static_cast< TRealType >( 1.0
                                  / static_cast< TRealType >( this->GetInput()->GetSpacing()[i] ) );
      m_HalfDerivativeWeights[i] = 0.5 * m_DerivativeWeights[i];
      }
    }
  //
  // cast might not be necessary, but CastImagefilter is optimized for
  // the case where the InputImageType == OutputImageType
  typename VectorCastImageFilter< TInputImage, RealVectorImageType >::Pointer
    caster = VectorCastImageFilter< TInputImage, RealVectorImageType >::New();
  caster->SetInput( this->GetInput() );
  caster->Update();
  m_RealValuedInputImage = caster->GetOutput();
}

template< typename TInputImage, typename TRealType, typename TOutputImage >
void
DisplacementFieldJacobianDeterminantFilter< TInputImage, TRealType, TOutputImage >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  ZeroFluxNeumannBoundaryCondition< RealVectorImageType > nbc;
  ConstNeighborhoodIteratorType                           bit;
  ImageRegionIterator< TOutputImage >                     it;

  // Find the data-set boundary "faces"
  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< RealVectorImageType >::
  FaceListType faceList;
  NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< RealVectorImageType > bC;
  faceList = bC(dynamic_cast< const RealVectorImageType * >( m_RealValuedInputImage.GetPointer() ),
                outputRegionForThread, m_NeighborhoodRadius);

  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< RealVectorImageType >::
  FaceListType::iterator fit;
  fit = faceList.begin();

  // Support progress methods/callbacks
  ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels() );

  // Process each of the data set faces.  The iterator is reinitialized on each
  // face so that it can determine whether or not to check for boundary
  // conditions.
  for ( fit = faceList.begin(); fit != faceList.end(); ++fit )
    {
    bit = ConstNeighborhoodIteratorType(m_NeighborhoodRadius,
                                        dynamic_cast< const RealVectorImageType * >( m_RealValuedInputImage.GetPointer() ),
                                        *fit);
    it = ImageRegionIterator< TOutputImage >(this->GetOutput(), *fit);
    bit.OverrideBoundaryCondition(&nbc);
    bit.GoToBegin();

    while ( !bit.IsAtEnd() )
      {
      it.Set( static_cast< OutputPixelType >( this->EvaluateAtNeighborhood(bit) ) );
      ++bit;
      ++it;
      progress.CompletedPixel();
      }
    }
}

template< typename TInputImage, typename TRealType, typename TOutputImage >
TRealType
DisplacementFieldJacobianDeterminantFilter< TInputImage, TRealType, TOutputImage >
::EvaluateAtNeighborhood(const ConstNeighborhoodIteratorType & it) const
{
  vnl_matrix_fixed< TRealType, ImageDimension, VectorDimension > J;
  for ( unsigned int i = 0; i < ImageDimension; ++i )
    {
    for ( unsigned int j = 0; j < VectorDimension; ++j )
      {
      J[i][j] = m_HalfDerivativeWeights[i] * ( it.GetNext(i)[j] - it.GetPrevious(i)[j] );
      }
    // add one on the diagonal to consider the warping and not only the
    // deformation field
    J[i][i] += 1.0;
    }
  return vnl_det(J);
}

template< typename TInputImage, typename TRealType, typename TOutputImage >
void
DisplacementFieldJacobianDeterminantFilter< TInputImage, TRealType, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  unsigned int i;

  Superclass::PrintSelf(os, indent);
  os << indent << "m_UseImageSpacing = "          << m_UseImageSpacing
     << std::endl;
  os << indent << "m_RequestedNumberOfThreads = " << m_RequestedNumberOfThreads
     << std::endl;
  os << indent << "m_DerivativeWeights = ";
  for ( i = 0; i < ImageDimension; i++ )
    {
    os << m_DerivativeWeights[i] << " ";
    }
  os << std::endl;
  os << indent << "m_HalfDerivativeWeights = ";
  for ( i = 0; i < ImageDimension; i++ )
    {
    os << m_HalfDerivativeWeights[i] << " ";
    }
  os << std::endl;
  os << indent << "m_NeighborhoodRadius = "          << m_NeighborhoodRadius
     << std::endl;
  os << indent << "m_RealValuedInputImage = "          << m_RealValuedInputImage.GetPointer()
     << std::endl;
}
} // end namespace itk

#endif
