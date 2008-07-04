/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWarpJacobianDeterminantFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkWarpJacobianDeterminantFilter_txx
#define __itkWarpJacobianDeterminantFilter_txx

#include "itkWarpJacobianDeterminantFilter.h"

#include "itkNeighborhoodAlgorithm.h"
#include "itkImageRegionIterator.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"
#include "itkProgressReporter.h"

#include "vnl/vnl_matrix.h"
#include "vnl/vnl_det.h"
#include "vnl/vnl_math.h"
#include "vnl/algo/vnl_qr.h"

#include <vnl/algo/vnl_determinant.h>

namespace itk
{

template <typename TInputImage, typename TOutputImage>
WarpJacobianDeterminantFilter<TInputImage, TOutputImage>
::WarpJacobianDeterminantFilter()
{
  unsigned int i;
  m_UseImageSpacing = true;
  m_RequestedNumberOfThreads = this->GetNumberOfThreads();
  for (i = 0; i < ImageDimension; i++)
    {
    m_NeighborhoodRadius[i] = 1; // radius of neighborhood we will use
    m_DerivativeWeights[i] = 1.0;
    }
}
template <typename TInputImage, typename TOutputImage>
void
WarpJacobianDeterminantFilter<TInputImage, TOutputImage>
::SetDerivativeWeights(double data[])
{
  m_UseImageSpacing = false;

  for (unsigned int i = 0; i < ImageDimension; ++i)
    {
    if (m_DerivativeWeights[i] != data[i])
      {
      this->Modified();
      m_DerivativeWeights[i] = data[i];
      }
    }
}

template <typename TInputImage, typename TOutputImage>
void 
WarpJacobianDeterminantFilter<TInputImage, TOutputImage>
::SetUseImageSpacing(bool f)
{
  if (m_UseImageSpacing == f)
    {
    return;
    }

  // Only reset the weights if they were previously set to the image spacing,
  // otherwise, the user may have provided their own weightings.
  if (f == false && m_UseImageSpacing == true)
    {
    for (unsigned int i = 0; i < ImageDimension; ++i)
      {
      m_DerivativeWeights[i] = 1.0;
      }
    }

  m_UseImageSpacing = f;
}
  
template <typename TInputImage, typename TOutputImage>
void 
WarpJacobianDeterminantFilter<TInputImage, TOutputImage>
::GenerateInputRequestedRegion() throw(InvalidRequestedRegionError)
{
  // call the superclass' implementation of this method
  Superclass::GenerateInputRequestedRegion();
  
  // get pointers to the input and output
  InputImagePointer  inputPtr = 
    const_cast< InputImageType * >( this->GetInput());
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
  inputRequestedRegion.PadByRadius( m_NeighborhoodRadius );

  // crop the input requested region at the input's largest possible region
  if ( inputRequestedRegion.Crop(inputPtr->GetLargestPossibleRegion()) )
    {
    inputPtr->SetRequestedRegion( inputRequestedRegion );
    return;
    }
  else
    {
    // Couldn't crop the region (requested region is outside the largest
    // possible region).  Throw an exception.

    // store what we tried to request (prior to trying to crop)
    inputPtr->SetRequestedRegion( inputRequestedRegion );
    
    // build an exception
    InvalidRequestedRegionError e(__FILE__, __LINE__);
    e.SetLocation(ITK_LOCATION);
    e.SetDescription("Requested region is (at least partially) outside the largest possible region.");
    e.SetDataObject(inputPtr);
    throw e;
    }
}

template<typename TInputImage, typename TOutputImage>
void
WarpJacobianDeterminantFilter<TInputImage, TOutputImage>
::BeforeThreadedGenerateData()
{
  Superclass::BeforeThreadedGenerateData();

  // Set the weights on the derivatives.
  // Are we using image spacing in the calculations?  If so we must update now
  // in case our input image has changed.
  if (m_UseImageSpacing == true)
    {

    for (unsigned int i = 0; i < ImageDimension; i++)
      {
      if (this->GetInput()->GetSpacing()[i] <= 0.0)
        {
        itkExceptionMacro(<< "Image spacing in dimension " << i << " is zero.");
        }
      m_DerivativeWeights[i] = 1.0 / static_cast<double>(this->GetInput()->GetSpacing()[i]);
      }
    }  
}

template<typename TInputImage, typename TOutputImage>
void
WarpJacobianDeterminantFilter<TInputImage, TOutputImage>
::ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                       int threadId)
{

  ZeroFluxNeumannBoundaryCondition<InputImageType> nbc;
  ConstNeighborhoodIteratorType bit;
  ImageRegionIterator<TOutputImage> it;
  
  // Find the data-set boundary "faces"
  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType>::
    FaceListType faceList;
  NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType> bC;
  faceList = bC(this->GetInput(), outputRegionForThread, m_NeighborhoodRadius);

  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType>::
    FaceListType::iterator fit;
  fit = faceList.begin();

  // Support progress methods/callbacks
  ProgressReporter progress(this, threadId, outputRegionForThread.GetNumberOfPixels());

  // Process each of the data set faces.  The iterator is reinitialized on each
  // face so that it can determine whether or not to check for boundary
  // conditions.
  for (fit=faceList.begin(); fit != faceList.end(); ++fit)
    { 
    bit = ConstNeighborhoodIteratorType(m_NeighborhoodRadius,
                                        this->GetInput(),
                                        *fit);
    it = ImageRegionIterator<TOutputImage>(this->GetOutput(), *fit);
    bit.OverrideBoundaryCondition(&nbc);
    bit.GoToBegin();

    while ( ! bit.IsAtEnd() )
      {
      it.Set( this->EvaluateAtNeighborhood(bit) );
      ++bit;
      ++it;
      progress.CompletedPixel();
      }
    }
}


template <typename TInputImage, typename TOutputImage>
typename WarpJacobianDeterminantFilter<TInputImage, TOutputImage>::OutputPixelType
WarpJacobianDeterminantFilter<TInputImage, TOutputImage>
::EvaluateAtNeighborhood(ConstNeighborhoodIteratorType &it) const
{
  // Simple method using field derivatives
  
  unsigned int i, j;
  vnl_matrix_fixed<double,ImageDimension,VectorDimension> J;
  
  //const InputPixelType centerpix = it.GetCenterPixel();
  InputPixelType next, prev;
  
  double weight;
  
  for (i = 0; i < ImageDimension; ++i)
    {
    next = it.GetNext(i);
    prev = it.GetPrevious(i);
    
    weight = 0.5*m_DerivativeWeights[i];
    
    for (j = 0; j < VectorDimension; ++j)
      {
      J[i][j] = weight * (  static_cast<double>( next[j] )
                           -static_cast<double>( prev[j] ) );
      }
    
    // add one on the diagonal to consider the warp
    // and not only the deformation field
    J[i][i] += 1.0;
    }
  
  return static_cast<OutputPixelType>(vnl_det(J));
}


template <typename TInputImage, typename TOutputImage>
void
WarpJacobianDeterminantFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  unsigned int i;
  Superclass::PrintSelf(os,indent);
  os << indent << "m_UseImageSpacing =    "
     << m_UseImageSpacing<< std::endl;
  os << indent << "m_RequestedNumberOfThreads = "
     << m_RequestedNumberOfThreads<< std::endl;
  os << indent << "m_DerivativeWeights =  ";
  for (i = 0; i < ImageDimension; i++)
    { os << m_DerivativeWeights[i] << " "; }
  os << std::endl;
  os << indent << "m_NeighborhoodRadius = "
     << m_NeighborhoodRadius<< std::endl;
}
  
} // end namespace itk

#endif
