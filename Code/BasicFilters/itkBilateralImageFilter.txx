/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBilateralImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkBilateralImageFilter_txx
#define _itkBilateralImageFilter_txx

#include "itkBilateralImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkGaussianImageSource.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"
#include "itkProgressReporter.h"



namespace itk
{
template <class TInputImage, class TOutputImage>
void 
BilateralImageFilter<TInputImage,TOutputImage>
::GenerateInputRequestedRegion() throw(InvalidRequestedRegionError)
{
  // call the superclass' implementation of this method. this should
  // copy the output requested region to the input requested region
  Superclass::GenerateInputRequestedRegion();
  
  // get pointers to the input and output
  typename Superclass::InputImagePointer  inputPtr = 
    const_cast< TInputImage *>( this->GetInput() );
  
  if ( !inputPtr )
    {
    return;
    }

  // Pad the image by 2.5*sigma in all directions
  const double mu = 2.5;
  typename TInputImage::SizeType radius;
  
  for (unsigned int i = 0; i < TInputImage::ImageDimension; i++)
    {
    radius[i] =
      (typename TInputImage::SizeType::SizeValueType)
      ceil(mu*m_DomainSigma[i]/this->GetInput()->GetSpacing()[i]);
    }

  // get a copy of the input requested region (should equal the output
  // requested region)
  typename TInputImage::RegionType inputRequestedRegion;
  inputRequestedRegion = inputPtr->GetRequestedRegion();

  // pad the input requested region by the operator radius
  inputRequestedRegion.PadByRadius( radius );

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
    OStringStream msg;
    msg << (char *)this->GetNameOfClass()
        << "::GenerateInputRequestedRegion()";
    e.SetLocation(msg.str().c_str());
    e.SetDescription("Requested region is (at least partially) outside the largest possible region.");
    e.SetDataObject(inputPtr);
    throw e;
    }
}

template< class TInputImage, class TOutputImage >
void
BilateralImageFilter<TInputImage, TOutputImage>
::BeforeThreadedGenerateData()
{
  // Build a small image of the N-dimensional Gaussian used for domain filter
  //
  // Gaussian image size will be (2*ceil(2.5*sigma)+1) x (2*ceil(2.5*sigma)+1)
  unsigned int i;
  const double mu = 2.5;
  typename TInputImage::SizeType radius, domainKernelSize;

  for (i = 0; i < ImageDimension; i++)
    {
    radius[i] =
      (typename TInputImage::SizeType::SizeValueType)
      ceil(mu*m_DomainSigma[i]/this->GetInput()->GetSpacing()[i]);
    domainKernelSize[i] = 2*radius[i] + 1;
    }

  typename GaussianImageSource<GaussianImageType>::Pointer gaussianImage;
  typename GaussianImageSource<GaussianImageType>::ArrayType mean;
  typename GaussianImageSource<GaussianImageType>::ArrayType sigma;
  
  gaussianImage = GaussianImageSource<GaussianImageType>::New();
  gaussianImage->SetSize(domainKernelSize.GetSize());
  gaussianImage->SetSpacing( this->GetInput()->GetSpacing() );
  gaussianImage->SetOrigin( this->GetInput()->GetOrigin() );
  gaussianImage->SetScale( 1.0 );
  gaussianImage->SetNormalized( true );

  for (i=0; i < ImageDimension; i++)
    {
    mean[i] = this->GetInput()->GetSpacing()[i]*radius[i]; // center pixel pos
    sigma[i] = m_DomainSigma[i];
    }
  gaussianImage->SetSigma( sigma );
  gaussianImage->SetMean( mean );

  gaussianImage->Update();

  // copy this small Gaussian image into a neighborhood
  m_GaussianKernel.SetRadius(radius);

  KernelIteratorType kernel_it;
  ImageRegionIterator<GaussianImageType> git
    = ImageRegionIterator<GaussianImageType>(gaussianImage->GetOutput(),
                                             gaussianImage->GetOutput()
                                                     ->GetBufferedRegion() );
  for (git.GoToBegin(), kernel_it = m_GaussianKernel.Begin(); !git.IsAtEnd();
       ++git, ++kernel_it)
    {
    *kernel_it = git.Get();
    }
}

  
template< class TInputImage, class TOutputImage >
void
BilateralImageFilter<TInputImage, TOutputImage>
::ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                       int threadId) 
{
  typename TInputImage::ConstPointer input = this->GetInput();
  typename TOutputImage::Pointer output = this->GetOutput();
  unsigned long i;

  // Now we are ready to bilateral filter!
  //
  //
  //

  // Boundary condition
  ZeroFluxNeumannBoundaryCondition<TInputImage> BC;

  // Find the boundary "faces"
  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType>::FaceListType faceList;
  NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType> fC;
  faceList = fC(this->GetInput(), outputRegionForThread,
                m_GaussianKernel.GetRadius());

  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType>::FaceListType::iterator fit;

  OutputPixelRealType centerPixel;
  OutputPixelRealType val, normFactor, rangeGaussian, rangeDistanceSq,
    rangeDistance, pixel, gaussianProduct;
  double rangeGaussianDenom;
  double rangeVariance = m_RangeSigma * m_RangeSigma;

  // denominator (normalization factor) for Gaussian used for range
  rangeGaussianDenom = sqrt(2.0*3.1415927*m_RangeSigma);

  // Process all the faces, the NeighborhoodIterator will deteremine
  // whether a specified region needs to use the boundary conditions or
  // not.
  NeighborhoodIteratorType b_iter;
  ImageRegionIterator<OutputImageType> o_iter;
  KernelConstIteratorType k_it;
  KernelConstIteratorType kernelEnd = m_GaussianKernel.End();

  ProgressReporter progress(this, threadId, outputRegionForThread.GetNumberOfPixels());

  for (fit = faceList.begin(); fit != faceList.end(); ++fit)
    { 
    // walk the boundary face and the corresponding section of the output
    b_iter = NeighborhoodIteratorType(m_GaussianKernel.GetRadius(),
                                           this->GetInput(), *fit);
    b_iter.OverrideBoundaryCondition(&BC);
    o_iter = ImageRegionIterator<OutputImageType>(this->GetOutput(), *fit);
    
    while ( ! b_iter.IsAtEnd() )
      {
      // Setup
      centerPixel = b_iter.GetCenterPixel();
      val = 0.0;
      normFactor = 0.0;
    
      // Walk the neighborhood of the input and the kernel
      for (i=0, k_it = m_GaussianKernel.Begin(); k_it < kernelEnd;
           ++k_it, ++i)
        {
        // distance squared between neighborhood pixel and neighborhood center
        pixel = static_cast<OutputPixelRealType>(b_iter.GetPixel(i));
        rangeDistance = pixel - centerPixel;
        rangeDistanceSq = rangeDistance*rangeDistance;

        // range Gaussian value
        rangeGaussian = exp(-0.5*rangeDistanceSq / rangeVariance)
          / rangeGaussianDenom;

        // normalization factor so filter integrates to one
        // (product of the domain and the range gaussian)
        gaussianProduct = (*k_it) * rangeGaussian;
        normFactor += gaussianProduct;
      
        // Input Image * Domain Gaussian * Range Gaussian 
        val += pixel * gaussianProduct;
        }
      // normalize the value
      val /= normFactor;

      // store the filtered value
      o_iter.Set( static_cast<OutputPixelType>(val) );

      ++b_iter;
      ++o_iter;
      progress.CompletedPixel();
      }
    }
}


template< class TInputImage, class TOutputImage >
void
BilateralImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
  {
  Superclass::PrintSelf(os,indent);

  os << indent << "DomainSigma: ";
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    os << m_DomainSigma[i] << " ";
    }
  os << std::endl;
  os << indent << "RangeSigma: " << m_RangeSigma << std::endl;
  os << indent << "FilterDimensionality: " << m_FilterDimensionality << std::endl;
}

} // end namespace itk

#endif
