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

#include "itkImageRegionIterator.h"
#include "itkGaussianImageSource.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"


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
      (typename TInputImage::SizeType::SizeValueType) ceil(2.5*m_DomainSigma[i]);
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
::ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                       int threadId) 
{
  typename TInputImage::ConstPointer input = this->GetInput();
  typename TOutputImage::Pointer output = this->GetOutput();
  
  // Build a small image of the N-dimensional Gaussian used for domain filter
  //
  // Gaussian image size will be (2*ceil(2.5*sigma)+1) x (2*ceil(2.5*sigma)+1)
  unsigned int i;
  const double mu = 2.5;
  typename TInputImage::SizeType radius, domainKernelSize;
  
  for (i = 0; i < ImageDimension; i++)
    {
    radius[i] =
      (typename TInputImage::SizeType::SizeValueType) ceil(2.5*m_DomainSigma[i]);
    domainKernelSize[i] = 2*radius[i] + 1;
    }

  GaussianImageSource<GaussianImageType>::Pointer gaussianImage;
  GaussianImageSource<GaussianImageType>::ArrayType mean;
  GaussianImageSource<GaussianImageType>::ArrayType sigma;
  
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
  KernelType gaussianKernel;
  gaussianKernel.SetRadius(radius);
  
  KernelIteratorType kernel_it;
  ImageRegionIterator<GaussianImageType> git
    = ImageRegionIterator<GaussianImageType>(gaussianImage->GetOutput(),
                                             gaussianImage->GetOutput()
                                                     ->GetBufferedRegion() );
  for (git.GoToBegin(), kernel_it = gaussianKernel.Begin(); !git.IsAtEnd();
       ++git, ++kernel_it)
    {
    *kernel_it = git.Get();
    }

  // Now we are ready to bilateral filter!
  //
  //
  //

  // Boundary condition
  ZeroFluxNeumannBoundaryCondition<TInputImage> BC;

  // support progress methods/callbacks
  unsigned long ii = 0;
  unsigned long updateVisits = 0;
  unsigned long totalPixels = 0;
  if ( threadId == 0 )
    {
    totalPixels = outputRegionForThread.GetNumberOfPixels();
    updateVisits = totalPixels / 10;
    if( updateVisits < 1 ) updateVisits = 1;
    }

  // Find the boundary "faces"
  NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType>::FaceListType faceList;
  NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType> fC;
  faceList = fC(this->GetInput(), outputRegionForThread,
                gaussianKernel.GetRadius());

  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<InputImageType>::FaceListType::iterator fit;
  fit = faceList.begin();

  // Process the non-boundary face
  //
  NeighborhoodIteratorType n_iter
    = NeighborhoodIteratorType(gaussianKernel.GetRadius(),
                               this->GetInput(), *fit);
  ImageRegionIterator<TOutputImage> o_iter(this->GetOutput(), *fit);

  KernelIteratorType k_it;
  const KernelConstIteratorType kernelEnd = gaussianKernel.End();
  
  OutputPixelRealType centerPixel;
  OutputPixelRealType val, normFactor, rangeGaussian, rangeDistanceSq;
  double rangeGaussianDenom;

  
  // denominator (normalization factor) for Gaussian used for range
  rangeGaussianDenom = sqrt(2.0*3.1415927*m_RangeSigma);

  // walk the interior face and the corresponding section of the output
  n_iter.GoToBegin();
  o_iter.GoToBegin();

  while ( ! n_iter.IsAtEnd() )
    {
    if ( threadId == 0 && !(++ii % updateVisits ) )
      {
      this->UpdateProgress((float)ii / (float)totalPixels);
      }

    // Setup
    centerPixel = static_cast<OutputPixelRealType>(n_iter.GetCenterPixel());
    val = 0.0;
    normFactor = 0.0;
    
    // Walk the neighborhood of the input pixel and the kernel
    for (i=0, k_it = gaussianKernel.Begin(); k_it < kernelEnd;
         ++k_it, ++i)
      {
      // distance squared between neighborhood pixel and neighborhood center
      rangeDistanceSq
        = (static_cast<OutputPixelRealType>(n_iter.GetPixel(i)) - centerPixel)
        * (static_cast<OutputPixelRealType>(n_iter.GetPixel(i)) - centerPixel);
        
      // range Gaussian value
      rangeGaussian = exp(-0.5*rangeDistanceSq / (m_RangeSigma * m_RangeSigma))
        / rangeGaussianDenom;

      // normalization factor so filter integrates to one
      normFactor += (*k_it) * rangeGaussian;
      
      // Input Image * Domain Gaussian * Range Gaussian 
      val += n_iter.GetPixel(i) * (*k_it) * rangeGaussian;
      }
    // normalize the final value
    val /= normFactor;

    // store the filtered value
    o_iter.Set( static_cast<OutputPixelType>(val) );
    
    ++n_iter ;
    ++o_iter ;
    }
 
  // Process the boundary faces, these are N-d regions which border the
  // edge of the buffer
  SmartNeighborhoodIteratorType b_iter;
  for (++fit; fit != faceList.end(); ++fit)
    { 
    // walk the boundary face and the corresponding section of the output
    b_iter = SmartNeighborhoodIteratorType(gaussianKernel.GetRadius(),
                                           this->GetInput(), *fit);
    b_iter.OverrideBoundaryCondition(&BC);
    b_iter.GoToBegin();
    o_iter = ImageRegionIterator<OutputImageType>(this->GetOutput(), *fit);
    
    while ( ! b_iter.IsAtEnd() )
      {
      if ( threadId == 0 && !(++ii % updateVisits ) )
        {
        this->UpdateProgress((float)ii / (float)totalPixels);
        }
    
      // Setup
      centerPixel = b_iter.GetCenterPixel();
      val = 0.0;
      normFactor = 0.0;
    
      // Walk the neighborhood of the input and the kernel
      for (i=0, k_it = gaussianKernel.Begin(); k_it < kernelEnd;
           ++k_it, ++i)
        {
        // distance squared between neighborhood pixel and neighborhood center
        rangeDistanceSq
          = (static_cast<OutputPixelRealType>(n_iter.GetPixel(i))-centerPixel)
          * (static_cast<OutputPixelRealType>(n_iter.GetPixel(i))-centerPixel);

        // range Gaussian value
        rangeGaussian = exp(-0.5*rangeDistanceSq / (m_RangeSigma*m_RangeSigma))
          / rangeGaussianDenom;

        // normalization factor so filter integrates to one
        normFactor += (*k_it) * rangeGaussian;
      
        // Input Image * Domain Gaussian * Range Gaussian 
        val += n_iter.GetPixel(i) * (*k_it) * rangeGaussian;
        }
      // normalize the value
      val /= normFactor;

      // store the filtered value
      o_iter.Set( static_cast<OutputPixelType>(val) );

      ++b_iter;
      ++o_iter;
      }
    }
}


template< class TInputImage, class TOutputImage >
void
BilateralImageFilter<TInputImage, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
  {
  Superclass::PrintSelf(os,indent);

  os << indent << "DomainSigma: " << m_DomainSigma << std::endl;
  os << indent << "RangeSigma: " << m_RangeSigma << std::endl;
  os << indent << "FilterDimensionality: " << m_FilterDimensionality << std::endl;
}

} // end namespace itk

#endif
