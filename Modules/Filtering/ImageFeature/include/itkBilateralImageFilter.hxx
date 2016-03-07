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
#ifndef itkBilateralImageFilter_hxx
#define itkBilateralImageFilter_hxx

#include "itkBilateralImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkGaussianImageSource.h"
#include "itkNeighborhoodAlgorithm.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"
#include "itkProgressReporter.h"
#include "itkStatisticsImageFilter.h"

namespace itk
{
template< typename TInputImage, typename TOutputImage >
BilateralImageFilter< TInputImage, TOutputImage >
::BilateralImageFilter()
{
  this->m_Radius.Fill(1);
  this->m_AutomaticKernelSize = true;
  this->m_DomainSigma.Fill(4.0);
  this->m_RangeSigma = 50.0;
  this->m_FilterDimensionality = ImageDimension;
  this->m_NumberOfRangeGaussianSamples = 100;
  this->m_DynamicRange = 0.0;
  this->m_DynamicRangeUsed = 0.0;
  this->m_DomainMu = 2.5;  // keep small to keep kernels small
  this->m_RangeMu = 4.0;   // can be bigger then DomainMu since we only
                           // index into a single table
}

template< typename TInputImage, typename TOutputImage >
void
BilateralImageFilter< TInputImage, TOutputImage >
::SetRadius(const SizeValueType i)
{
  m_Radius.Fill(i);
}

template< typename TInputImage, typename TOutputImage >
void
BilateralImageFilter< TInputImage, TOutputImage >
::GenerateInputRequestedRegion()
{
  // call the superclass' implementation of this method. this should
  // copy the output requested region to the input requested region
  Superclass::GenerateInputRequestedRegion();

  // get pointers to the input and output
  typename Superclass::InputImagePointer inputPtr =
    const_cast< TInputImage * >( this->GetInput() );

  if ( !inputPtr )
    {
    return;
    }

  // Pad the image by 2.5*sigma in all directions
  typename TInputImage::SizeType radius;
  unsigned int i;

  if ( m_AutomaticKernelSize )
    {
    for ( i = 0; i < ImageDimension; i++ )
      {
      radius[i] =
        ( typename TInputImage::SizeType::SizeValueType )
        std::ceil(m_DomainMu * m_DomainSigma[i] / this->GetInput()->GetSpacing()[i]);
      }
    }
  else
    {
    for ( i = 0; i < ImageDimension; i++ )
      {
      radius[i] = m_Radius[i];
      }
    }

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
BilateralImageFilter< TInputImage, TOutputImage >
::BeforeThreadedGenerateData()
{
  // Build a small image of the N-dimensional Gaussian used for domain filter
  //
  // Gaussian image size will be (2*std::ceil(2.5*sigma)+1) x
  // (2*std::ceil(2.5*sigma)+1)
  unsigned int i;

  typename InputImageType::SizeType radius;
  typename InputImageType::SizeType domainKernelSize;

  const InputImageType *inputImage = this->GetInput();

  const typename InputImageType::SpacingType inputSpacing = inputImage->GetSpacing();
  const typename InputImageType::PointType inputOrigin  = inputImage->GetOrigin();

  if ( m_AutomaticKernelSize )
    {
    for ( i = 0; i < ImageDimension; i++ )
      {
      radius[i] =
        ( typename TInputImage::SizeType::SizeValueType )
        std::ceil(m_DomainMu * m_DomainSigma[i] / inputSpacing[i]);
      domainKernelSize[i] = 2 * radius[i] + 1;
      }
    }
  else
    {
    for ( i = 0; i < ImageDimension; i++ )
      {
      radius[i] = m_Radius[i];
      domainKernelSize[i] = 2 * radius[i] + 1;
      }
    }

  typename GaussianImageSource< GaussianImageType >::Pointer gaussianImage;
  typename GaussianImageSource< GaussianImageType >::ArrayType mean;
  typename GaussianImageSource< GaussianImageType >::ArrayType sigma;

  gaussianImage = GaussianImageSource< GaussianImageType >::New();
  gaussianImage->SetSize( domainKernelSize );
  gaussianImage->SetSpacing(inputSpacing);
  gaussianImage->SetOrigin(inputOrigin);
  gaussianImage->SetScale(1.0);
  gaussianImage->SetNormalized(true);

  for ( i = 0; i < ImageDimension; i++ )
    {
    mean[i] = inputSpacing[i] * radius[i] + inputOrigin[i]; // center pixel pos
    sigma[i] = m_DomainSigma[i];
    }
  gaussianImage->SetSigma(sigma);
  gaussianImage->SetMean(mean);

  gaussianImage->Update();

  // copy this small Gaussian image into a neighborhood
  m_GaussianKernel.SetRadius(radius);

  KernelIteratorType                       kernel_it;
  ImageRegionIterator< GaussianImageType > git =
    ImageRegionIterator< GaussianImageType >( gaussianImage->GetOutput(),
                                              gaussianImage->GetOutput()
                                              ->GetBufferedRegion() );
  double norm = 0.0;
  for ( git.GoToBegin(); !git.IsAtEnd(); ++git )
    {
    norm += git.Get();
    }
  for ( git.GoToBegin(), kernel_it = m_GaussianKernel.Begin(); !git.IsAtEnd();
        ++git, ++kernel_it )
    {
    *kernel_it = git.Get() / norm;
    }

  // Build a lookup table for the range gaussian
  //
  //

  // First, determine the min and max intensity range
  typename StatisticsImageFilter< TInputImage >::Pointer statistics =
    StatisticsImageFilter< TInputImage >::New();

  statistics->SetInput(inputImage);
  statistics->GetOutput()
  ->SetRequestedRegion( this->GetOutput()->GetRequestedRegion() );
  statistics->Update();

  // Now create the lookup table whose domain runs from 0.0 to
  // (max-min) and range is gaussian evaluated at
  // that point
  //
  double rangeVariance = m_RangeSigma * m_RangeSigma;

  // denominator (normalization factor) for Gaussian used for range
  double rangeGaussianDenom;
  rangeGaussianDenom = m_RangeSigma * std::sqrt(2.0 * itk::Math::pi);

  // Maximum delta for the dynamic range
  double tableDelta;
  double v;

  m_DynamicRange = ( static_cast< double >( statistics->GetMaximum() )
                     - static_cast< double >( statistics->GetMinimum() ) );

  m_DynamicRangeUsed = m_RangeMu * m_RangeSigma;

  tableDelta = m_DynamicRangeUsed
               / static_cast< double >( m_NumberOfRangeGaussianSamples );

  // Finally, build the table
  m_RangeGaussianTable.resize(m_NumberOfRangeGaussianSamples);
  for ( i = 0, v = 0.0; i < m_NumberOfRangeGaussianSamples;
        ++i, v += tableDelta )
    {
    m_RangeGaussianTable[i] = std::exp(-0.5 * v * v / rangeVariance) / rangeGaussianDenom;
    }
}

template< typename TInputImage, typename TOutputImage >
void
BilateralImageFilter< TInputImage, TOutputImage >
::ThreadedGenerateData(const OutputImageRegionType & outputRegionForThread,
                       ThreadIdType threadId)
{
  typename TInputImage::ConstPointer input = this->GetInput();
  typename TOutputImage::Pointer output = this->GetOutput();
  typename TInputImage::IndexValueType i;
  const double  rangeDistanceThreshold = m_DynamicRangeUsed;

  // Now we are ready to bilateral filter!
  //
  //
  //

  // Boundary condition
  ZeroFluxNeumannBoundaryCondition< TInputImage > BC;

  // Find the boundary "faces"
  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< InputImageType >::FaceListType faceList;
  NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< InputImageType > fC;
  faceList = fC( this->GetInput(), outputRegionForThread,
                 m_GaussianKernel.GetRadius() );

  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator< InputImageType >::FaceListType::iterator fit;

  OutputPixelRealType centerPixel;
  OutputPixelRealType val, tableArg, normFactor, rangeGaussian,
                      rangeDistance, pixel, gaussianProduct;

  const double distanceToTableIndex =
    static_cast< double >( m_NumberOfRangeGaussianSamples ) / m_DynamicRangeUsed;

  // Process all the faces, the NeighborhoodIterator will deteremine
  // whether a specified region needs to use the boundary conditions or
  // not.
  NeighborhoodIteratorType               b_iter;
  ImageRegionIterator< OutputImageType > o_iter;
  KernelConstIteratorType                k_it;
  KernelConstIteratorType                kernelEnd = m_GaussianKernel.End();

  ProgressReporter progress( this, threadId, outputRegionForThread.GetNumberOfPixels() );

  for ( fit = faceList.begin(); fit != faceList.end(); ++fit )
    {
    // walk the boundary face and the corresponding section of the output
    b_iter = NeighborhoodIteratorType(m_GaussianKernel.GetRadius(),
                                      this->GetInput(), *fit);
    b_iter.OverrideBoundaryCondition(&BC);
    o_iter = ImageRegionIterator< OutputImageType >(this->GetOutput(), *fit);

    while ( !b_iter.IsAtEnd() )
      {
      // Setup
      centerPixel = static_cast< OutputPixelRealType >( b_iter.GetCenterPixel() );
      val = 0.0;
      normFactor = 0.0;

      // Walk the neighborhood of the input and the kernel
      for ( i = 0, k_it = m_GaussianKernel.Begin(); k_it < kernelEnd;
            ++k_it, ++i )
        {
        // range distance between neighborhood pixel and neighborhood center
        pixel = static_cast< OutputPixelRealType >( b_iter.GetPixel(i) );
        rangeDistance = pixel - centerPixel;
        // flip sign if needed
        if ( rangeDistance < 0.0 )
          {
          rangeDistance *= -1.0;
          }

        // if the range distance is close enough, then use the pixel
        if ( rangeDistance < rangeDistanceThreshold )
          {
          // look up the range gaussian in a table
          tableArg = rangeDistance * distanceToTableIndex;
          rangeGaussian = m_RangeGaussianTable[Math::Floor < SizeValueType > ( tableArg )];

          // normalization factor so filter integrates to one
          // (product of the domain and the range gaussian)
          gaussianProduct = ( *k_it ) * rangeGaussian;
          normFactor += gaussianProduct;

          // Input Image * Domain Gaussian * Range Gaussian
          val += pixel * gaussianProduct;
          }
        }
      // normalize the value
      val /= normFactor;

      // store the filtered value
      o_iter.Set( static_cast< OutputPixelType >( val ) );

      ++b_iter;
      ++o_iter;
      progress.CompletedPixel();
      }
    }
}

template< typename TInputImage, typename TOutputImage >
void
BilateralImageFilter< TInputImage, TOutputImage >
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "DomainSigma: " << m_DomainSigma << std::endl;
  os << indent << "RangeSigma: " << m_RangeSigma << std::endl;
  os << indent << "FilterDimensionality: " << m_FilterDimensionality << std::endl;
  os << indent << "NumberOfRangeGaussianSamples: " << m_NumberOfRangeGaussianSamples << std::endl;
  os << indent << "Input dynamic range: " << m_DynamicRange << std::endl;
  os << indent << "Amount of dynamic range used: " << m_DynamicRangeUsed << std::endl;
  os << indent << "AutomaticKernelSize: " << m_AutomaticKernelSize << std::endl;
  os << indent << "Radius: " << m_Radius << std::endl;
}
} // end namespace itk

#endif
