/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGaussianImageSource.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkGaussianImageSource_txx
#define _itkGaussianImageSource_txx

#include "itkGaussianImageSource.h"
#include "itkGaussianSpatialFunction.h"
#include "itkImageRegionIterator.h"
#include "itkObjectFactory.h"
 
namespace itk
{

template <class TOutputImage>
GaussianImageSource<TOutputImage>
::GaussianImageSource()
{
  m_Size = new unsigned long [TOutputImage::GetImageDimension()];
  m_Spacing = new float [TOutputImage::GetImageDimension()];
  m_Origin = new float [TOutputImage::GetImageDimension()];  

  //Initial image is 64 wide in each direction.
  for (unsigned int i=0; i<TOutputImage::GetImageDimension(); i++)
    {
    m_Size[i] = 64;
    m_Spacing[i] = 1.0;
    m_Origin[i] = 0.0;
    }

  // Gaussian parameters, defined so that the gaussian
  // is centered in the default image
  m_Mean = ArrayType::Filled(32.0);
  m_Sigma = ArrayType::Filled(16.0);
  m_Scale = 255.0;
  m_Normalized = false;

}

template <class TOutputImage>
GaussianImageSource<TOutputImage>
::~GaussianImageSource()
{
  delete [] m_Size;
  delete [] m_Spacing;
  delete [] m_Origin;
}

template <class TOutputImage>
void 
GaussianImageSource<TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  unsigned int i;
    
  os << indent << "Size: [";
  for (i=0; i < NDimensions - 1; i++)
    {
    os << m_Size[i] << ", ";
    }
  os << "]" << std::endl;

  os << indent << "Origin: [";
  for (i=0; i < NDimensions - 1; i++)
    {
    os << m_Origin[i] << ", ";
    }
  os << "]" << std::endl;

  os << indent << "Spacing: [";
  for (i=0; i < NDimensions - 1; i++)
    {
    os << m_Spacing[i] << ", ";
    }
  os << "]" << std::endl;

  os << indent << "Gaussian sigma: [";
  for (i=0; i < NDimensions - 1; i++)
    {
    os << m_Sigma[i] << ", ";
    }
  os << "]" << std::endl;

  os << indent << "Gaussian mean: [";
  for (i=0; i < NDimensions - 1; i++)
    {
    os << m_Mean[i] << ", ";
    }
  os << "]" << std::endl;

  os << indent << "Gaussian scale: " << m_Scale << std::endl;
  os << indent << "Normalized Gaussian?: " << m_Normalized << std::endl;
}

template <typename TOutputImage>
void 
GaussianImageSource<TOutputImage>
::GenerateData()
{
  // Initialize the output image
  TOutputImage *outputPtr;
  typename TOutputImage::IndexType index = {{0}};
  typename TOutputImage::SizeType size = {{0}};
  size.SetSize( m_Size );
  
  outputPtr = this->GetOutput(0);

  typename TOutputImage::RegionType largestPossibleRegion;
  largestPossibleRegion.SetSize( size );
  largestPossibleRegion.SetIndex( index );
  outputPtr->SetLargestPossibleRegion( largestPossibleRegion );
  outputPtr->SetBufferedRegion( largestPossibleRegion );
  outputPtr->Allocate();

  outputPtr->SetSpacing(m_Spacing);
  outputPtr->SetOrigin(m_Origin);
  
  // Create and initialize a new gaussian function
  typedef itk::GaussianSpatialFunction<double, NDimensions> TFunctionType;
  typedef TFunctionType::InputType TFunctionPositionType;
  TFunctionType::Pointer pGaussian = TFunctionType::New();
  
  pGaussian->SetSigma(m_Sigma);
  pGaussian->SetMean(m_Mean);
  pGaussian->SetScale(m_Scale);
  pGaussian->SetNormalized(m_Normalized);

  // Create an iterator that will walk the output region
  typedef ImageRegionIterator<TOutputImage> OutputIterator;
  OutputIterator outIt = OutputIterator(outputPtr, largestPossibleRegion);

  // The value produced by the spatial function
  double value;

  // The position at which the function is evaluated
  Point<double, TOutputImage::ImageDimension> evalPoint;

  // Walk the output image, evaluating the spatial function at each pixel
  for ( ; !outIt.IsAtEnd(); ++outIt)
    {
    typename TOutputImage::IndexType index = outIt.GetIndex();
    outputPtr->TransformIndexToPhysicalPoint(index, evalPoint );
    value = pGaussian->Evaluate(evalPoint);

    // Set the pixel value to the function value
    outIt.Set( (typename TOutputImage::PixelType) value);
    }
}

} // end namespace itk

#endif
