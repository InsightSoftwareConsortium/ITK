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
  //Initial image is 64 wide in each direction.
  for (unsigned int i=0; i<TOutputImage::GetImageDimension(); i++)
    {
    m_Size[i] = 64;
    m_Spacing[i] = 1.0;
    m_Origin[i] = 0.0;
    }

  // Gaussian parameters, defined so that the gaussian
  // is centered in the default image
  m_Mean.Fill(32.0);
  m_Sigma.Fill(16.0);
  m_Scale = 255.0;
  m_Normalized = false;

}

template <class TOutputImage>
GaussianImageSource<TOutputImage>
::~GaussianImageSource()
{
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

//----------------------------------------------------------------------------
template <typename TOutputImage>
void 
GaussianImageSource<TOutputImage>
::GenerateOutputInformation()
{
  TOutputImage *output;
  typename TOutputImage::IndexType index = {{0}};
  typename TOutputImage::SizeType size = {{0}};
  size.SetSize( m_Size );
  
  output = this->GetOutput(0);

  typename TOutputImage::RegionType largestPossibleRegion;
  largestPossibleRegion.SetSize( size );
  largestPossibleRegion.SetIndex( index );
  output->SetLargestPossibleRegion( largestPossibleRegion );

  output->SetSpacing(m_Spacing);
  output->SetOrigin(m_Origin);
}

template <typename TOutputImage>
void 
GaussianImageSource<TOutputImage>
::GenerateData()
{
  typename TOutputImage::Pointer outputPtr = this->GetOutput();

  // allocate the output buffer
  outputPtr->SetBufferedRegion( outputPtr->GetRequestedRegion() );
  outputPtr->Allocate();

  // Create and initialize a new gaussian function
  typedef itk::GaussianSpatialFunction<double, NDimensions> FunctionType;
  typedef typename FunctionType::InputType FunctionPositionType;
  typename FunctionType::Pointer pGaussian = FunctionType::New();
  
  pGaussian->SetSigma(m_Sigma);
  pGaussian->SetMean(m_Mean);
  pGaussian->SetScale(m_Scale);
  pGaussian->SetNormalized(m_Normalized);

  // Create an iterator that will walk the output region
  typedef ImageRegionIterator<TOutputImage> OutputIterator;
  OutputIterator outIt = OutputIterator(outputPtr,
                                        outputPtr->GetRequestedRegion());

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

template<typename TOutputImage>
void 
GaussianImageSource<TOutputImage>
::SetSpacing(const float spacing[TOutputImage::ImageDimension] )
{
  unsigned int i; 
  for (i=0; i<TOutputImage::ImageDimension; i++)
    {
    if ( (double)spacing[i] != m_Spacing[i] )
      {
      break;
      }
    } 
  if ( i < TOutputImage::ImageDimension ) 
    { 
    for (i=0; i<TOutputImage::ImageDimension; i++)
      {
      m_Spacing[i] = spacing[i];
      }
    this->Modified();
    }
}

template<typename TOutputImage>
void 
GaussianImageSource<TOutputImage>
::SetSpacing(const double spacing[TOutputImage::ImageDimension] )
{
  unsigned int i; 
  for (i=0; i<TOutputImage::ImageDimension; i++)
    {
    if ( spacing[i] != m_Spacing[i] )
      {
      break;
      }
    } 
  if ( i < TOutputImage::ImageDimension ) 
    { 
    for (i=0; i<TOutputImage::ImageDimension; i++)
      {
      m_Spacing[i] = spacing[i];
      }
    this->Modified();
    }
}

template<typename TOutputImage>
void 
GaussianImageSource<TOutputImage>
::SetOrigin(const float origin[TOutputImage::ImageDimension] )
{
  unsigned int i; 
  for (i=0; i<TOutputImage::ImageDimension; i++)
    {
    if ( (double)origin[i] != m_Origin[i] )
      {
      break;
      }
    } 
  if ( i < TOutputImage::ImageDimension ) 
    { 
    for (i=0; i<TOutputImage::ImageDimension; i++)
      {
      m_Origin[i] = origin[i];
      }
    this->Modified();
    }
}

template<typename TOutputImage>
void 
GaussianImageSource<TOutputImage>
::SetOrigin(const double origin[TOutputImage::ImageDimension] )
{
  unsigned int i; 
  for (i=0; i<TOutputImage::ImageDimension; i++)
    {
    if ( origin[i] != m_Origin[i] )
      {
      break;
      }
    } 
  if ( i < TOutputImage::ImageDimension ) 
    { 
    for (i=0; i<TOutputImage::ImageDimension; i++)
      {
      m_Origin[i] = origin[i];
      }
    this->Modified();
    }
}

template<typename TOutputImage>
void 
GaussianImageSource<TOutputImage>
::SetSize(const unsigned long size[TOutputImage::ImageDimension] )
{
  unsigned int i; 
  for (i=0; i<TOutputImage::ImageDimension; i++)
    {
    if ( size[i] != m_Size[i] )
      {
      break;
      }
    } 
  if ( i < TOutputImage::ImageDimension ) 
    { 
    for (i=0; i<TOutputImage::ImageDimension; i++)
      {
      m_Size[i] = size[i];
      }
    this->Modified();
    }
}


} // end namespace itk

#endif
