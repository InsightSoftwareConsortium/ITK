/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGaussianBlurImageFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkGaussianBlurImageFunction_txx
#define _itkGaussianBlurImageFunction_txx

#include "itkGaussianBlurImageFunction.h"

namespace itk
{

/** Set the Input Image */
template <class TInputImage,class TOutput>
GaussianBlurImageFunction<TInputImage,TOutput>
::GaussianBlurImageFunction()
{
  typename GaussianFunctionType::ArrayType mean;
  mean[0]=0.0f;
  for(unsigned int i=0;i<itkGetStaticConstMacro(ImageDimension);i++)
    {
    m_Sigma[i] = 1.0f;
    m_MaximumError[i] = 0.001f;
    m_MaximumKernelWidth = 32;
    m_Extent[i] = 1.0f;
    }
  m_UseImageSpacing = true;

  m_GaussianFunction = GaussianFunctionType::New();
  m_GaussianFunction->SetMean(mean);
  m_GaussianFunction->SetNormalized(false); // faster
  m_OperatorImageFunction = OperatorImageFunctionType::New();
  this->RecomputeGaussianKernel();
}

/** Set the input image */
template <class TInputImage,class TOutput>
void
GaussianBlurImageFunction<TInputImage,TOutput>
::SetInputImage( const InputImageType * ptr )
{
  Superclass::SetInputImage(ptr);
  m_OperatorImageFunction->SetInputImage(ptr);
}


/** Print self method */
template <class TInputImage,class TOutput>
void
GaussianBlurImageFunction<TInputImage,TOutput>
::PrintSelf(std::ostream& os, Indent indent) const
{
  this->Superclass::PrintSelf(os,indent);
 
  for(unsigned int i=0;i<itkGetStaticConstMacro(ImageDimension);i++)
    {
    os << indent << "Sigma["<< i << "] : " <<  m_Sigma[i] << std::endl;
    os << indent << "MaximumError["<< i << "] : " << m_MaximumError[i] << std::endl;
    os << indent << "Extent["<< i << "] : " << m_Extent[i] << std::endl;
    }
  os << indent << "MaximumKernelWidth: " << m_MaximumKernelWidth << std::endl;
  os << indent << "UseImageSpacing: " << m_UseImageSpacing << std::endl;

}

/** Set the variance of the gaussian in each direction */
template <class TInputImage,class TOutput>
void
GaussianBlurImageFunction<TInputImage,TOutput>
::SetSigma( const double sigma[itkGetStaticConstMacro(ImageDimension)] )
{
  unsigned int i; 
  for (i=0; i<itkGetStaticConstMacro(ImageDimension); i++)
    {
    if ( sigma[i] != m_Sigma[i] )
      {
      break;
      }
    } 
  if ( i < itkGetStaticConstMacro(ImageDimension) ) 
    { 
    for (i=0; i<itkGetStaticConstMacro(ImageDimension); i++)
      {
      m_Sigma[i] = sigma[i];
      }
    this->RecomputeGaussianKernel();
    }
}


/** Set the variance of the gaussian in each direction */
template <class TInputImage,class TOutput>
void
GaussianBlurImageFunction<TInputImage,TOutput>
::SetSigma( const double sigma)
{
  unsigned int i; 
  for (i=0; i<itkGetStaticConstMacro(ImageDimension); i++)
    {
    if ( sigma != m_Sigma[i] )
      {
      break;
      }
    } 
  if ( i < itkGetStaticConstMacro(ImageDimension) ) 
    { 
    for (i=0; i<itkGetStaticConstMacro(ImageDimension); i++)
      {
      m_Sigma[i] = sigma;
      }
    this->RecomputeGaussianKernel();
    }
}

/** Set the extent of the gaussian in each direction */
template <class TInputImage,class TOutput>
void
GaussianBlurImageFunction<TInputImage,TOutput>
::SetExtent( const double extent[itkGetStaticConstMacro(ImageDimension)] )
{
  unsigned int i; 
  for (i=0; i<itkGetStaticConstMacro(ImageDimension); i++)
    {
    if ( extent[i] != m_Extent[i] )
      {
      break;
      }
    } 
  if ( i < itkGetStaticConstMacro(ImageDimension) ) 
    { 
    for (i=0; i<itkGetStaticConstMacro(ImageDimension); i++)
      {
      m_Extent[i] = extent[i];
      }
    this->RecomputeGaussianKernel();
    }
}


/** Set the extent of the gaussian in each direction */
template <class TInputImage,class TOutput>
void
GaussianBlurImageFunction<TInputImage,TOutput>
::SetExtent( const double extent)
{
  unsigned int i; 
  for (i=0; i<itkGetStaticConstMacro(ImageDimension); i++)
    {
    if ( extent != m_Extent[i] )
      {
      break;
      }
    } 
  if ( i < itkGetStaticConstMacro(ImageDimension) ) 
    { 
    for (i=0; i<itkGetStaticConstMacro(ImageDimension); i++)
      {
      m_Extent[i] = extent;
      }
    this->RecomputeGaussianKernel();
    }
}

/** Recompute the gaussian kernel used to evaluate indexes*/
template <class TInputImage,class TOutput>
void
GaussianBlurImageFunction<TInputImage,TOutput>
::RecomputeGaussianKernel()
{
  // Compute the convolution of each kernel in each direction
  for(unsigned int direction=0;direction<itkGetStaticConstMacro(ImageDimension);direction++)
    {
    GaussianOperatorType gaussianOperator;
 
    gaussianOperator.SetDirection(direction);
    gaussianOperator.SetMaximumError(m_MaximumError[direction]);
    gaussianOperator.SetMaximumKernelWidth(m_MaximumKernelWidth);

    if( (m_UseImageSpacing == true) && (this->GetInputImage()) )
      {
      if (this->GetInputImage()->GetSpacing()[direction] == 0.0)
        {
        itkExceptionMacro(<< "Pixel spacing cannot be zero");
        }
      else
        {
        gaussianOperator.SetVariance(m_Sigma[direction]*m_Sigma[direction]  / this->GetInputImage()->GetSpacing()[direction]);
        }
      }
    else
      {
      gaussianOperator.SetVariance(m_Sigma[direction]*m_Sigma[direction]);
      }

    gaussianOperator.CreateDirectional();
    m_OperatorArray[direction] = gaussianOperator;
    }
}

/** Evaluate the function at the specifed point */
template <class TInputImage,class TOutput>
TOutput
GaussianBlurImageFunction<TInputImage,TOutput>
::EvaluateAtIndex(const IndexType& index) const
{
  // Apply each gaussian kernel to a subset of the image
  InputPixelType pixel = this->GetInputImage()->GetPixel(index);
  double value = pixel;

  for(unsigned int direction=0;direction<itkGetStaticConstMacro(ImageDimension);direction++)
    {
    unsigned int center = (unsigned int)((m_OperatorArray[direction].GetSize()[direction]-1)/2);
    double centerval = m_OperatorArray[direction].GetCenterValue();
    m_OperatorArray[direction][center] = 0;
    m_OperatorImageFunction->SetOperator(m_OperatorArray[direction]);
    value = m_OperatorImageFunction->EvaluateAtIndex(index)+centerval*value;
    //std::cout << "value1 = " << centerval << std::endl;
    }
  return value;
}


/** Recompute the gaussian kernel used to evaluate indexes 
 *  The variance should be uniform */
template <class TInputImage,class TOutput>
void
GaussianBlurImageFunction<TInputImage,TOutput>
::RecomputeContinuousGaussianKernel(
  const double offset[itkGetStaticConstMacro(ImageDimension)]) const
{
  for(unsigned int direction=0;direction<itkGetStaticConstMacro(ImageDimension);direction++)
    {
    NeighborhoodType gaussianNeighborhood;
    typename GaussianFunctionType::InputType pt;
    typename NeighborhoodType::SizeType size;
    size.Fill(0);
    size[direction] = (unsigned long)(m_Sigma[direction]*m_Extent[direction]);

    gaussianNeighborhood.SetRadius(size); 

    typename NeighborhoodType::Iterator it = gaussianNeighborhood.Begin();

    itk::FixedArray<double,1> s;
    s[0]=m_Sigma[direction];
    m_GaussianFunction->SetSigma(s);

    unsigned int i=0;
    float sum = 0;
    while(it != gaussianNeighborhood.End() )
      {
      pt[0]= gaussianNeighborhood.GetOffset(i)[direction]-offset[direction];
      if( (m_UseImageSpacing == true) && (this->GetInputImage()) )
        {
        if (this->GetInputImage()->GetSpacing()[direction] == 0.0)
          {
          itkExceptionMacro(<< "Pixel spacing cannot be zero");
          }
        else
          {
          pt[0] *= this->GetInputImage()->GetSpacing()[direction];
          }
        }
       
      (*it)= m_GaussianFunction->Evaluate(pt);
      sum += (*it);
      i++;
      it++;
      }

    // Make the filter DC-Constant
    it = gaussianNeighborhood.Begin();
    while(it != gaussianNeighborhood.End() )
      {    
      (*it) /= sum;
      //std::cout << *it << std::endl;
      it++;
      }
    //std::cout << std::endl;
    //std::cout << std::endl;
    m_ContinuousOperatorArray[direction] = gaussianNeighborhood;
    }
}

/** Evaluate the function at the specifed point */
template <class TInputImage,class TOutput>
TOutput
GaussianBlurImageFunction<TInputImage,TOutput>
::Evaluate(const PointType& point) const
{

  IndexType index;

  double offset[itkGetStaticConstMacro(ImageDimension)];
  for(unsigned int i=0; i<itkGetStaticConstMacro(ImageDimension);i++)
    {
    index[i] = (unsigned long)point[i];
    offset[i] = point[i]-index[i];
    }

  this->RecomputeContinuousGaussianKernel(offset);

  // Apply each gaussian kernel to a subset of the image
  InputPixelType pixel = this->GetInputImage()->GetPixel(index);
  double value = pixel;

  for(unsigned int direction=0;direction<itkGetStaticConstMacro(ImageDimension);direction++)
    {
    unsigned int center = (unsigned int)((m_ContinuousOperatorArray[direction].GetSize()[direction]-1)/2);
    double centerval = m_ContinuousOperatorArray[direction][center];
    m_ContinuousOperatorArray[direction][center] = 0;
    m_OperatorImageFunction->SetOperator(m_ContinuousOperatorArray[direction]);
    value = m_OperatorImageFunction->EvaluateAtIndex(index)+centerval*value;
    }

  return value;
}

/** Evaluate the function at specified ContinousIndex position.*/
template <class TInputImage,class TOutput>
TOutput
GaussianBlurImageFunction<TInputImage,TOutput>
::EvaluateAtContinuousIndex( const ContinuousIndexType & index ) const
{
  PointType point;
  for(unsigned int i=0; i<itkGetStaticConstMacro(ImageDimension);i++)
    {
    point[i] = index[i];
    }
  return this->Evaluate(point);

}

} // namespace itk

#endif
