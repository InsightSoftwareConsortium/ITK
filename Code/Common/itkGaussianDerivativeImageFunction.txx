/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGaussianDerivativeImageFunction.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkGaussianDerivativeImageFunction_txx
#define _itkGaussianDerivativeImageFunction_txx

#include "itkGaussianDerivativeImageFunction.h"

namespace itk
{

/** Set the Input Image */
template <class TInputImage, class TOutput>
GaussianDerivativeImageFunction<TInputImage,TOutput>
::GaussianDerivativeImageFunction()
{
  typename GaussianFunctionType::ArrayType mean;
  mean[0]=0.0f;
  for(unsigned int i=0;i<itkGetStaticConstMacro(ImageDimension2);i++)
    {
    m_Sigma[i] = 1.0f;
    m_Extent[i] = 1.0f;
    }
  m_UseImageSpacing = true;
  m_GaussianDerivativeFunction = GaussianDerivativeFunctionType::New();
  m_GaussianFunction = GaussianFunctionType::New();
  m_OperatorImageFunction = OperatorImageFunctionType::New();
  m_GaussianFunction->SetMean(mean);
  m_GaussianFunction->SetNormalized(false); // faster
  m_GaussianDerivativeFunction->SetNormalized(false); // faster
  this->RecomputeGaussianKernel();
}

/** Print self method */
template <class TInputImage, class TOutput>
void
GaussianDerivativeImageFunction<TInputImage,TOutput>
::PrintSelf(std::ostream& os, Indent indent) const
{
  this->Superclass::PrintSelf(os,indent);
  os << indent << "UseImageSpacing: " << m_UseImageSpacing << std::endl;

  os << indent << "Sigma: " << m_Sigma << std::endl;
  os << indent << "Extent: " << m_Extent << std::endl;
  
  os << indent << "OperatorArray: " << m_OperatorArray << std::endl;
  os << indent << "ContinuousOperatorArray: " 
     << m_ContinuousOperatorArray << std::endl;
  os << indent << "OperatorImageFunction: " 
     << m_OperatorImageFunction << std::endl;
  os << indent << "GaussianDerivativeFunction: " 
     << m_GaussianDerivativeFunction << std::endl;
  os << indent << "GaussianFunction: " 
     << m_GaussianFunction << std::endl;
}

/** Set the input image */
template <class TInputImage, class TOutput>
void
GaussianDerivativeImageFunction<TInputImage,TOutput>
::SetInputImage( const InputImageType * ptr )
{
  Superclass::SetInputImage(ptr);
  m_OperatorImageFunction->SetInputImage(ptr);
}

/** Set the variance of the gaussian in each direction */
template <class TInputImage, class TOutput>
void
GaussianDerivativeImageFunction<TInputImage,TOutput>
::SetSigma( const double variance[itkGetStaticConstMacro(ImageDimension2)] )
{
  unsigned int i; 
  for (i=0; i<itkGetStaticConstMacro(ImageDimension2); i++)
    {
    if ( sigma[i] != m_Sigma[i] )
      {
      break;
      }
    } 
  if ( i < itkGetStaticConstMacro(ImageDimension2) ) 
    { 
    for (i=0; i<itkGetStaticConstMacro(ImageDimension2); i++)
      {
      m_Sigma[i] = sigma[i];
      }
    this->RecomputeGaussianKernel();
    }
}


/** Set the variance of the gaussian in each direction */
template <class TInputImage, class TOutput>
void
GaussianDerivativeImageFunction<TInputImage,TOutput>
::SetSigma (const double sigma)
{
  unsigned int i; 
  for (i=0; i<itkGetStaticConstMacro(ImageDimension2); i++)
    {
    if ( sigma != m_Sigma[i] )
      {
      break;
      }
    } 
  if ( i < itkGetStaticConstMacro(ImageDimension2) ) 
    { 
    for (i=0; i<itkGetStaticConstMacro(ImageDimension2); i++)
      {
      m_Sigma[i] = sigma;
      }
    this->RecomputeGaussianKernel();
    }
}

/** Set the extent of the gaussian in each direction */
template <class TInputImage, class TOutput>
void
GaussianDerivativeImageFunction<TInputImage,TOutput>
::SetExtent( const double extent[itkGetStaticConstMacro(ImageDimension2)] )
{
  unsigned int i; 
  for (i=0; i<itkGetStaticConstMacro(ImageDimension2); i++)
    {
    if ( extent[i] != m_Extent[i] )
      {
      break;
      }
    } 
  if ( i < itkGetStaticConstMacro(ImageDimension2) ) 
    { 
    for (i=0; i<itkGetStaticConstMacro(ImageDimension2); i++)
      {
      m_Extent[i] = extent[i];
      }
    this->RecomputeGaussianKernel();
    }
}


/** Set the extent of the gaussian in each direction */
template <class TInputImage, class TOutput>
void
GaussianDerivativeImageFunction<TInputImage,TOutput>
::SetExtent( const double extent)
{
  unsigned int i; 
  for (i=0; i<itkGetStaticConstMacro(ImageDimension2); i++)
    {
    if ( extent != m_Extent[i] )
      {
      break;
      }
    } 
  if ( i < itkGetStaticConstMacro(ImageDimension2) ) 
    { 
    for (i=0; i<itkGetStaticConstMacro(ImageDimension2); i++)
      {
      m_Extent[i] = extent;
      }
    this->RecomputeGaussianKernel();
    }
}

/** Recompute the gaussian kernel used to evaluate indexes
 *  This should use a fastest Derivative Gaussian operator*/
template <class TInputImage, class TOutput>
void
GaussianDerivativeImageFunction<TInputImage,TOutput>
::RecomputeGaussianKernel()
{
  unsigned int direction = 0;
  for(unsigned int op = 0; op<itkGetStaticConstMacro(ImageDimension2)*2; op++)
    {  
    
    // Set the derivative of the gaussian first
    OperatorNeighborhoodType dogNeighborhood;
    typename GaussianDerivativeFunctionType::InputType pt;
    typename NeighborhoodType::SizeType size;
    size.Fill(0);
    size[direction] = (unsigned long)(m_Sigma[direction]*m_Extent[direction]);
    dogNeighborhood.SetRadius(size);

    typename GaussianDerivativeFunctionType::ArrayType s;
    s[0] = m_Sigma[direction];
    m_GaussianDerivativeFunction->SetSigma(s);

    typename OperatorNeighborhoodType::Iterator it = dogNeighborhood.Begin();

    unsigned int i=0;
    //float sum = 0;
    while(it != dogNeighborhood.End() )
      {
      pt[0]= dogNeighborhood.GetOffset(i)[direction];
      
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
      (*it)= m_GaussianDerivativeFunction->Evaluate(pt);
      i++;
      it++;
      }


    m_OperatorArray[op] = dogNeighborhood;

    // Set the gaussian operator
    m_GaussianFunction->SetSigma(s);
    op++;
    OperatorNeighborhoodType gaussianNeighborhood;
    gaussianNeighborhood.SetRadius(size);

    it = gaussianNeighborhood.Begin();

    i=0;
    double sum = 0;
    while(it != gaussianNeighborhood.End() )
      {
      pt[0]= gaussianNeighborhood.GetOffset(i)[direction];
      
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
      it++;
      }

    m_OperatorArray[op] = gaussianNeighborhood;
    direction++;
    }
}

/** Evaluate the function at the specifed index */
template <class TInputImage, class TOutput>
typename GaussianDerivativeImageFunction<TInputImage,TOutput>::OutputType
GaussianDerivativeImageFunction<TInputImage,TOutput>
::EvaluateAtIndex(const IndexType& index) const
{
  OutputType gradient;

  for(unsigned int i=0; i<itkGetStaticConstMacro(ImageDimension2);i++)
    { 
    // Apply each gaussian kernel to a subset of the image
    InputPixelType pixel = this->GetInputImage()->GetPixel(index);
    double value = pixel;

    // gaussian blurring first 
    for(unsigned int direction=0;direction<itkGetStaticConstMacro(ImageDimension2);direction++)
      {
      if(i != direction)
        {
        unsigned int id= 2*direction+1; // select only gaussian kernel;
        unsigned int center = (unsigned int)((m_OperatorArray[id].GetSize()[direction]-1)/2);
        TOutput centerval = m_OperatorArray[id].GetCenterValue();
        m_OperatorArray[id][center] = 0;
        m_OperatorImageFunction->SetOperator(m_OperatorArray[id]);
        value = m_OperatorImageFunction->EvaluateAtIndex(index)+centerval*value;
        }
      }
    
    // then derivative in the direction
    signed int center = (unsigned int)((m_OperatorArray[2*i].GetSize()[i]-1)/2);
    TOutput centerval = m_OperatorArray[2*i].GetCenterValue();
    m_OperatorArray[2*i][center] = 0;
    m_OperatorImageFunction->SetOperator(m_OperatorArray[2*i]);      
    value = m_OperatorImageFunction->EvaluateAtIndex(index)+centerval*value;

    gradient[i] = value;
    }
  
  return gradient;
}

/** Recompute the gaussian kernel used to evaluate indexes 
 *  The variance should be uniform */
template <class TInputImage, class TOutput>
void
GaussianDerivativeImageFunction<TInputImage,TOutput>
::RecomputeContinuousGaussianKernel(
  const double offset[itkGetStaticConstMacro(ImageDimension2)]) const
{
  
  unsigned int direction = 0;
  for(unsigned int op = 0; op<itkGetStaticConstMacro(ImageDimension2)*2; op++)
    {    
    // Set the derivative of the gaussian first
    OperatorNeighborhoodType dogNeighborhood;
    typename GaussianDerivativeFunctionType::InputType pt;
    typename OperatorNeighborhoodType::SizeType size;
    size.Fill(0);
    size[direction] = (unsigned long)(m_Sigma[direction]*m_Extent[direction]);
    dogNeighborhood.SetRadius(size);

    typename GaussianDerivativeFunctionType::ArrayType s;
    s[0] = m_Sigma[direction];
    m_GaussianDerivativeFunction->SetSigma(s);

    typename OperatorNeighborhoodType::Iterator it = dogNeighborhood.Begin();

    unsigned int i=0;
    //float sum = 0;
    while(it != dogNeighborhood.End() )
      {
      pt[0]= dogNeighborhood.GetOffset(i)[direction]-offset[direction];
      
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
      (*it)= m_GaussianDerivativeFunction->Evaluate(pt);
      i++;
      it++;
      }


    m_ContinuousOperatorArray[op] = dogNeighborhood;

    // Set the gaussian operator
    m_GaussianFunction->SetSigma(s);
    op++;
    OperatorNeighborhoodType gaussianNeighborhood;
    gaussianNeighborhood.SetRadius(size);

    it = gaussianNeighborhood.Begin();

    i=0;
    double sum = 0;
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
      it++;
      }

    m_ContinuousOperatorArray[op] = gaussianNeighborhood;
    direction++;
    }
}

/** Evaluate the function at the specifed point */
template <class TInputImage, class TOutput>
typename GaussianDerivativeImageFunction<TInputImage,TOutput>::OutputType
GaussianDerivativeImageFunction<TInputImage,TOutput>
::Evaluate(const PointType& point) const
{
  OutputType gradient;
 
  IndexType index;

  double offset[itkGetStaticConstMacro(ImageDimension2)];
  for(unsigned int i=0; i<itkGetStaticConstMacro(ImageDimension2);i++)
    {
    index[i] = (unsigned long)point[i];
    offset[i] = point[i]-index[i];
    }

  this->RecomputeContinuousGaussianKernel(offset);

  for(unsigned int i=0; i<itkGetStaticConstMacro(ImageDimension2);i++)
    { 
    // Apply each gaussian kernel to a subset of the image
    InputPixelType pixel = this->GetInputImage()->GetPixel(index);
    double value = pixel;

    // gaussian blurring first 
    for(unsigned int direction=0;direction<itkGetStaticConstMacro(ImageDimension2);direction++)
      {
      if(i != direction)
        {
        unsigned int id= 2*direction+1; // select only gaussian kernel;
        unsigned int center = (unsigned int)((m_ContinuousOperatorArray[id].GetSize()[direction]-1)/2);
        TOutput centerval = m_ContinuousOperatorArray[id][center];
        m_ContinuousOperatorArray[id][center] = 0;
        m_OperatorImageFunction->SetOperator(m_ContinuousOperatorArray[id]);
        value = m_OperatorImageFunction->EvaluateAtIndex(index)+centerval*value;
        }
      }
    
    // then derivative in the direction
    signed int center = (unsigned int)((m_ContinuousOperatorArray[2*i].GetSize()[i]-1)/2);
    TOutput centerval = m_ContinuousOperatorArray[2*i][center];
    m_ContinuousOperatorArray[2*i][center] = 0;
    m_OperatorImageFunction->SetOperator(m_ContinuousOperatorArray[2*i]);      
    value = m_OperatorImageFunction->EvaluateAtIndex(index)+centerval*value;

    gradient[i] = value;
    }

  return gradient;
}

/** Evaluate the function at specified ContinousIndex position.*/
template <class TInputImage, class TOutput>
typename GaussianDerivativeImageFunction<TInputImage,TOutput>::OutputType
GaussianDerivativeImageFunction<TInputImage,TOutput>
::EvaluateAtContinuousIndex(const ContinuousIndexType & index ) const
{
  PointType point;
  for(unsigned int i=0; i<itkGetStaticConstMacro(ImageDimension2);i++)
    {
    point[i] = index[i];
    }
  return this->Evaluate(point);
}

} // namespace itk

#endif
