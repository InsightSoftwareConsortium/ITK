/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorGradientMagnitudeImageFilter.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkVectorGradientMagnitudeImageFilter_txx
#define _itkVectorGradientMagnitudeImageFilter_txx

#include "itkVectorGradientMagnitudeImageFilter.h"

#include "itkNeighborhoodAlgorithm.h"
#include "itkImageRegionIterator.h"
#include "itkZeroFluxNeumannBoundaryCondition.h"
#include "itkProgressReporter.h"
#include "itkVectorCastImageFilter.h"

#include "vnl/vnl_math.h"

namespace itk
{

template <typename TInputImage, typename TRealType, typename TOutputImage>
void
VectorGradientMagnitudeImageFilter<TInputImage, TRealType, TOutputImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  unsigned i;
  Superclass::PrintSelf(os,indent);
  os << indent << "m_UseImageSpacing = "          << m_UseImageSpacing
     << std::endl;
  os << indent << "m_UsePrincipleComponents = "   << m_UseImageSpacing
     << std::endl;
  os << indent << "m_RequestedNumberOfThreads = " << m_RequestedNumberOfThreads
     << std::endl;
  os << indent << "m_DerivativeWeights = ";
  for (i = 0; i < ImageDimension; i++)
    { os << m_DerivativeWeights[i] << " "; }
  os << std::endl;
  os << indent << "m_ComponentWeights = ";
  for (i = 0; i < VectorDimension; i++)
    { os << m_ComponentWeights[i] << " "; }
  os << std::endl;
}
  
template <typename TInputImage, typename TRealType, typename TOutputImage>
VectorGradientMagnitudeImageFilter<TInputImage, TRealType, TOutputImage>
::VectorGradientMagnitudeImageFilter()
{
  unsigned int i;
  m_UseImageSpacing = false;
  m_UsePrincipleComponents = true;
  m_RequestedNumberOfThreads = this->GetNumberOfThreads();
  for (i = 0; i < ImageDimension; i++)
    {
    m_NeighborhoodRadius[i] = 1; // radius of neighborhood we will use
    m_DerivativeWeights[i] = static_cast<TRealType>(1.0);
    }
  for (i = 0; i < VectorDimension; i++)
    {
    m_ComponentWeights[i] = static_cast<TRealType>(1.0);
    m_SqrtComponentWeights[i] = static_cast<TRealType>(1.0);
    }
}
template <typename TInputImage, typename TRealType, typename TOutputImage>
void
VectorGradientMagnitudeImageFilter<TInputImage, TRealType, TOutputImage>
::SetDerivativeWeights(TRealType data[])
{
  m_UseImageSpacing = false;

  for (unsigned i = 0; i < ImageDimension; ++i)
    {
    if (m_DerivativeWeights[i] != data[i])
      {
      this->Modified();
      m_DerivativeWeights[i] = data[i];
      }
    }
}

template <typename TInputImage, typename TRealType, typename TOutputImage>
void 
VectorGradientMagnitudeImageFilter<TInputImage, TRealType, TOutputImage>
::SetUseImageSpacing(bool f)
{
  if (m_UseImageSpacing == f) { return; }

  // Only reset the weights if they were previously set to the image spacing,
  // otherwise, the user may have provided their own weightings.
  if (f == false && m_UseImageSpacing == true)
    {
    for (unsigned i = 0; i < ImageDimension; ++i)
      {
      m_DerivativeWeights[i] = static_cast<TRealType>(1.0);
      }
    }

  m_UseImageSpacing = f;
}
  
template <typename TInputImage, typename TRealType, typename TOutputImage>
void 
VectorGradientMagnitudeImageFilter<TInputImage, TRealType, TOutputImage>
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
    OStringStream msg;
    msg << static_cast<const char *>(this->GetNameOfClass())
        << "::GenerateInputRequestedRegion()";
    e.SetLocation(msg.str().c_str());
    e.SetDescription("Requested region is (at least partially) outside the largest possible region.");
    e.SetDataObject(inputPtr);
    throw e;
    }
}

template< typename TInputImage, typename TRealType, typename TOutputImage >
void
VectorGradientMagnitudeImageFilter<TInputImage, TRealType, TOutputImage>
::BeforeThreadedGenerateData()
{
  Superclass::BeforeThreadedGenerateData();

  // Calculate the square-roots of the component weights.
  for (unsigned i = 0; i < VectorDimension; ++i)
    {
    if (m_ComponentWeights[i] < 0 )
      {
      itkExceptionMacro( << "Component weights must be positive numbers" );
      }
    m_SqrtComponentWeights[i] = ::sqrt(m_ComponentWeights[i]);
    }

  
  // Set the weights on the derivatives.
  // Are we using image spacing in the calculations?  If so we must update now
  // in case our input image has changed.
  if (m_UseImageSpacing == true)
    {

    for (unsigned i = 0; i < ImageDimension; i++)
      {
      if (static_cast<TRealType>(this->GetInput()->GetSpacing()[i]) == 0.0)
        {
        itkExceptionMacro(<< "Image spacing in dimension " << i << " is zero.");
        }
      m_DerivativeWeights[i]
        = static_cast<TRealType>( 1.0 /
                                  static_cast<TRealType>(this->GetInput()->GetSpacing()[i]) );
      }
    }

  // If using the principle components method, then force this filter to use a
  // single thread because vnl eigensystem objects are not thread-safe.  3D
  // data is ok because we have a special solver.
  if (m_UsePrincipleComponents == true && ImageDimension != 3)
    {
    m_RequestedNumberOfThreads = this->GetNumberOfThreads();
    this->SetNumberOfThreads(1);
    }
  else
    {
    this->SetNumberOfThreads(m_RequestedNumberOfThreads);
    }

  /** If the input needs casting to a real-valued vector type, create the
      appropriate image and set the m_RealValuedInputImage pointer to this
      image.  Otherwise just point to the input image. */
  if ( typeid( typename InputImageType::PixelType ) != typeid( RealVectorType ) )
    {
    typename VectorCastImageFilter<TInputImage, RealVectorImageType>::Pointer
      caster = VectorCastImageFilter<TInputImage, RealVectorImageType>::New();
    caster->SetInput(this->GetInput());
    caster->Update();
    m_RealValuedInputImage = caster->GetOutput();
    }
  else
    {
    m_RealValuedInputImage
      = dynamic_cast<const ImageBase<ImageDimension> *>(this->GetInput());
    }
  
}

template< typename TInputImage, typename TRealType, typename TOutputImage >
void
VectorGradientMagnitudeImageFilter< TInputImage, TRealType, TOutputImage >
::ThreadedGenerateData(const OutputImageRegionType& outputRegionForThread,
                       int threadId)
{

  ZeroFluxNeumannBoundaryCondition<RealVectorImageType> nbc;
  ConstNeighborhoodIteratorType bit;
  ImageRegionIterator<TOutputImage> it;
  
  // Find the data-set boundary "faces"
  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<RealVectorImageType>::
    FaceListType faceList;
  NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<RealVectorImageType> bC;
  faceList = bC(dynamic_cast<const RealVectorImageType *>(m_RealValuedInputImage.GetPointer()),
                outputRegionForThread, m_NeighborhoodRadius);

  typename NeighborhoodAlgorithm::ImageBoundaryFacesCalculator<RealVectorImageType>::
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
                                        dynamic_cast<const RealVectorImageType *>(m_RealValuedInputImage.GetPointer()),
                                        *fit);
    it = ImageRegionIterator<TOutputImage>(this->GetOutput(), *fit);
    bit.OverrideBoundaryCondition(&nbc);
    bit.GoToBegin();

    if (m_UsePrincipleComponents == true)
      {
      if (ImageDimension == 3)
        { // Use the specialized eigensolve which can be threaded
        while ( ! bit.IsAtEnd() )
          {
          it.Set( this->EvaluateAtNeighborhood3D(bit) );
          ++bit;
          ++it;
          progress.CompletedPixel();
          }
        }
      else
        {
        while ( ! bit.IsAtEnd() )
          {
          it.Set( this->EvaluateAtNeighborhood(bit) );
          ++bit;
          ++it;
          progress.CompletedPixel();
          }
        }
      }
    else
      {
      while ( ! bit.IsAtEnd() )
        {
        it.Set( this->NonPCEvaluateAtNeighborhood(bit) );
        ++bit;
        ++it;
        progress.CompletedPixel();
        }
      }
    }
}

template <typename TInputImage, typename TRealType, typename TOutputImage>
int
VectorGradientMagnitudeImageFilter<TInputImage, TRealType, TOutputImage>
::CubicSolver(double *c, double *s)
{
  // IMPORTANT
  // This code is specialized for particular case of positive symmetric
  // matrix.   It also assumes that x^3 coefficient is 1. c contains the
  // coefficients of the polynomial: x^3 + c[2]x^2 + c[1]x^1 + c[0].  The roots
  // s are not necessarily sorted, and int is the number of distinct roots
  // found in s.
  int     num;
  const double pi = 3.14159265358979323846;
  const double epsilon = 1.0e-11;

  // Substitution of  x = y - c[2]/3 eliminate the quadric term  x^3 +px + q = 0
  double sq_c2 = c[2] * c[2];
  double p = 1.0/3 * (- 1.0/3.0 * sq_c2 + c[1]);
  double q = 1.0/2 * (2.0/27.0 * c[2] * sq_c2 - 1.0/3.0 * c[2] * c[1] + c[0]);
  
  // Cardano's formula
  double cb_p = p * p * p;
  double D = q * q + cb_p;
  
  if (D < -epsilon) // D < 0, three real solutions, by far the common case.
    {
    double phi = 1.0/3.0 * acos(-q / sqrt(-cb_p));
    double t = 2.0 * sqrt(-p);
      
    s[0] =   t * cos(phi);
    s[1] = - t * cos(phi + pi / 3);
    s[2] = - t * cos(phi - pi / 3);
    num = 3;
    }

  else if (D < epsilon) // D == 0
    {
    if (q > -epsilon && q < epsilon)
      {
      s[0] = 0.0;
      num = 1;
      }
    else
      {
      double u = vnl_math_cuberoot(-q);
      s[0] = 2 * u;
      s[1] = - u;
      num = 2;
      }
    }
  else // Only one real solution. This case misses a double root on rare
       // occasions with very large char eqn coefficients.
    {
    double sqrt_D = sqrt(D);
    double u = vnl_math_cuberoot(sqrt_D - q);
    double v = - vnl_math_cuberoot(sqrt_D + q);
      
    s[0] = u + v;
    num = 1;
    }
  
  // Resubstitute
  double sub = 1.0/3.0 * c[2];
  
  for (int i = 0; i < num; ++i)
    {      s[i] -= sub;    }
  
  return num;
}


} // end namespace itk

#endif
