/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    $RCSfile: itkDemonsImageToImageObjectMetric.hxx,v $
  Language:  C++
  Date:      $Date: $
  Version:   $Revision: $

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkDemonsImageToImageObjectMetric_hxx
#define __itkDemonsImageToImageObjectMetric_hxx

#include "itkDemonsImageToImageObjectMetric.h"

namespace itk
{

/**
 * Constructor
 */
template < class TFixedImage, class TMovingImage, class TVirtualImage >
DemonsImageToImageObjectMetric<TFixedImage,TMovingImage,TVirtualImage>
::DemonsImageToImageObjectMetric()
{
}

template < class TFixedImage, class TMovingImage, class TVirtualImage >
DemonsImageToImageObjectMetric<TFixedImage,TMovingImage,TVirtualImage>
::~DemonsImageToImageObjectMetric()
{
}

/*
 * GetValue
 */
template < class TFixedImage, class TMovingImage, class TVirtualImage >
typename DemonsImageToImageObjectMetric<TFixedImage,TMovingImage,TVirtualImage>::MeasureType
DemonsImageToImageObjectMetric<TFixedImage,TMovingImage,TVirtualImage>
::GetValue() const
{
  itkExceptionMacro("GetValue not yet implemented.");
}

/*
 * GetValueAndDerivative
 */
template < class TFixedImage, class TMovingImage, class TVirtualImage >
void
DemonsImageToImageObjectMetric<TFixedImage,TMovingImage,TVirtualImage>
::GetValueAndDerivative( MeasureType & value, DerivativeType & derivative) const
{
  // This starts threading, and will iterate over virtual image region and
  // call GetValueAndDerivativeProcessPoint.
  this->GetValueAndDerivativeThreadedExecute( derivative );

  // Sums up results from each thread, and optionally averages them.
  // Derivative results are written directly to \c derivative.
  this->GetValueAndDerivativeThreadedPostProcess( true /*doAverage*/ );

  value = this->GetValueResult();
}

/** This function computes the local voxel-wise contribution of
 *  the metric to the global integral of the metric/derivative.
 */
template < class TFixedImage, class TMovingImage, class TVirtualImage >
bool
DemonsImageToImageObjectMetric<TFixedImage,TMovingImage,TVirtualImage>
::GetValueAndDerivativeProcessPoint(
                    const VirtualPointType &,
                    const FixedImagePointType &,
                    const FixedImagePixelType &        fixedImageValue,
                    const FixedImageGradientType &,
                    const MovingImagePointType &       mappedMovingPoint,
                    const MovingImagePixelType &       movingImageValue,
                    const MovingImageGradientType &    movingImageGradient,
                    MeasureType &                      metricValueReturn,
                    DerivativeType &                   localDerivativeReturn,
                    const ThreadIdType                 threadID) const
{
  /** Only the voxelwise contribution given the point pairs. */
  FixedImagePixelType diff = fixedImageValue - movingImageValue;
  metricValueReturn =
    vcl_fabs( diff  ) / static_cast<MeasureType>( this->FixedImageDimension );

  /* Use a pre-allocated jacobian object for efficiency */
  typedef typename Superclass::JacobianType &   JacobianReferenceType;
  JacobianReferenceType jacobian = this->m_MovingTransformJacobianPerThread[threadID];

  /** For dense transforms, this returns identity */
  this->m_MovingTransform->ComputeJacobianWithRespectToParameters( mappedMovingPoint, jacobian);

  typedef typename DerivativeType::ValueType    DerivativeValueType;
  DerivativeValueType floatingpointcorrectionresolution = 10000.0;

  for ( unsigned int par = 0; par < this->GetNumberOfLocalParameters(); par++ )
    {
    double sum = 0.0;
    for ( SizeValueType dim = 0; dim < this->MovingImageDimension; dim++ )
      {
      sum += 2.0 * diff * jacobian(dim, par) * movingImageGradient[dim];
      }

    localDerivativeReturn[par] = sum;

    intmax_t test = static_cast<intmax_t>
            ( localDerivativeReturn[par] * floatingpointcorrectionresolution );

    localDerivativeReturn[par] = static_cast<DerivativeValueType>
                                  ( test / floatingpointcorrectionresolution );
    }
  return true;
}

/**
 * Print out internal information about this class
 */
template < class TFixedImage, class TMovingImage, class TVirtualImage  >
void
DemonsImageToImageObjectMetric<TFixedImage,TMovingImage,TVirtualImage>
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}

/**
 * Initialize
 */
template <class TFixedImage, class TMovingImage, class TVirtualImage>
void
DemonsImageToImageObjectMetric<TFixedImage,TMovingImage,TVirtualImage>
::Initialize(void) throw ( ExceptionObject )
{
  this->Superclass::Initialize();
}

} // end namespace itk


#endif
