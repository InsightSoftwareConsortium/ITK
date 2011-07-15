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
#ifndef __itkBSplineControlPointImageFunction_hxx
#define __itkBSplineControlPointImageFunction_hxx

#include "itkBSplineControlPointImageFunction.h"

#include "itkImageRegionIteratorWithIndex.h"
#include "itkLBFGSBOptimizer.h"

/*
 *
 * This code was contributed in the Insight Journal paper:
 * "N-D C^k B-Spline Scattered Data Approximation"
 * by Nicholas J. Tustison, James C. Gee
 * http://hdl.handle.net/1926/140
 * http://www.insight-journal.org/browse/publication/57
 *
 */

namespace itk
{

template<class TControlPointLattice>
BSplineParametricDistanceCostFunction<TControlPointLattice>
::BSplineParametricDistanceCostFunction()
{
  this->m_ControlPointLattice = NULL;

  this->m_DataPoint = NumericTraits<
      typename TControlPointLattice::PixelType >::Zero;

  this->m_Origin.Fill( 0.0 );
  this->m_Spacing.Fill( 1.0 );
  this->m_Size.Fill( 0 );
  this->m_CloseDimension.Fill( 0 );
  this->m_SplineOrder.Fill( 3 );
}

template<class TControlPointLattice>
BSplineParametricDistanceCostFunction<TControlPointLattice>
::~BSplineParametricDistanceCostFunction()
{
}

template<class TControlPointLattice>
void
BSplineParametricDistanceCostFunction<TControlPointLattice>
::PrintSelf( std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "ControlPointLattice: " << m_ControlPointLattice << std::endl;
  os << indent << "Origin" << m_Origin << std::endl;
  os << indent << "Spacing" << m_Spacing << std::endl;
  os << indent << "Size" << m_Size << std::endl;
  os << indent << "SplineOrder" << m_SplineOrder << std::endl;
  os << indent << "CloseDimension" << m_CloseDimension << std::endl;
  os << indent << "DataPoint" << m_DataPoint << std::endl;
}

template<class TControlPointLattice>
typename BSplineParametricDistanceCostFunction<TControlPointLattice>::MeasureType
BSplineParametricDistanceCostFunction<TControlPointLattice>
::GetValue( const ParametersType & parameters ) const
{
  typename TControlPointLattice::PointType point;

  for( unsigned int d = 0; d < ParametricDimension; d++ )
    {
    point[d] = parameters[d];
    if( this->m_CloseDimension[d] )
      {
      MeasureType sign = -1.0;
      if( point[d] < 0.0 )
        {
        sign = 1.0;
        }
      while( point[d] < 0 || point[d] >= 1.0 )
        {
        point[d] += sign;
        }
      }
    }

  typedef BSplineControlPointImageFunction<TControlPointLattice>
    BSplineFilterType;
  typename BSplineFilterType::Pointer bspliner = BSplineFilterType::New();
  bspliner->SetOrigin( this->m_Origin );
  bspliner->SetSpacing( this->m_Spacing );
  bspliner->SetSize( this->m_Size );
  bspliner->SetSplineOrder( this->m_SplineOrder );
  bspliner->SetCloseDimension( this->m_CloseDimension );
  bspliner->SetInputImage( this->m_ControlPointLattice );

  typename TControlPointLattice::PixelType value = bspliner->Evaluate( point );

  MeasureType metric = 0.0;
  for( unsigned int d = 0; d < value.Size(); d++ )
    {
    metric += vnl_math_sqr( value[d] - this->m_DataPoint[d] );
    }

  return metric;
}

template<class TControlPointLattice>
void
BSplineParametricDistanceCostFunction<TControlPointLattice>
::GetDerivative( const ParametersType & parameters,
                 DerivativeType & derivative ) const
{
  typename TControlPointLattice::PointType point;

  for( unsigned int d = 0; d < ParametricDimension; d++ )
    {
    point[d] = parameters[d];
    if( this->m_CloseDimension[d] )
      {
      MeasureType sign = -1.0;
      if( point[d] < 0.0 )
        {
        sign = 1.0;
        }
      while( point[d] < 0 || point[d] >= 1.0 )
        {
        point[d] += sign;
        }
      }
    }

  typedef BSplineControlPointImageFunction<TControlPointLattice>
    BSplineFilterType;
  typename BSplineFilterType::Pointer bspliner = BSplineFilterType::New();
  bspliner->SetOrigin( this->m_Origin );
  bspliner->SetSpacing( this->m_Spacing );
  bspliner->SetSize( this->m_Size );
  bspliner->SetSplineOrder( this->m_SplineOrder );
  bspliner->SetCloseDimension( this->m_CloseDimension );
  bspliner->SetInputImage( this->m_ControlPointLattice );

  typename TControlPointLattice::PixelType value = bspliner->Evaluate( point );
  typename BSplineFilterType::GradientType gradient =
    bspliner->EvaluateGradient( point );

  derivative.SetSize( this->GetNumberOfParameters() );
  derivative.Fill( 0.0 );

  for( unsigned int i = 0; i < gradient.Rows(); i++ )
    {
    for( unsigned int j = 0; j < gradient.Cols(); j++ )
      {
      derivative[j] += ( 2.0 * ( value[i] - this->m_DataPoint[i] ) *
        gradient(i, j) );
      }
    }
}

template<class TControlPointLattice>
unsigned int
BSplineParametricDistanceCostFunction<TControlPointLattice>
::GetNumberOfParameters() const
{
  return ParametricDimension;
}

/*
 * BSplineControlPointImageFunction class definitions
 */

template<class TInputImage, class TCoordRep>
BSplineControlPointImageFunction<TInputImage, TCoordRep>
::BSplineControlPointImageFunction()
{
  this->m_SplineOrder.Fill( 3 );
  this->m_Origin.Fill( 0.0 );
  this->m_Spacing.Fill( 1.0 );
  this->m_Size.Fill( 0 );

  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    this->m_NumberOfControlPoints[i] = ( this->m_SplineOrder[i] + 1 );
    this->m_Kernel[i] = KernelType::New();
    this->m_Kernel[i]->SetSplineOrder( this->m_SplineOrder[i] );
    }
  this->m_KernelOrder0 = KernelOrder0Type::New();
  this->m_KernelOrder1 = KernelOrder1Type::New();
  this->m_KernelOrder2 = KernelOrder2Type::New();
  this->m_KernelOrder3 = KernelOrder3Type::New();

  this->m_CloseDimension.Fill( 0 );

  this->m_NeighborhoodWeightImage = NULL;

  this->m_BSplineEpsilon = vcl_numeric_limits<CoordRepType>::epsilon();
}

template<class TInputImage, class TCoordRep>
BSplineControlPointImageFunction<TInputImage, TCoordRep>
::~BSplineControlPointImageFunction()
{
}

template<class TInputImage, class TCoordRep>
void
BSplineControlPointImageFunction<TInputImage, TCoordRep>
::SetSplineOrder( const unsigned int order )
{
  this->m_SplineOrder.Fill( order );
  this->SetSplineOrder( this->m_SplineOrder );
}

template<class TInputImage, class TCoordRep>
void
BSplineControlPointImageFunction<TInputImage, TCoordRep>
::SetSplineOrder( const ArrayType & order )
{
  itkDebugMacro( "Setting m_SplineOrder to " << order );

  this->m_SplineOrder = order;
  for( int i = 0; i < ImageDimension; i++ )
    {
    if( this->m_SplineOrder[i] == 0 )
      {
      itkExceptionMacro(
        "The spline order in each dimension must be greater than 0" );
      }

    this->m_Kernel[i] = KernelType::New();
    this->m_Kernel[i]->SetSplineOrder( this->m_SplineOrder[i] );
    }
  this->Modified();
}

template<class TInputImage, class TCoordRep>
void
BSplineControlPointImageFunction<TInputImage, TCoordRep>
::SetInputImage( const InputImageType *image )
{
  Superclass::SetInputImage( image );

  for( unsigned int i = 0; i < ImageDimension; i++)
    {
    if( this->m_Size[i] == 0 )
      {
      itkExceptionMacro( "Size must be specified." );
      }
    }

  unsigned int maximumNumberOfSpans = 0;
  for( unsigned int d = 0; d < ImageDimension; d++ )
    {
    unsigned int numberOfSpans = this->m_NumberOfControlPoints[d] -
      this->m_SplineOrder[d];
    if( numberOfSpans > maximumNumberOfSpans )
      {
      maximumNumberOfSpans = numberOfSpans;
      }
    }
  this->m_BSplineEpsilon = 100 * vcl_numeric_limits<CoordRepType>::epsilon();
  while( static_cast<CoordRepType>( maximumNumberOfSpans ) ==
    static_cast<CoordRepType>( maximumNumberOfSpans ) - this->m_BSplineEpsilon )
    {
    this->m_BSplineEpsilon *= 10;
    }

  for( unsigned int i = 0; i < ImageDimension; i++)
    {
    this->m_NumberOfControlPoints[i] =
      this->GetInputImage()->GetLargestPossibleRegion().GetSize()[i];
    }

  typename RealImageType::SizeType size;
  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    size[i] = this->m_SplineOrder[i] + 1;
    }
  this->m_NeighborhoodWeightImage = RealImageType::New();
  this->m_NeighborhoodWeightImage->SetRegions( size );
  this->m_NeighborhoodWeightImage->Allocate();
}

template<class TInputImage, class TCoordRep>
typename BSplineControlPointImageFunction<TInputImage, TCoordRep>
::OutputType
BSplineControlPointImageFunction<TInputImage, TCoordRep>
::EvaluateAtParametricPoint( const PointType &point ) const
{
  PointType params;
  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    params[i] = ( point[i] - this->m_Origin[i] ) /
      ( static_cast<CoordRepType>( this->m_Size[i] - 1 ) * this->m_Spacing[i] );
    }

  return this->Evaluate( params );
}

template<class TInputImage, class TCoordRep>
typename BSplineControlPointImageFunction<TInputImage, TCoordRep>
::OutputType
BSplineControlPointImageFunction<TInputImage, TCoordRep>
::EvaluateAtIndex( const IndexType &idx ) const
{
  PointType params;
  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    params[i] = static_cast<CoordRepType>( idx[i] ) /
      static_cast<CoordRepType>( this->m_Size[i] - 1 );
    }

  return this->Evaluate( params );
}

template<class TInputImage, class TCoordRep>
typename BSplineControlPointImageFunction<TInputImage, TCoordRep>
::OutputType
BSplineControlPointImageFunction<TInputImage, TCoordRep>
::EvaluateAtContinuousIndex( const ContinuousIndexType &idx ) const
{
  PointType params;
  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    params[i] = idx[i] / static_cast<CoordRepType>( this->m_Size[i] - 1 );
    }

  return this->Evaluate( params );
}

template<class TInputImage, class TCoordRep>
typename BSplineControlPointImageFunction<TInputImage, TCoordRep>
::OutputType
BSplineControlPointImageFunction<TInputImage, TCoordRep>
::Evaluate( const PointType &params ) const
{
  vnl_vector<CoordRepType> p( ImageDimension );
  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    p[i] = params[i];
    if( p[i] == NumericTraits<CoordRepType>::One )
      {
      p[i] = NumericTraits<CoordRepType>::One - this->m_BSplineEpsilon;
      }
    if( p[i] < 0.0 || p[i] >= 1.0 )
      {
      itkExceptionMacro( "The specified point " << params
        << " is outside the reparameterized domain [0, 1]." );
      }
    CoordRepType numberOfSpans = static_cast<CoordRepType>(
      this->GetInputImage()->GetLargestPossibleRegion().GetSize()[i] );
    if( !this->m_CloseDimension[i] )
      {
      numberOfSpans -= static_cast<CoordRepType>( this->m_SplineOrder[i] );
      }
    p[i] = static_cast<CoordRepType>( p[i] ) * numberOfSpans;
    }

  vnl_vector<CoordRepType> bsplineWeights[ImageDimension];

  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    bsplineWeights[i].set_size( this->m_SplineOrder[i] + 1 );
    }

  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    for( unsigned int j = 0; j < bsplineWeights[i].size(); j++ )
      {
      CoordRepType u = p[i] - static_cast<CoordRepType>( static_cast<unsigned>(
        p[i] ) + j ) + 0.5 * static_cast<CoordRepType>(
        this->m_SplineOrder[i] - 1 );

      CoordRepType B = 1.0;
      switch( this->m_SplineOrder[i] )
        {
        case 0:
          {
          B = this->m_KernelOrder0->Evaluate( u );
          break;
          }
        case 1:
          {
          B = this->m_KernelOrder1->Evaluate( u );
          break;
          }
        case 2:
          {
          B = this->m_KernelOrder2->Evaluate( u );
          break;
          }
        case 3:
          {
          B = this->m_KernelOrder3->Evaluate( u );
          break;
          }
        default:
          {
          B = this->m_Kernel[i]->Evaluate( u );
          break;
          }
        }
      bsplineWeights[i].put( j, B );
      }
    }

  OutputType data;
  data.Fill( 0.0 );

  ImageRegionIteratorWithIndex<RealImageType> ItW(
    this->m_NeighborhoodWeightImage,
    this->m_NeighborhoodWeightImage->GetLargestPossibleRegion() );
  for( ItW.GoToBegin(); !ItW.IsAtEnd(); ++ItW )
    {
    CoordRepType B = 1.0;
    typename RealImageType::IndexType idx = ItW.GetIndex();
    for( unsigned int i = 0; i < ImageDimension; i++ )
      {
      B *= bsplineWeights[i].get( idx[i] );

      idx[i] += static_cast<unsigned int>( p[i] );
      if( this->m_CloseDimension[i] )
        {
        idx[i] %=
          this->GetInputImage()->GetLargestPossibleRegion().GetSize()[i];
        }
      }
    if( this->GetInputImage()->GetLargestPossibleRegion().IsInside( idx ) )
      {
      PixelType val = this->GetInputImage()->GetPixel( idx );
      val *= B;
      data += val;
      }
    }
  return data;
}

template<class TInputImage, class TCoordRep>
typename BSplineControlPointImageFunction<TInputImage, TCoordRep>
::GradientType
BSplineControlPointImageFunction<TInputImage, TCoordRep>
::EvaluateGradientAtParametricPoint( const PointType &point ) const
{
  PointType params;
  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    params[i] = ( point[i] - this->m_Origin[i] ) /
      ( static_cast<CoordRepType>( this->m_Size[i] - 1 ) * this->m_Spacing[i] );
    }

  return this->EvaluateGradient( params );
}

template<class TInputImage, class TCoordRep>
typename BSplineControlPointImageFunction<TInputImage, TCoordRep>
::GradientType
BSplineControlPointImageFunction<TInputImage, TCoordRep>
::EvaluateGradientAtIndex( const IndexType &idx ) const
{
  PointType params;
  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    params[i] = static_cast<CoordRepType>( idx[i] ) /
      static_cast<CoordRepType>( this->m_Size[i] - 1 );
    }

  return this->EvaluateGradient( params );
}

template<class TInputImage, class TCoordRep>
typename BSplineControlPointImageFunction<TInputImage, TCoordRep>
::GradientType
BSplineControlPointImageFunction<TInputImage, TCoordRep>
::EvaluateGradientAtContinuousIndex( const ContinuousIndexType &idx ) const
{
  PointType params;
  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    params[i] = idx[i] / static_cast<CoordRepType>( this->m_Size[i] - 1 );
    }

  return this->EvaluateGradient( params );
}

template<class TInputImage, class TCoordRep>
typename BSplineControlPointImageFunction<TInputImage, TCoordRep>
::GradientType
BSplineControlPointImageFunction<TInputImage, TCoordRep>
::EvaluateGradient( const PointType &params ) const
{
  vnl_vector<CoordRepType> p( ImageDimension );
  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    p[i] = params[i];
    if( p[i] == NumericTraits<CoordRepType>::One )
      {
      p[i] = NumericTraits<CoordRepType>::One - this->m_BSplineEpsilon;
      }
    if( p[i] < 0.0 || p[i] >= 1.0 )
      {
      itkExceptionMacro( "The specified point " << params
        << " is outside the reparameterized domain [0, 1]." );
      }
    CoordRepType numberOfSpans = static_cast<CoordRepType>(
      this->GetInputImage()->GetLargestPossibleRegion().GetSize()[i] );
    if( !this->m_CloseDimension[i] )
      {
      numberOfSpans -= static_cast<CoordRepType>( this->m_SplineOrder[i] );
      }
    p[i] = static_cast<CoordRepType>( p[i] ) * numberOfSpans;
    }

  GradientType gradient;
  gradient.SetSize( PixelType::GetNumberOfComponents(), ImageDimension );
  gradient.Fill( 0.0 );

  ImageRegionIteratorWithIndex<RealImageType> ItW(
    this->m_NeighborhoodWeightImage,
    this->m_NeighborhoodWeightImage->GetLargestPossibleRegion() );

  vnl_vector<CoordRepType> bsplineWeights[ImageDimension];

  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    bsplineWeights[i].set_size( this->m_SplineOrder[i] + 1 );
    }

  for( unsigned int k = 0; k < gradient.Cols(); k++ )
    {
    for( unsigned int i = 0; i < ImageDimension; i++ )
      {
      for( unsigned int j = 0; j < bsplineWeights[i].size(); j++ )
        {
        CoordRepType u = p[i] - static_cast<CoordRepType>(
          static_cast<unsigned>( p[i] ) + j ) + 0.5 *
          static_cast<CoordRepType>( this->m_SplineOrder[i] - 1 );

        CoordRepType B = 1.0;
        if( i == k )
          {
          B = this->m_Kernel[i]->EvaluateDerivative( u );
          }
        else
          {
          switch( this->m_SplineOrder[i] )
            {
            case 0:
              {
              B = this->m_KernelOrder0->Evaluate( u );
              break;
              }
            case 1:
              {
              B = this->m_KernelOrder1->Evaluate( u );
              break;
              }
            case 2:
              {
              B = this->m_KernelOrder2->Evaluate( u );
              break;
              }
            case 3:
              {
              B = this->m_KernelOrder3->Evaluate( u );
              break;
              }
            default:
              {
              B = this->m_Kernel[i]->Evaluate( u );
              break;
              }
            }
          }
        bsplineWeights[i].put( j, B );
        }
      }

    for( ItW.GoToBegin(); !ItW.IsAtEnd(); ++ItW )
      {
      CoordRepType B = 1.0;
      typename RealImageType::IndexType idx = ItW.GetIndex();
      for( unsigned int i = 0; i < ImageDimension; i++ )
        {
        B *= bsplineWeights[i].get( idx[i] );

        idx[i] += static_cast<unsigned int>( p[i] );
        if( this->m_CloseDimension[i] )
          {
          idx[i] %=
            this->GetInputImage()->GetLargestPossibleRegion().GetSize()[i];
          }
        }
      if( this->GetInputImage()->GetLargestPossibleRegion().IsInside( idx ) )
        {
        PixelType val = this->GetInputImage()->GetPixel( idx );
        val *= B;
        for( unsigned int i = 0; i < val.Size(); i++ )
          {
          gradient(i, k) += val[i];
          }
        }
      }
    }

  return gradient;
}

template<class TInputImage, class TCoordRep>
typename BSplineControlPointImageFunction<TInputImage, TCoordRep>
::HessianComponentType
BSplineControlPointImageFunction<TInputImage, TCoordRep>
::EvaluateHessianAtParametricPoint( const PointType &point,
  const unsigned int component ) const
{
  PointType params;
  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    params[i] = ( point[i] - this->m_Origin[i] ) /
      ( static_cast<CoordRepType>( this->m_Size[i] - 1 ) * this->m_Spacing[i] );
    }

  return this->EvaluateHessian( params, component );
}

template<class TInputImage, class TCoordRep>
typename BSplineControlPointImageFunction<TInputImage, TCoordRep>
::HessianComponentType
BSplineControlPointImageFunction<TInputImage, TCoordRep>
::EvaluateHessianAtIndex(
  const IndexType &idx, const unsigned int component ) const
{
  PointType params;
  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    params[i] = static_cast<CoordRepType>( idx[i] ) /
      static_cast<CoordRepType>( this->m_Size[i] - 1 );
    }

  return this->EvaluateHessian( params, component );
}

template<class TInputImage, class TCoordRep>
typename BSplineControlPointImageFunction<TInputImage, TCoordRep>
::HessianComponentType
BSplineControlPointImageFunction<TInputImage, TCoordRep>
::EvaluateHessianAtContinuousIndex( const ContinuousIndexType &idx,
  const unsigned int component )  const
{
  PointType params;
  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    params[i] = idx[i] / static_cast<CoordRepType>( this->m_Size[i] - 1 );
    }

  return this->EvaluateHessian( params, component );
}

template<class TInputImage, class TCoordRep>
typename BSplineControlPointImageFunction<TInputImage, TCoordRep>
::HessianComponentType
BSplineControlPointImageFunction<TInputImage, TCoordRep>
::EvaluateHessian(
  const PointType &params, const unsigned int component )  const
{
  vnl_vector<CoordRepType> p( ImageDimension );
  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    p[i] = params[i];
    if( p[i] == NumericTraits<CoordRepType>::One )
      {
      p[i] = NumericTraits<CoordRepType>::One - this->m_BSplineEpsilon;
      }
    if( p[i] < 0.0 || p[i] >= 1.0 )
      {
      itkExceptionMacro( "The specified point " << params
        << " is outside the reparameterized domain [0, 1]." );
      }
    CoordRepType numberOfSpans = static_cast<CoordRepType>(
      this->GetInputImage()->GetLargestPossibleRegion().GetSize()[i] );
    if( !this->m_CloseDimension[i] )
      {
      numberOfSpans -= static_cast<CoordRepType>( this->m_SplineOrder[i] );
      }
    p[i] = static_cast<CoordRepType>( p[i] ) * numberOfSpans;
    }

  CoordRepType val;
  HessianComponentType hessian;
  hessian.SetSize( ImageDimension, ImageDimension );
  hessian.Fill( 0.0 );

  ImageRegionIteratorWithIndex<RealImageType> ItW(
    this->m_NeighborhoodWeightImage,
    this->m_NeighborhoodWeightImage->GetLargestPossibleRegion() );

  vnl_vector<CoordRepType> bsplineWeights[ImageDimension];

  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    bsplineWeights[i].set_size( this->m_SplineOrder[i] + 1 );
    }

  for( unsigned int j = 0; j < hessian.Rows(); j++ )
    {
    for( unsigned int k = j; k < hessian.Cols(); k++ )
      {
      for( unsigned int i = 0; i < ImageDimension; i++ )
        {
        for( unsigned int h = 0; h < bsplineWeights[i].size(); h++ )
          {
          CoordRepType u = p[i] - static_cast<CoordRepType>(
            static_cast<unsigned>( p[i] ) + h ) + 0.5 *
            static_cast<CoordRepType>( this->m_SplineOrder[i] - 1 );

          CoordRepType B = 1.0;
          if( i == j && j == k )
            {
            B = this->m_Kernel[i]->EvaluateNthDerivative( u, 2 );
            }
          else if( ( i == j || i == k ) && j != k )
            {
            B = this->m_Kernel[i]->EvaluateDerivative( u );
            }
          else
            {
            switch( this->m_SplineOrder[i] )
              {
              case 0:
                {
                B = this->m_KernelOrder0->Evaluate( u );
                break;
                }
              case 1:
                {
                B = this->m_KernelOrder1->Evaluate( u );
                break;
                }
              case 2:
                {
                B = this->m_KernelOrder2->Evaluate( u );
                break;
                }
              case 3:
                {
                B = this->m_KernelOrder3->Evaluate( u );
                break;
                }
              default:
                {
                B = this->m_Kernel[i]->Evaluate( u );
                break;
                }
              }
            }
          bsplineWeights[i].put( h, B );
          }
        }
      for( ItW.GoToBegin(); !ItW.IsAtEnd(); ++ItW )
        {
        CoordRepType B = 1.0;
        typename RealImageType::IndexType idx = ItW.GetIndex();
        for( unsigned int i = 0; i < ImageDimension; i++ )
          {
          B *= bsplineWeights[i].get( idx[i] );

          idx[i] += static_cast<unsigned int>( p[i] );
          if( this->m_CloseDimension[i] )
            {
            idx[i] %=
              this->GetInputImage()->GetLargestPossibleRegion().GetSize()[i];
            }
          }
        if( this->GetInputImage()->GetLargestPossibleRegion().IsInside( idx ) )
          {
          val = this->GetInputImage()->GetPixel( idx )[component];
          val *= B;
          hessian(k, j) += val;
          }
        }
      }
    }

  // Due to continuity properties, the hessian is symmetric
  for( unsigned int j = 0; j < hessian.Rows(); j++ )
    {
    for( unsigned int k = j; k < hessian.Cols(); k++ )
      {
      hessian(j, k) = hessian(k, j);
      }
    }

  return hessian;
}

template<class TInputImage, class TCoordRep>
void
BSplineControlPointImageFunction<TInputImage, TCoordRep>
::CalculateParametersClosestToDataPoint(
  const OutputType point, PointType &params ) const
{
  typedef BSplineParametricDistanceCostFunction<ControlPointLatticeType>
    CostFunctionType;
  typename CostFunctionType::Pointer costFunction = CostFunctionType::New();

  costFunction->SetControlPointLattice(
    const_cast<ControlPointLatticeType *>( this->GetInputImage() ) );
  costFunction->SetOrigin( this->m_Origin );
  costFunction->SetSpacing( this->m_Spacing );
  costFunction->SetSize( this->m_Size );
  costFunction->SetSplineOrder( this->m_SplineOrder );
  costFunction->SetCloseDimension( this->m_CloseDimension );
  costFunction->SetDataPoint( point );

  /*
   * Scale parameters between [0, 1)
   */
  typename LBFGSBOptimizer::ParametersType initialParameters;
  initialParameters.SetSize( ImageDimension );
  for( unsigned int d = 0; d < ImageDimension; d++ )
    {
    initialParameters[d] = ( params[d] - this->m_Origin[d] ) /
      ( static_cast<CoordRepType>( this->m_Size[d] - 1.0 ) *
      this->m_Spacing[d] );
    }
  typename LBFGSBOptimizer::BoundSelectionType boundSelection;
  boundSelection.SetSize( ImageDimension );
  for( unsigned int d = 0; d < ImageDimension; d++ )
    {
    if( this->m_CloseDimension[d] )
      {
      boundSelection[d] = 0;
      }
    else
      {
      boundSelection[d] = 2;
      }
    }
  typename LBFGSBOptimizer::BoundValueType lowerBounds;
  typename LBFGSBOptimizer::BoundValueType upperBounds;
  lowerBounds.SetSize( ImageDimension );
  upperBounds.SetSize( ImageDimension );
  for( unsigned int d = 0; d < ImageDimension; d++ )
    {
    lowerBounds[d] = 0.0;
    upperBounds[d] = 1.0;
    }

  typename LBFGSBOptimizer::Pointer optimizer = LBFGSBOptimizer::New();
  optimizer->SetMinimize( true );
  optimizer->SetCostFunction( costFunction );
  optimizer->SetInitialPosition( initialParameters );
  optimizer->SetCostFunctionConvergenceFactor( 1e1 );
  optimizer->SetLowerBound( lowerBounds );
  optimizer->SetUpperBound( upperBounds );
  optimizer->SetBoundSelection( boundSelection );
  optimizer->SetProjectedGradientTolerance( 1e-6 );

  optimizer->StartOptimization();

  typename LBFGSBOptimizer::ParametersType finalParameters =
    optimizer->GetCurrentPosition();

  /*
   * Rescale parameters back to original space.
   */
  for( unsigned int d = 0; d < ImageDimension; d++ )
    {
    params[d] = finalParameters[d] * this->m_Spacing[d] *
      static_cast<CoordRepType>( this->m_Size[d] - 1 ) + this->m_Origin[d];
    }
}

template<class TInputImage, class TCoordRep>
void
BSplineControlPointImageFunction<TInputImage, TCoordRep>
::PrintSelf( std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf( os, indent );
  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    this->m_Kernel[i]->Print( os, indent );
    }
  os << indent << "Spline order: " << this->m_SplineOrder << std::endl;
  os << indent << "Close dimension: " << this->m_CloseDimension << std::endl;
  os << indent << "Parametric domain" << std::endl;
  os << indent << "  Origin:    " << this->m_Origin << std::endl;
  os << indent << "  Spacing:   " << this->m_Spacing << std::endl;
  os << indent << "  Size:      " << this->m_Size << std::endl;
}

}  //end namespace itk

#endif
