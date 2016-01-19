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
#ifndef itkBSplineControlPointImageFunction_hxx
#define itkBSplineControlPointImageFunction_hxx

#include "itkBSplineControlPointImageFunction.h"

#include "itkMath.h"
#include "itkImageRegionIteratorWithIndex.h"

namespace itk
{

template<typename TInputImage, typename TCoordRep>
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

  this->m_NeighborhoodWeightImage = ITK_NULLPTR;

  this->m_BSplineEpsilon = 1e-3;
}

template<typename TInputImage, typename TCoordRep>
BSplineControlPointImageFunction<TInputImage, TCoordRep>
::~BSplineControlPointImageFunction()
{
}

template<typename TInputImage, typename TCoordRep>
void
BSplineControlPointImageFunction<TInputImage, TCoordRep>
::SetSplineOrder( const unsigned int order )
{
  this->m_SplineOrder.Fill( order );
  this->SetSplineOrder( this->m_SplineOrder );
}

template<typename TInputImage, typename TCoordRep>
void
BSplineControlPointImageFunction<TInputImage, TCoordRep>
::SetSplineOrder( const ArrayType & order )
{
  itkDebugMacro( "Setting m_SplineOrder to " << order );

  this->m_SplineOrder = order;
  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    if( this->m_SplineOrder[i] == 0 )
      {
      itkExceptionMacro( "The spline order in each dimension must be greater than 0" );
      }

    this->m_Kernel[i] = KernelType::New();
    this->m_Kernel[i]->SetSplineOrder( this->m_SplineOrder[i] );
    }
  this->Modified();
}

template<typename TInputImage, typename TCoordRep>
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

template<typename TInputImage, typename TCoordRep>
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

template<typename TInputImage, typename TCoordRep>
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

template<typename TInputImage, typename TCoordRep>
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

template<typename TInputImage, typename TCoordRep>
typename BSplineControlPointImageFunction<TInputImage, TCoordRep>
::OutputType
BSplineControlPointImageFunction<TInputImage, TCoordRep>
::Evaluate( const PointType &params ) const
{
  vnl_vector<CoordRepType> p( ImageDimension );
  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    p[i] = params[i];
    if( std::abs( p[i] - NumericTraits<CoordRepType>::OneValue() ) <= this->m_BSplineEpsilon )
      {
      p[i] = NumericTraits<CoordRepType>::OneValue() - this->m_BSplineEpsilon;
      }
    if( p[i] < NumericTraits<RealType>::ZeroValue() && std::abs( p[i] ) <= this->m_BSplineEpsilon )
      {
      p[i] = NumericTraits<RealType>::ZeroValue();
      }

    if( p[i] < NumericTraits<CoordRepType>::ZeroValue() ||
        p[i] >= NumericTraits<CoordRepType>::OneValue() )
      {
      itkExceptionMacro( "The specified point " << params
        << " is outside the reparameterized domain [0, 1)." );
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
        idx[i] %= this->GetInputImage()->GetLargestPossibleRegion().GetSize()[i];
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

template<typename TInputImage, typename TCoordRep>
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

template<typename TInputImage, typename TCoordRep>
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

template<typename TInputImage, typename TCoordRep>
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

template<typename TInputImage, typename TCoordRep>
typename BSplineControlPointImageFunction<TInputImage, TCoordRep>
::GradientType
BSplineControlPointImageFunction<TInputImage, TCoordRep>
::EvaluateGradient( const PointType &params ) const
{
  vnl_vector<CoordRepType> p( ImageDimension );
  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    p[i] = params[i];
    if( std::abs( p[i] - NumericTraits<CoordRepType>::OneValue() ) <= this->m_BSplineEpsilon )
      {
      p[i] = NumericTraits<CoordRepType>::OneValue() - this->m_BSplineEpsilon;
      }
    if( p[i] < NumericTraits<RealType>::ZeroValue() && std::abs( p[i] ) <= this->m_BSplineEpsilon )
      {
      p[i] = NumericTraits<RealType>::ZeroValue();
      }

    if( p[i] < NumericTraits<CoordRepType>::ZeroValue() ||
        p[i] >= NumericTraits<CoordRepType>::OneValue() )
      {
      itkExceptionMacro( "The specified point " << params
        << " is outside the reparameterized domain [0, 1)." );
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

template<typename TInputImage, typename TCoordRep>
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

template<typename TInputImage, typename TCoordRep>
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

template<typename TInputImage, typename TCoordRep>
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

template<typename TInputImage, typename TCoordRep>
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
    if( std::abs( p[i] - NumericTraits<CoordRepType>::OneValue() ) <= this->m_BSplineEpsilon )
      {
      p[i] = NumericTraits<CoordRepType>::OneValue() - this->m_BSplineEpsilon;
      }
    if( p[i] < NumericTraits<RealType>::ZeroValue() && std::abs( p[i] ) <= this->m_BSplineEpsilon )
      {
      p[i] = NumericTraits<RealType>::ZeroValue();
      }

    if( p[i] < NumericTraits<CoordRepType>::ZeroValue() ||
        p[i] >= NumericTraits<CoordRepType>::OneValue() )
      {
      itkExceptionMacro( "The specified point " << params
        << " is outside the reparameterized domain [0, 1)." );
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

template<typename TInputImage, typename TCoordRep>
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
