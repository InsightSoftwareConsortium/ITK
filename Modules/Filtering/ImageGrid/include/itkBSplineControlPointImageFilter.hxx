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
#ifndef itkBSplineControlPointImageFilter_hxx
#define itkBSplineControlPointImageFilter_hxx

#include "itkBSplineControlPointImageFilter.h"

#include "itkMath.h"
#include "itkImageDuplicator.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkMath.h"


namespace itk
{

template<typename TInputImage, typename TOutputImage>
BSplineControlPointImageFilter<TInputImage, TOutputImage>
::BSplineControlPointImageFilter() :
  m_DoMultilevel( false ),
  m_MaximumNumberOfLevels( 1 ),
  m_NumberOfLevels( 1 ),
  m_BSplineEpsilon( 1e-3 )
{
  this->m_Size.Fill( 0 );
  this->m_Spacing.Fill( 1.0 );
  this->m_Origin.Fill( 0.0 );
  this->m_Direction.SetIdentity();
  this->m_CloseDimension.Fill( 0 );
  this->m_SplineOrder.Fill( 3 );

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
}

template<typename InputImage, typename TOutputImage>
BSplineControlPointImageFilter<InputImage, TOutputImage>
::~BSplineControlPointImageFilter()
{
}

template<typename TInputImage, typename TOutputImage>
void
BSplineControlPointImageFilter<TInputImage, TOutputImage>
::SetNumberOfLevels( ArrayType levels )
{
  this->m_NumberOfLevels = levels;
  this->m_MaximumNumberOfLevels = 1;
  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    if( this->m_NumberOfLevels[i] == 0 )
      {
      itkExceptionMacro(
        "The number of levels in each dimension must be greater than 0" );
      }
    if( this->m_NumberOfLevels[i] > this->m_MaximumNumberOfLevels )
      {
      this->m_MaximumNumberOfLevels = this->m_NumberOfLevels[i];
      }
    }

  itkDebugMacro( "Setting m_NumberOfLevels to " <<  this->m_NumberOfLevels );
  itkDebugMacro( "Setting m_MaximumNumberOfLevels to " << this->
    m_MaximumNumberOfLevels );

  if( this->m_MaximumNumberOfLevels > 1 )
    {
    this->m_DoMultilevel = true;
    }
  else
    {
    this->m_DoMultilevel = false;
    }
  this->SetSplineOrder( this->m_SplineOrder );
  this->Modified();
}

template<typename TInputImage, typename TOutputImage>
void
BSplineControlPointImageFilter<TInputImage, TOutputImage>
::SetSplineOrder( unsigned int order )
{
  this->m_SplineOrder.Fill( order );
  this->SetSplineOrder( this->m_SplineOrder );
}

template<typename TInputImage, typename TOutputImage>
void
BSplineControlPointImageFilter<TInputImage, TOutputImage>
::SetSplineOrder( ArrayType order )
{
  itkDebugMacro( "Setting m_SplineOrder to " << order );

  this->m_SplineOrder = order;
  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    if( this->m_SplineOrder[i] == 0 )
      {
      itkExceptionMacro(
        "The spline order in each dimension must be greater than 0" );
      }

    this->m_Kernel[i] = KernelType::New();
    this->m_Kernel[i]->SetSplineOrder( this->m_SplineOrder[i] );

    if( this->m_DoMultilevel )
      {
      typename KernelType::MatrixType C;
      C = this->m_Kernel[i]->GetShapeFunctionsInZeroToOneInterval();

      vnl_matrix<RealType> R;
      vnl_matrix<RealType> S;
      R.set_size( C.rows(), C.cols() );
      S.set_size( C.rows(), C.cols() );
      for( unsigned int j = 0; j < C.rows(); j++ )
        {
        for( unsigned int k = 0; k < C.cols(); k++ )
          {
          R(j, k) = S(j, k) = static_cast<RealType>( C(j, k) );
          }
        }
      for( unsigned int j = 0; j < C.cols(); j++ )
        {
        RealType c = std::pow( static_cast<RealType>( 2.0 ),
                              static_cast<RealType>( C.cols()-j-1 ) );
        for( unsigned int k = 0; k < C.rows(); k++)
          {
          R(k, j) *= c;
          }
        }
      R = R.transpose();
      R.flipud();
      S = S.transpose();
      S.flipud();

      this->m_RefinedLatticeCoefficients[i] =
        ( vnl_svd<RealType>( R ).solve( S ) ).extract( 2, S.cols() );
      }
    }
  this->Modified();
}

template<typename TInputImage, typename TOutputImage>
void
BSplineControlPointImageFilter<TInputImage, TOutputImage>
::BeforeThreadedGenerateData()
{
  const TInputImage *inputPtr = this->GetInput();
  TOutputImage *outputPtr = this->GetOutput();

  for( unsigned int i = 0; i < ImageDimension; i++)
    {
    if( this->m_Size[i] == 0 )
      {
      itkExceptionMacro( "Size must be specified." );
      }
    }
  outputPtr->SetOrigin( this->m_Origin );
  outputPtr->SetSpacing( this->m_Spacing );
  outputPtr->SetRegions( this->m_Size );
  outputPtr->SetDirection( this->m_Direction );
  outputPtr->Allocate();

  for( unsigned int i = 0; i < ImageDimension; i++)
    {
    this->m_NumberOfControlPoints[i] =
      inputPtr->GetLargestPossibleRegion().GetSize()[i];
    }
}

template<typename TInputImage, typename TOutputImage>
void
BSplineControlPointImageFilter<TInputImage, TOutputImage>
::ThreadedGenerateData( const OutputImageRegionType & region,
  ThreadIdType itkNotUsed( threadId ) )
{
  const TInputImage *inputPtr = this->GetInput();
  TOutputImage *outputPtr = this->GetOutput();

  typename PointDataImageType::Pointer collapsedPhiLattices[ImageDimension + 1];
  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    collapsedPhiLattices[i] = PointDataImageType::New();
    collapsedPhiLattices[i]->CopyInformation( inputPtr );

    typename PointDataImageType::SizeType size;
    size.Fill( 1 );
    for( unsigned int j = 0; j < i; j++ )
      {
      size[j] = inputPtr->GetLargestPossibleRegion().GetSize()[j];
      }
    collapsedPhiLattices[i]->SetRegions( size );
    collapsedPhiLattices[i]->Allocate();
    }
  typedef ImageDuplicator<ControlPointLatticeType> ImageDuplicatorType;
  typename ImageDuplicatorType::Pointer duplicator = ImageDuplicatorType::New();
  duplicator->SetInputImage( inputPtr );
  duplicator->Update();

  collapsedPhiLattices[ImageDimension] = duplicator->GetModifiableOutput();

  ArrayType totalNumberOfSpans;
  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    if( this->m_CloseDimension[i] )
      {
      totalNumberOfSpans[i] = inputPtr->GetLargestPossibleRegion().GetSize()[i];
      }
    else
      {
      totalNumberOfSpans[i] = inputPtr->GetLargestPossibleRegion().GetSize()[i] -
        this->m_SplineOrder[i];
      }
    }
  FixedArray<RealType, ImageDimension> U;
  FixedArray<RealType, ImageDimension> currentU;
  currentU.Fill( -1 );

  typename OutputImageType::IndexType startIndex =
    outputPtr->GetRequestedRegion().GetIndex();
  typename PointDataImageType::IndexType startPhiIndex =
    inputPtr->GetLargestPossibleRegion().GetIndex();

  RealArrayType epsilon;
  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    RealType r = static_cast<RealType>( this->m_NumberOfControlPoints[i] -
      this->m_SplineOrder[i] ) / ( static_cast<RealType>( this->m_Size[i] - 1 ) *
      this->m_Spacing[i] );
    epsilon[i] = r * this->m_Spacing[i] * this->m_BSplineEpsilon;
    }

  ImageRegionIteratorWithIndex<OutputImageType> It( outputPtr, region );
  for( It.GoToBegin(); !It.IsAtEnd(); ++It )
    {
    typename OutputImageType::IndexType idx = It.GetIndex();
    for( unsigned int i = 0; i < ImageDimension; i++ )
      {
      U[i] = static_cast<RealType>( totalNumberOfSpans[i] ) *
        static_cast<RealType>( idx[i] - startIndex[i] ) /
        static_cast<RealType>( this->m_Size[i] - 1 );
      if( std::abs( U[i] - static_cast<RealType>( totalNumberOfSpans[i] ) ) <= epsilon[i] )
        {
        U[i] = static_cast<RealType>( totalNumberOfSpans[i] ) - epsilon[i];
        }
      if( U[i] < NumericTraits<RealType>::ZeroValue() && std::abs( U[i] ) <= epsilon[i] )
        {
        U[i] = NumericTraits<RealType>::ZeroValue();
        }

      if( U[i] < NumericTraits<RealType>::ZeroValue() ||
          U[i] >= static_cast<RealType>( totalNumberOfSpans[i] ) )
        {
        itkExceptionMacro( "The collapse point component " << U[i]
          << " is outside the corresponding parametric domain of [0, "
          << totalNumberOfSpans[i] << ")." );
        }
      }
    for( int i = ImageDimension - 1; i >= 0; i-- )
      {
      if( Math::NotExactlyEquals(U[i], currentU[i]) )
        {
        for( int j = i; j >= 0; j-- )
          {
          this->CollapsePhiLattice( collapsedPhiLattices[j + 1],
            collapsedPhiLattices[j], U[j], j );
          currentU[j] = U[j];
          }
        break;
        }
      }
    It.Set( collapsedPhiLattices[0]->GetPixel( startPhiIndex ) );
    }
}

template<typename TInputImage, typename TOutputImage>
void
BSplineControlPointImageFilter<TInputImage, TOutputImage>
::CollapsePhiLattice( PointDataImageType *lattice,
  PointDataImageType *collapsedLattice, const RealType u,
  const unsigned int dimension )
{
  ImageRegionIteratorWithIndex< PointDataImageType > It(
    collapsedLattice, collapsedLattice->GetLargestPossibleRegion() );

  for( It.GoToBegin(); !It.IsAtEnd(); ++It )
    {
    PointDataType data;
    data.Fill( 0.0 );
    typename PointDataImageType::IndexType idx = It.GetIndex();
    for( unsigned int i = 0; i < this->m_SplineOrder[dimension] + 1; i++ )
      {
      idx[dimension] = static_cast<unsigned int>( u ) + i;
      RealType v = u - idx[dimension] + 0.5 * static_cast<RealType>(
        this->m_SplineOrder[dimension] - 1 );

      RealType B = 0.0;
      switch( this->m_SplineOrder[dimension] )
        {
        case 0:
          {
          B = this->m_KernelOrder0->Evaluate( v );
          break;
          }
        case 1:
          {
          B = this->m_KernelOrder1->Evaluate( v );
          break;
          }
        case 2:
          {
          B = this->m_KernelOrder2->Evaluate( v );
          break;
          }
        case 3:
          {
          B = this->m_KernelOrder3->Evaluate( v );
          break;
          }
        default:
          {
          B = this->m_Kernel[dimension]->Evaluate( v );
          break;
          }
        }
      if( this->m_CloseDimension[dimension] )
        {
        idx[dimension] %=
          lattice->GetLargestPossibleRegion().GetSize()[dimension];
        }
      data += ( lattice->GetPixel( idx ) * B );
      }
    It.Set( data );
    }
}

template<typename TInputImage, typename TOutputImage>
unsigned int
BSplineControlPointImageFilter<TInputImage, TOutputImage>
::SplitRequestedRegion( unsigned int i, unsigned int num, OutputImageRegionType &splitRegion )
{
  // Get the output pointer
  OutputImageType *outputPtr = this->GetOutput();

  const SizeType requestedRegionSize =
    outputPtr->GetRequestedRegion().GetSize();

  int splitAxis;
  typename TOutputImage::IndexType splitIndex;
  typename TOutputImage::SizeType splitSize;

  // Initialize the splitRegion to the output requested region
  splitRegion = outputPtr->GetRequestedRegion();
  splitIndex = splitRegion.GetIndex();
  splitSize = splitRegion.GetSize();

  // split on the outermost dimension
  splitAxis = outputPtr->GetImageDimension() - 1;

  // determine the actual number of pieces that will be generated
  typename SizeType::SizeValueType range = requestedRegionSize[splitAxis];
  unsigned int valuesPerThread = static_cast<unsigned int>( std::ceil(
    range / static_cast<double>( num ) ) );
  unsigned int maxThreadIdUsed = static_cast<unsigned int>( std::ceil(
    range / static_cast<double>( valuesPerThread ) ) - 1 );

  // Split the region
  if ( i < maxThreadIdUsed )
    {
    splitIndex[splitAxis] += i * valuesPerThread;
    splitSize[splitAxis] = valuesPerThread;
    }
  if ( i == maxThreadIdUsed )
    {
    splitIndex[splitAxis] += i * valuesPerThread;
    // last thread needs to process the "rest" dimension being split
    splitSize[splitAxis] = splitSize[splitAxis] - i * valuesPerThread;
    }

  // set the split region ivars
  splitRegion.SetIndex( splitIndex );
  splitRegion.SetSize( splitSize );

  itkDebugMacro( "Split piece: " << splitRegion );

  return maxThreadIdUsed + 1;
}

template<typename TInputPointImage, typename TOutputImage>
typename BSplineControlPointImageFilter<TInputPointImage, TOutputImage>
::ControlPointLatticeType::Pointer
BSplineControlPointImageFilter<TInputPointImage, TOutputImage>
::RefineControlPointLattice( ArrayType numberOfLevels )
{
  this->SetNumberOfLevels( numberOfLevels );

  typedef ImageDuplicator<ControlPointLatticeType> ImageDuplicatorType;
  typename ImageDuplicatorType::Pointer duplicator = ImageDuplicatorType::New();
  duplicator->SetInputImage( this->GetInput() );
  duplicator->Update();

  typename ControlPointLatticeType::Pointer psiLattice =
    ControlPointLatticeType::New();
  psiLattice = duplicator->GetModifiableOutput();

  for( unsigned int m = 1; m < this->m_MaximumNumberOfLevels; m++ )
    {
    ArrayType numberOfNewControlPoints;
    for( unsigned int i = 0; i < ImageDimension; i++ )
      {
      numberOfNewControlPoints[i] =
        psiLattice->GetLargestPossibleRegion().GetSize()[i];
      }
    for( unsigned int i = 0; i < ImageDimension; i++ )
      {
      if( m < this->m_NumberOfLevels[i] )
        {
        numberOfNewControlPoints[i] =
          2 * numberOfNewControlPoints[i]-this->m_SplineOrder[i];
        }
      }
    typename RealImageType::RegionType::SizeType size;
    for( unsigned int i = 0; i < ImageDimension; i++ )
      {
      if( this->m_CloseDimension[i] )
        {
        size[i] = numberOfNewControlPoints[i] - this->m_SplineOrder[i];
        }
      else
        {
        size[i] = numberOfNewControlPoints[i];
        }
      }

    typename ControlPointLatticeType::Pointer refinedLattice =
      ControlPointLatticeType::New();
    refinedLattice->SetRegions( size );
    refinedLattice->Allocate();
    PixelType data;
    data.Fill( 0.0 );
    refinedLattice->FillBuffer( data );

    typename ControlPointLatticeType::IndexType            idx;
    typename ControlPointLatticeType::IndexType            idxPsi;
    typename ControlPointLatticeType::IndexType            tmp;
    typename ControlPointLatticeType::IndexType            tmpPsi;
    typename ControlPointLatticeType::IndexType            off;
    typename ControlPointLatticeType::IndexType            offPsi;
    typename ControlPointLatticeType::RegionType::SizeType sizePsi;

    size.Fill( 2 );
    unsigned int N = 1;
    for( unsigned int i = 0; i < ImageDimension; i++ )
      {
      N *= ( this->m_SplineOrder[i] + 1 );
      sizePsi[i] = this->m_SplineOrder[i] + 1;
      }

    ImageRegionIteratorWithIndex<ControlPointLatticeType> It(
      refinedLattice, refinedLattice->GetLargestPossibleRegion() );

    const TInputPointImage *input = this->GetInput();

    It.GoToBegin();
    while( !It.IsAtEnd() )
      {
      idx = It.GetIndex();
      for( unsigned int i = 0; i < ImageDimension; i++ )
        {
        if( m < this->m_NumberOfLevels[i] )
          {
          idxPsi[i] = static_cast<unsigned int>( 0.5 * idx[i] );
          }
        else
          {
          idxPsi[i] = static_cast<unsigned int>( idx[i] );
          }
        }
      for( unsigned int i = 0; i < ( 2 << ( ImageDimension - 1 ) ); i++ )
        {
        PixelType sum( 0.0 );

        PixelType val;
        off = this->NumberToIndex( i, size );

        bool outOfBoundary = false;
        for( unsigned int j = 0; j < ImageDimension; j++ )
          {
          tmp[j] = idx[j] + off[j];
          if( tmp[j] >= static_cast<int>( numberOfNewControlPoints[j] ) &&
              !this->m_CloseDimension[j] )
            {
            outOfBoundary = true;
            break;
            }
          if( this->m_CloseDimension[j] )
            {
            tmp[j] %= refinedLattice->GetLargestPossibleRegion().GetSize()[j];
            }
          }
        if( outOfBoundary )
          {
          continue;
          }

        for( unsigned int j = 0; j < N; j++ )
          {
          offPsi = this->NumberToIndex( j, sizePsi );

          bool outOfBoundary2 = false;
          for( unsigned int k = 0; k < ImageDimension; k++ )
            {
            tmpPsi[k] = idxPsi[k] + offPsi[k];
            if( tmpPsi[k] >= static_cast<int>(
              input->GetLargestPossibleRegion().GetSize()[k] ) &&
              !this->m_CloseDimension[k] )
              {
              outOfBoundary2 = true;
              break;
              }
            if( this->m_CloseDimension[k] )
              {
              tmpPsi[k] %= psiLattice->GetLargestPossibleRegion().GetSize()[k];
              }
            }
          if( outOfBoundary2 )
            {
            continue;
            }
          RealType coeff = 1.0;
          for( unsigned int k = 0; k < ImageDimension; k++ )
            {
            coeff *= this->m_RefinedLatticeCoefficients[k]( off[k], offPsi[k] );
            }
          val = psiLattice->GetPixel( tmpPsi );
          val *= coeff;
          sum += val;
          }
        refinedLattice->SetPixel( tmp, sum );
        }

      bool IsEvenIndex = false;
      while( !IsEvenIndex && !It.IsAtEnd() )
        {
        ++It;
        idx = It.GetIndex();
        IsEvenIndex = true;
        for( unsigned int i = 0; i < ImageDimension; i++ )
          {
          if( idx[i] % 2 )
            {
            IsEvenIndex = false;
            }
          }
        }
      }
    typename ImageDuplicatorType::Pointer duplicator2 = ImageDuplicatorType::New();
    duplicator2->SetInputImage( refinedLattice );
    duplicator2->Update();
    psiLattice = duplicator2->GetModifiableOutput();
    }

  // Specify the pose parameters of the control point lattice

  typename PointDataImageType::PointType origin;
  typename PointDataImageType::SpacingType spacing;

  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    RealType domain = this->m_Spacing[i] * static_cast<RealType>( this->m_Size[i] - 1 );

    unsigned int totalNumberOfSpans =
      psiLattice->GetLargestPossibleRegion().GetSize()[i];
    if( !this->m_CloseDimension[i] )
      {
      totalNumberOfSpans -= this->m_SplineOrder[i];
      }

    spacing[i] = domain / static_cast<RealType>( totalNumberOfSpans );

    origin[i] = -0.5 * spacing[i] * ( this->m_SplineOrder[i] - 1 );
    }
  origin = this->m_Direction * origin;

  psiLattice->SetOrigin( origin );
  psiLattice->SetSpacing( spacing );
  psiLattice->SetDirection( this->m_Direction );

  return psiLattice;
}

template<typename TInputImage, typename TOutputImage>
void
BSplineControlPointImageFilter<TInputImage, TOutputImage>
::PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf( os, indent );
  for( unsigned int i = 0; i < ImageDimension; i++ )
    {
    this->m_Kernel[i]->Print( os, indent.GetNextIndent() );
    }
  os << indent << "Spline order: " << this->m_SplineOrder << std::endl;
  os << indent << "Close dimension: " << this->m_CloseDimension << std::endl;
  os << indent << "Parametric domain" << std::endl;
  os << indent << "  Origin:    " << this->m_Origin << std::endl;
  os << indent << "  Spacing:   " << this->m_Spacing << std::endl;
  os << indent << "  Size:      " << this->m_Size << std::endl;
  os << indent << "  Direction: " << this->m_Direction << std::endl;
}

}  //end namespace itk

#endif
