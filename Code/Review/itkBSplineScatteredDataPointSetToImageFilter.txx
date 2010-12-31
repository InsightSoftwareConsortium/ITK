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
#ifndef __itkBSplineScatteredDataPointSetToImageFilter_txx
#define __itkBSplineScatteredDataPointSetToImageFilter_txx

#include "itkBSplineScatteredDataPointSetToImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageDuplicator.h"
#include "itkCastImageFilter.h"
#include "itkNumericTraits.h"

#include "vnl/vnl_math.h"
#include "vnl/algo/vnl_matrix_inverse.h"
#include "vnl/vnl_vector.h"
#include "vcl_limits.h"

namespace itk
{
/**
 * \author Nicholas J. Tustison
 *
 * Contributed by Nicholas J. Tustison, James C. Gee
 * in the Insight Journal paper:
 * http://hdl.handle.net/1926/140
 */

template< class TInputPointSet, class TOutputImage >
BSplineScatteredDataPointSetToImageFilter< TInputPointSet, TOutputImage >
::BSplineScatteredDataPointSetToImageFilter()
{
  this->m_SplineOrder.Fill(3);
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    this->m_NumberOfControlPoints[i] = ( this->m_SplineOrder[i] + 1 );
    this->m_Kernel[i] = KernelType::New();
    this->m_Kernel[i]->SetSplineOrder(this->m_SplineOrder[i]);
    }
  this->m_KernelOrder0 = KernelOrder0Type::New();
  this->m_KernelOrder1 = KernelOrder1Type::New();
  this->m_KernelOrder2 = KernelOrder2Type::New();
  this->m_KernelOrder3 = KernelOrder3Type::New();

  this->m_CloseDimension.Fill(0);
  this->m_DoMultilevel = false;
  this->m_GenerateOutputImage = true;
  this->m_NumberOfLevels.Fill(1);
  this->m_MaximumNumberOfLevels = 1;

  this->m_PhiLattice = NULL;
  this->m_PsiLattice = PointDataImageType::New();
  this->m_InputPointData = PointDataContainerType::New();
  this->m_OutputPointData = PointDataContainerType::New();

  this->m_PointWeights = WeightsContainerType::New();
  this->m_UsePointWeights = false;

  this->m_BSplineEpsilon = vcl_numeric_limits< RealType >::epsilon();
}

template< class TInputPointSet, class TOutputImage >
BSplineScatteredDataPointSetToImageFilter< TInputPointSet, TOutputImage >
::~BSplineScatteredDataPointSetToImageFilter()
{}

template< class TInputPointSet, class TOutputImage >
void
BSplineScatteredDataPointSetToImageFilter< TInputPointSet, TOutputImage >
::SetSplineOrder(unsigned int order)
{
  this->m_SplineOrder.Fill(order);
  this->SetSplineOrder(this->m_SplineOrder);
}

template< class TInputPointSet, class TOutputImage >
void
BSplineScatteredDataPointSetToImageFilter< TInputPointSet, TOutputImage >
::SetSplineOrder(const ArrayType & order)
{
  itkDebugMacro("Setting m_SplineOrder to " << order);

  this->m_SplineOrder = order;
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    if ( this->m_SplineOrder[i] == 0 )
      {
      itkExceptionMacro(
        "The spline order in each dimension must be greater than 0");
      }

    this->m_Kernel[i] = KernelType::New();
    this->m_Kernel[i]->SetSplineOrder(this->m_SplineOrder[i]);

    if ( this->m_DoMultilevel )
      {
      typename KernelType::MatrixType C;
      C = this->m_Kernel[i]->GetShapeFunctionsInZeroToOneInterval();

      vnl_matrix< RealType > R;
      vnl_matrix< RealType > S;
      R.set_size( C.rows(), C.cols() );
      S.set_size( C.rows(), C.cols() );
      for ( unsigned int j = 0; j < C.rows(); j++ )
        {
        for ( unsigned int k = 0; k < C.cols(); k++ )
          {
          R(j, k) = S(j, k) = static_cast< RealType >( C(j, k) );
          }
        }
      for ( unsigned int j = 0; j < C.cols(); j++ )
        {
        RealType c = vcl_pow(static_cast< RealType >( 2.0 ),
                             static_cast< RealType >( C.cols() ) - j - 1);
        for ( unsigned int k = 0; k < C.rows(); k++ )
          {
          R(k, j) *= c;
          }
        }
      R = R.transpose();
      R.flipud();
      S = S.transpose();
      S.flipud();

      this->m_RefinedLatticeCoefficients[i] =
        ( vnl_svd< RealType >(R).solve(S) ).extract( 2, S.cols() );
      }
    }
  this->Modified();
}

template< class TInputPointSet, class TOutputImage >
void
BSplineScatteredDataPointSetToImageFilter< TInputPointSet, TOutputImage >
::SetNumberOfLevels(unsigned int levels)
{
  this->m_NumberOfLevels.Fill(levels);
  this->SetNumberOfLevels(this->m_NumberOfLevels);
}

template< class TInputPointSet, class TOutputImage >
void
BSplineScatteredDataPointSetToImageFilter< TInputPointSet, TOutputImage >
::SetNumberOfLevels(const ArrayType & levels)
{
  this->m_NumberOfLevels = levels;
  this->m_MaximumNumberOfLevels = 1;
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    if ( this->m_NumberOfLevels[i] == 0 )
      {
      itkExceptionMacro(
        "The number of levels in each dimension must be greater than 0");
      }
    if ( this->m_NumberOfLevels[i] > this->m_MaximumNumberOfLevels )
      {
      this->m_MaximumNumberOfLevels = this->m_NumberOfLevels[i];
      }
    }

  itkDebugMacro("Setting m_NumberOfLevels to "
                << this->m_NumberOfLevels);
  itkDebugMacro("Setting m_MaximumNumberOfLevels to "
                << this->m_MaximumNumberOfLevels);

  if ( this->m_MaximumNumberOfLevels > 1 )
    {
    this->m_DoMultilevel = true;
    }
  else
    {
    this->m_DoMultilevel = false;
    }
  this->SetSplineOrder(this->m_SplineOrder);
  this->Modified();
}

template< class TInputPointSet, class TOutputImage >
void
BSplineScatteredDataPointSetToImageFilter< TInputPointSet, TOutputImage >
::SetPointWeights(WeightsContainerType *weights)
{
  this->m_UsePointWeights = true;
  this->m_PointWeights = weights;
  this->Modified();
}

template< class TInputPointSet, class TOutputImage >
void
BSplineScatteredDataPointSetToImageFilter< TInputPointSet, TOutputImage >
::GenerateData()
{
  /**
   *  Create the output image
   */
  itkDebugMacro("Size: " << this->m_Size);
  itkDebugMacro("Origin: " << this->m_Origin);
  itkDebugMacro("Spacing: " << this->m_Spacing);
  itkDebugMacro("Direction: " << this->m_Direction);
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    if ( this->m_Size[i] == 0 )
      {
      itkExceptionMacro("Size must be specified.");
      }
    }

  this->GetOutput()->SetOrigin(this->m_Origin);
  this->GetOutput()->SetSpacing(this->m_Spacing);
  this->GetOutput()->SetDirection(this->m_Direction);
  this->GetOutput()->SetRegions(this->m_Size);
  this->GetOutput()->Allocate();

  /**
   * Perform some error checking on the input
   */
  if ( this->m_UsePointWeights
       && ( this->m_PointWeights->Size() != this->GetInput()->GetNumberOfPoints() ) )
    {
    itkExceptionMacro(
      "The number of weight points and input points must be equal.");
    }

  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    if ( this->m_NumberOfControlPoints[i] < this->m_SplineOrder[i] + 1 )
      {
      itkExceptionMacro(
        "The number of control points must be greater than the spline order.");
      }
    }

  /**
   * Calculate the appropriate epsilon value.
   */
  unsigned int maximumNumberOfSpans = 0;
  for ( unsigned int d = 0; d < ImageDimension; d++ )
    {
    unsigned int numberOfSpans = this->m_NumberOfControlPoints[d]
                                 - this->m_SplineOrder[d];
    numberOfSpans <<= ( this->m_NumberOfLevels[d] - 1 );
    if ( numberOfSpans > maximumNumberOfSpans )
      {
      maximumNumberOfSpans = numberOfSpans;
      }
    }
  this->m_BSplineEpsilon = 100 * vcl_numeric_limits< RealType >::epsilon();
  while ( static_cast< RealType >( maximumNumberOfSpans ) ==
          static_cast< RealType >( maximumNumberOfSpans ) - this->m_BSplineEpsilon )
    {
    this->m_BSplineEpsilon *= 10;
    }

  this->m_InputPointData->Initialize();
  this->m_OutputPointData->Initialize();
  if ( this->GetInput()->GetNumberOfPoints() > 0 )
    {
    typename PointSetType::PointDataContainer::ConstIterator It;
    It = this->GetInput()->GetPointData()->Begin();
    while ( It != this->GetInput()->GetPointData()->End() )
      {
      if ( !this->m_UsePointWeights )
        {
        this->m_PointWeights->InsertElement(It.Index(), 1.0);
        }
      this->m_InputPointData->InsertElement( It.Index(), It.Value() );
      this->m_OutputPointData->InsertElement( It.Index(), It.Value() );
      ++It;
      }
    }

  this->m_CurrentLevel = 0;
  this->m_CurrentNumberOfControlPoints = this->m_NumberOfControlPoints;

  /**
   * Set up multithread processing to handle generating the
   * control point lattice.
   */
  typename ImageSource< TOutputImage >::ThreadStruct str1;
  str1.Filter = this;

  this->GetMultiThreader()->SetNumberOfThreads( this->GetNumberOfThreads() );
  this->GetMultiThreader()->SetSingleMethod(this->ThreaderCallback, &str1);

  /**
   * Multithread the generation of the control point lattice.
   */
  this->BeforeThreadedGenerateData();
  this->GetMultiThreader()->SingleMethodExecute();
  this->AfterThreadedGenerateData();

  this->UpdatePointSet();

  if ( this->m_DoMultilevel )
    {
    this->m_PsiLattice->SetRegions(
      this->m_PhiLattice->GetLargestPossibleRegion() );
    this->m_PsiLattice->Allocate();
    PointDataType P(0.0);
    this->m_PsiLattice->FillBuffer(P);
    }

  for ( this->m_CurrentLevel = 1;
        this->m_CurrentLevel < this->m_MaximumNumberOfLevels;
        this->m_CurrentLevel++ )
    {
    ImageRegionIterator< PointDataImageType > ItPsi( this->m_PsiLattice,
                                                     this->m_PsiLattice->GetLargestPossibleRegion() );
    ImageRegionIterator< PointDataImageType > ItPhi( this->m_PhiLattice,
                                                     this->m_PhiLattice->GetLargestPossibleRegion() );
    for ( ItPsi.GoToBegin(), ItPhi.GoToBegin();
          !ItPsi.IsAtEnd(); ++ItPsi, ++ItPhi )
      {
      ItPsi.Set( ItPhi.Get() + ItPsi.Get() );
      }
    this->RefineControlPointLattice();

    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      if ( this->m_CurrentLevel < this->m_NumberOfLevels[i] )
        {
        this->m_CurrentNumberOfControlPoints[i] =
          2 * this->m_CurrentNumberOfControlPoints[i] - this->m_SplineOrder[i];
        }
      }

    itkDebugMacro("Current Level = " << this->m_CurrentLevel);
    itkDebugMacro("  Current number of control points = "
                  << this->m_CurrentNumberOfControlPoints);

    RealType avg_p = 0.0;
    RealType totalWeight = 0.0;

    typename PointDataContainerType::Iterator ItIn =
      this->m_InputPointData->Begin();
    typename PointDataContainerType::Iterator ItOut =
      this->m_OutputPointData->Begin();
    while ( ItIn != this->m_InputPointData->End() )
      {
      this->m_InputPointData->InsertElement(
        ItIn.Index(), ItIn.Value() - ItOut.Value() );

      if ( this->GetDebug() )
        {
        RealType weight = this->m_PointWeights->GetElement( ItIn.Index() );
        avg_p += ( ItIn.Value() - ItOut.Value() ).GetNorm() * weight;
        totalWeight += weight;
        }

      ++ItIn;
      ++ItOut;
      }
    if ( totalWeight > 0 )
      {
      itkDebugMacro(
        "The average weighted difference norm of the point set is "
        << avg_p / totalWeight);
      }
    /**
     * Set up multithread processing to handle generating the
     * control point lattice.
     */
    typename ImageSource< TOutputImage >::ThreadStruct str2;
    str2.Filter = this;

    this->GetMultiThreader()->SetNumberOfThreads( this->GetNumberOfThreads() );
    this->GetMultiThreader()->SetSingleMethod(this->ThreaderCallback, &str2);

    /**
     * Multithread the generation of the control point lattice.
     */
    this->BeforeThreadedGenerateData();
    this->GetMultiThreader()->SingleMethodExecute();
    this->AfterThreadedGenerateData();

    this->UpdatePointSet();
    }

  if ( this->m_DoMultilevel )
    {
    ImageRegionIterator< PointDataImageType > ItPsi( this->m_PsiLattice,
                                                     this->m_PsiLattice->GetLargestPossibleRegion() );
    ImageRegionIterator< PointDataImageType > ItPhi( this->m_PhiLattice,
                                                     this->m_PhiLattice->GetLargestPossibleRegion() );
    for ( ItPsi.GoToBegin(), ItPhi.GoToBegin();
          !ItPsi.IsAtEnd(); ++ItPsi, ++ItPhi )
      {
      ItPsi.Set( ItPhi.Get() + ItPsi.Get() );
      }

    typedef ImageDuplicator< PointDataImageType > ImageDuplicatorType;
    typename ImageDuplicatorType::Pointer Duplicator =
      ImageDuplicatorType::New();
    Duplicator->SetInputImage(this->m_PsiLattice);
    Duplicator->Update();
    this->m_PhiLattice = Duplicator->GetOutput();
    this->UpdatePointSet();
    }

  if ( this->m_GenerateOutputImage )
    {
    //this->GenerateOutputImage();
    this->GenerateOutputImageFast();
    }
}

template< class TInputPointSet, class TOutputImage >
void
BSplineScatteredDataPointSetToImageFilter< TInputPointSet, TOutputImage >
::BeforeThreadedGenerateData()
{
  this->m_DeltaLatticePerThread.resize( this->GetNumberOfThreads() );
  this->m_OmegaLatticePerThread.resize( this->GetNumberOfThreads() );
}

template< class TInputPointSet, class TOutputImage >
void
BSplineScatteredDataPointSetToImageFilter< TInputPointSet, TOutputImage >
::ThreadedGenerateData(const RegionType & itkNotUsed(region), int threadId)
{
  /**
   * Ignore the output region as we're only interested in dividing the
   * points among the threads.
   */
  typename RealImageType::RegionType::SizeType size;
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    if ( this->m_CloseDimension[i] )
      {
      size[i] = this->m_CurrentNumberOfControlPoints[i]
                - this->m_SplineOrder[i];
      }
    else
      {
      size[i] = this->m_CurrentNumberOfControlPoints[i];
      }
    }

  this->m_OmegaLatticePerThread[threadId] = RealImageType::New();
  this->m_OmegaLatticePerThread[threadId]->SetRegions(size);
  this->m_OmegaLatticePerThread[threadId]->Allocate();
  this->m_OmegaLatticePerThread[threadId]->FillBuffer(0.0);

  this->m_DeltaLatticePerThread[threadId] = PointDataImageType::New();
  this->m_DeltaLatticePerThread[threadId]->SetRegions(size);
  this->m_DeltaLatticePerThread[threadId]->Allocate();
  this->m_DeltaLatticePerThread[threadId]->FillBuffer(0.0);

  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    size[i] = this->m_SplineOrder[i] + 1;
    }

  typename RealImageType::Pointer w = RealImageType::New();
  w->SetRegions(size);
  w->Allocate();

  typename PointDataImageType::Pointer phi = PointDataImageType::New();
  phi->SetRegions(size);
  phi->Allocate();

  ImageRegionIteratorWithIndex< RealImageType >
  Itw( w, w->GetLargestPossibleRegion() );
  ImageRegionIteratorWithIndex< PointDataImageType >
  Itp( phi, phi->GetLargestPossibleRegion() );

  vnl_vector< RealType > p(ImageDimension);
  vnl_vector< RealType > r(ImageDimension);
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    r[i] = static_cast< RealType >( this->m_CurrentNumberOfControlPoints[i]
                                    - this->m_SplineOrder[i] ) / ( static_cast< RealType >( this->m_Size[i] - 1 )
                                                                   * this->m_Spacing[i] );
    }

  /**
   * Determine which points should be handled by this particular thread.
   */
  int           numberOfThreads = this->GetNumberOfThreads();
  SizeValueType numberOfPointsPerThread = static_cast< SizeValueType >(
    this->GetInput()->GetNumberOfPoints() / numberOfThreads );

  unsigned int start = threadId * numberOfPointsPerThread;
  unsigned int end = start + numberOfPointsPerThread;
  if ( threadId == this->GetNumberOfThreads() - 1 )
    {
    end = this->GetInput()->GetNumberOfPoints();
    }

  for ( unsigned int n = start; n < end; n++ )
    {
    PointType point;
    point.Fill(0.0);

    this->GetInput()->GetPoint(n, &point);

    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      unsigned int totalNumberOfSpans =
        this->m_CurrentNumberOfControlPoints[i] - this->m_SplineOrder[i];

      p[i] = ( point[i] - this->m_Origin[i] ) * r[i];
      if ( vnl_math_abs( p[i] - static_cast< RealType >( totalNumberOfSpans ) )
           <= this->m_BSplineEpsilon )
        {
        p[i] = static_cast< RealType >( totalNumberOfSpans )
               - this->m_BSplineEpsilon;
        }
      if ( p[i] >= static_cast< RealType >( totalNumberOfSpans ) )
        {
        itkExceptionMacro(
          "The reparameterized point component " << p[i]
                                                 <<
          " is outside the corresponding parametric domain of [0, "
                                                 << totalNumberOfSpans << "].");
        }
      }

    RealType w2_sum = 0.0;
    for ( Itw.GoToBegin(); !Itw.IsAtEnd(); ++Itw )
      {
      RealType B = 1.0;
      typename RealImageType::IndexType idx = Itw.GetIndex();
      for ( unsigned int i = 0; i < ImageDimension; i++ )
        {
        RealType u = static_cast< RealType >( p[i]
                                              - static_cast< unsigned >( p[i] ) - idx[i] )
                     + 0.5 * static_cast< RealType >( this->m_SplineOrder[i] - 1 );
        switch ( this->m_SplineOrder[i] )
          {
          case 0:
            {
            B *= this->m_KernelOrder0->Evaluate(u);
            break;
            }
          case 1:
            {
            B *= this->m_KernelOrder1->Evaluate(u);
            break;
            }
          case 2:
            {
            B *= this->m_KernelOrder2->Evaluate(u);
            break;
            }
          case 3:
            {
            B *= this->m_KernelOrder3->Evaluate(u);
            break;
            }
          default:
            {
            B *= this->m_Kernel[i]->Evaluate(u);
            break;
            }
          }
        }
      Itw.Set(B);
      w2_sum += B * B;
      }

    for ( Itp.GoToBegin(), Itw.GoToBegin(); !Itp.IsAtEnd(); ++Itp, ++Itw )
      {
      typename RealImageType::IndexType idx = Itw.GetIndex();
      for ( unsigned int i = 0; i < ImageDimension; i++ )
        {
        idx[i] += static_cast< unsigned >( p[i] );
        if ( this->m_CloseDimension[i] )
          {
          idx[i] %= this->m_DeltaLatticePerThread[threadId]
                    ->GetLargestPossibleRegion().GetSize()[i];
          }
        }
      RealType wc = this->m_PointWeights->GetElement(n);
      RealType t = Itw.Get();
      this->m_OmegaLatticePerThread[threadId]->SetPixel(idx,
                                                        this->m_OmegaLatticePerThread[threadId]->GetPixel(
                                                          idx) + wc * t * t);

      PointDataType data = this->m_InputPointData->GetElement(n);
      data *= ( t / w2_sum );
      Itp.Set(data);
      data *= ( t * t * wc );
      this->m_DeltaLatticePerThread[threadId]->SetPixel(
        idx, this->m_DeltaLatticePerThread[threadId]->GetPixel(idx) + data);
      }
    }
}

template< class TInputPointSet, class TOutputImage >
void
BSplineScatteredDataPointSetToImageFilter< TInputPointSet, TOutputImage >
::AfterThreadedGenerateData()
{
  /**
   * Accumulate all the delta lattice and omega lattice values to
   * calculate the final phi lattice.
   */
  ImageRegionIterator< PointDataImageType > ItD(
    this->m_DeltaLatticePerThread[0],
    this->m_DeltaLatticePerThread[0]->GetLargestPossibleRegion() );
  ImageRegionIterator< RealImageType > ItO(
    this->m_OmegaLatticePerThread[0],
    this->m_OmegaLatticePerThread[0]->GetLargestPossibleRegion() );

  for ( int n = 1; n < this->GetNumberOfThreads(); n++ )
    {
    ImageRegionIterator< PointDataImageType > Itd(
      this->m_DeltaLatticePerThread[n],
      this->m_DeltaLatticePerThread[n]->GetLargestPossibleRegion() );
    ImageRegionIterator< RealImageType > Ito(
      this->m_OmegaLatticePerThread[n],
      this->m_OmegaLatticePerThread[n]->GetLargestPossibleRegion() );

    ItD.GoToBegin();
    ItO.GoToBegin();
    Itd.GoToBegin();
    Ito.GoToBegin();
    while ( !ItD.IsAtEnd() )
      {
      ItD.Set( ItD.Get() + Itd.Get() );
      ItO.Set( ItO.Get() + Ito.Get() );

      ++ItD;
      ++ItO;
      ++Itd;
      ++Ito;
      }
    }

  /**
   * Generate the control point lattice
   */
  typename RealImageType::RegionType::SizeType size;
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    if ( this->m_CloseDimension[i] )
      {
      size[i] = this->m_CurrentNumberOfControlPoints[i] - this->m_SplineOrder[i];
      }
    else
      {
      size[i] = this->m_CurrentNumberOfControlPoints[i];
      }
    }
  this->m_PhiLattice = PointDataImageType::New();
  this->m_PhiLattice->SetRegions(size);
  this->m_PhiLattice->Allocate();
  this->m_PhiLattice->FillBuffer(0.0);

  ImageRegionIterator< PointDataImageType > ItP(
    this->m_PhiLattice, this->m_PhiLattice->GetLargestPossibleRegion() );

  for ( ItP.GoToBegin(), ItO.GoToBegin(), ItD.GoToBegin();
        !ItP.IsAtEnd(); ++ItP, ++ItO, ++ItD )
    {
    PointDataType P;
    P.Fill(0);
    if ( ItO.Get() != 0 )
      {
      P = ItD.Get() / ItO.Get();
      for ( unsigned int i = 0; i < P.Size(); i++ )
        {
        if ( vnl_math_isnan(P[i]) || vnl_math_isinf(P[i]) )
          {
          P[i] = 0;
          }
        }
      ItP.Set(P);
      }
    }
}

template< class TInputPointSet, class TOutputImage >
void
BSplineScatteredDataPointSetToImageFilter< TInputPointSet, TOutputImage >
::RefineControlPointLattice()
{
  ArrayType NumberOfNewControlPoints = this->m_CurrentNumberOfControlPoints;

  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    if ( this->m_CurrentLevel < this->m_NumberOfLevels[i] )
      {
      NumberOfNewControlPoints[i] =
        2 * NumberOfNewControlPoints[i] - this->m_SplineOrder[i];
      }
    }
  typename RealImageType::RegionType::SizeType size;
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    if ( this->m_CloseDimension[i] )
      {
      size[i] = NumberOfNewControlPoints[i] - this->m_SplineOrder[i];
      }
    else
      {
      size[i] = NumberOfNewControlPoints[i];
      }
    }

  typename PointDataImageType::Pointer RefinedLattice =
    PointDataImageType::New();
  RefinedLattice->SetRegions(size);
  RefinedLattice->Allocate();
  PointDataType data;
  data.Fill(0.0);
  RefinedLattice->FillBuffer(data);

  typename PointDataImageType::IndexType idx;
  typename PointDataImageType::IndexType idx_Psi;
  typename PointDataImageType::IndexType tmp;
  typename PointDataImageType::IndexType tmp_Psi;
  typename PointDataImageType::IndexType off;
  typename PointDataImageType::IndexType off_Psi;
  typename PointDataImageType::RegionType::SizeType size_Psi;

  size.Fill(2);
  unsigned int N = 1;
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    N *= ( this->m_SplineOrder[i] + 1 );
    size_Psi[i] = this->m_SplineOrder[i] + 1;
    }

  ImageRegionIteratorWithIndex< PointDataImageType >
  It( RefinedLattice, RefinedLattice->GetLargestPossibleRegion() );

  It.GoToBegin();
  while ( !It.IsAtEnd() )
    {
    idx = It.GetIndex();
    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      if ( this->m_CurrentLevel < this->m_NumberOfLevels[i] )
        {
        idx_Psi[i] = static_cast< unsigned int >( 0.5 * idx[i] );
        }
      else
        {
        idx_Psi[i] = static_cast< unsigned int >( idx[i] );
        }
      }

    for ( unsigned int i = 0; i < ( 2 << ( ImageDimension - 1 ) ); i++ )
      {
      PointDataType sum(0.0);
      PointDataType val(0.0);
      off = this->NumberToIndex(i, size);

      bool OutOfBoundary = false;
      for ( unsigned int j = 0; j < ImageDimension; j++ )
        {
        tmp[j] = idx[j] + off[j];
        if ( tmp[j] >= static_cast< int >( NumberOfNewControlPoints[j] )
             && !this->m_CloseDimension[j] )
          {
          OutOfBoundary = true;
          break;
          }
        if ( this->m_CloseDimension[j] )
          {
          tmp[j] %= RefinedLattice->GetLargestPossibleRegion().GetSize()[j];
          }
        }
      if ( OutOfBoundary )
        {
        continue;
        }

      for ( unsigned int j = 0; j < N; j++ )
        {
        off_Psi = this->NumberToIndex(j, size_Psi);

        bool IsOutOfBoundary = false;
        for ( unsigned int k = 0; k < ImageDimension; k++ )
          {
          tmp_Psi[k] = idx_Psi[k] + off_Psi[k];
          if ( tmp_Psi[k] >=
               static_cast< int >( this->m_CurrentNumberOfControlPoints[k] )
               && !this->m_CloseDimension[k] )
            {
            IsOutOfBoundary = true;
            break;
            }
          if ( this->m_CloseDimension[k] )
            {
            tmp_Psi[k] %=
              this->m_PsiLattice->GetLargestPossibleRegion().GetSize()[k];
            }
          }
        if ( IsOutOfBoundary )
          {
          continue;
          }
        RealType coeff = 1.0;
        for ( unsigned int k = 0; k < ImageDimension; k++ )
          {
          coeff *= this->m_RefinedLatticeCoefficients[k](off[k], off_Psi[k]);
          }
        val = this->m_PsiLattice->GetPixel(tmp_Psi);
        val *= coeff;
        sum += val;
        }
      RefinedLattice->SetPixel(tmp, sum);
      }

    bool IsEvenIndex = false;
    while ( !IsEvenIndex && !It.IsAtEnd() )
      {
      ++It;
      idx = It.GetIndex();
      IsEvenIndex = true;
      for ( unsigned int i = 0; i < ImageDimension; i++ )
        {
        if ( idx[i] % 2 )
          {
          IsEvenIndex = false;
          }
        }
      }
    }

  typedef ImageDuplicator< PointDataImageType > ImageDuplicatorType;
  typename ImageDuplicatorType::Pointer Duplicator = ImageDuplicatorType::New();
  Duplicator->SetInputImage(RefinedLattice);
  Duplicator->Update();
  this->m_PsiLattice = Duplicator->GetOutput();
}

template< class TInputPointSet, class TOutputImage >
void
BSplineScatteredDataPointSetToImageFilter< TInputPointSet, TOutputImage >
::UpdatePointSet()
{
  typename PointDataImageType::Pointer collapsedPhiLattices[ImageDimension + 1];
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    collapsedPhiLattices[i] = PointDataImageType::New();
    collapsedPhiLattices[i]->SetOrigin( this->m_PhiLattice->GetOrigin() );
    collapsedPhiLattices[i]->SetSpacing( this->m_PhiLattice->GetSpacing() );
    collapsedPhiLattices[i]->SetDirection( this->m_PhiLattice->GetDirection() );
    typename PointDataImageType::SizeType size;
    size.Fill(1);
    for ( unsigned int j = 0; j < i; j++ )
      {
      size[j] = this->m_PhiLattice->GetLargestPossibleRegion().GetSize()[j];
      }
    collapsedPhiLattices[i]->SetRegions(size);
    collapsedPhiLattices[i]->Allocate();
    }
  collapsedPhiLattices[ImageDimension] = this->m_PhiLattice;
  ArrayType totalNumberOfSpans;
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    if ( this->m_CloseDimension[i] )
      {
      totalNumberOfSpans[i] =
        this->m_PhiLattice->GetLargestPossibleRegion().GetSize()[i];
      }
    else
      {
      totalNumberOfSpans[i] =
        this->m_PhiLattice->GetLargestPossibleRegion().GetSize()[i]
        - this->m_SplineOrder[i];
      }
    }
  FixedArray< RealType, ImageDimension > U;
  FixedArray< RealType, ImageDimension > currentU;
  currentU.Fill(-1);
  typename PointDataImageType::IndexType startPhiIndex =
    this->m_PhiLattice->GetLargestPossibleRegion().GetIndex();

  typename PointDataContainerType::ConstIterator ItIn;

  ItIn = this->m_InputPointData->Begin();
  while ( ItIn != this->m_InputPointData->End() )
    {
    PointType point;
    point.Fill(0.0);

    this->GetInput()->GetPoint(ItIn.Index(), &point);

    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      U[i] = static_cast< RealType >( totalNumberOfSpans[i] )
             * static_cast< RealType >( point[i] - this->m_Origin[i] )
             / ( static_cast< RealType >( this->m_Size[i] - 1 ) * this->m_Spacing[i] );
      if ( vnl_math_abs( U[i] - static_cast< RealType >( totalNumberOfSpans[i] ) )
           <= this->m_BSplineEpsilon )
        {
        U[i] = static_cast< RealType >( totalNumberOfSpans[i] )
               - this->m_BSplineEpsilon;
        }
      if ( U[i] >= static_cast< RealType >( totalNumberOfSpans[i] ) )
        {
        itkExceptionMacro("The collapse point component " << U[i]
                                                          << " is outside the corresponding parametric domain of [0, "
                                                          << totalNumberOfSpans[i] << "].");
        }
      }
    for ( int i = ImageDimension - 1; i >= 0; i-- )
      {
      if ( U[i] != currentU[i] )
        {
        for ( int j = i; j >= 0; j-- )
          {
          this->CollapsePhiLattice(collapsedPhiLattices[j + 1],
                                   collapsedPhiLattices[j], U[j], j);
          currentU[j] = U[j];
          }
        break;
        }
      }
    this->m_OutputPointData->InsertElement( ItIn.Index(),
                                            collapsedPhiLattices[0]->GetPixel(startPhiIndex) );
    ++ItIn;
    }
}

template< class TInputPointSet, class TOutputImage >
void
BSplineScatteredDataPointSetToImageFilter< TInputPointSet, TOutputImage >
::GenerateOutputImage()
{
  ImageRegionIteratorWithIndex< ImageType >
  It( this->GetOutput(), this->GetOutput()->GetBufferedRegion() );

  for ( It.GoToBegin(); !It.IsAtEnd(); ++It )
    {
    PointDataType data;
    this->EvaluateAtIndex(It.GetIndex(), data);
    It.Set(data);
    }
}

template< class TInputPointSet, class TOutputImage >
void
BSplineScatteredDataPointSetToImageFilter< TInputPointSet, TOutputImage >
::GenerateOutputImageFast()
{
  typename PointDataImageType::Pointer collapsedPhiLattices[ImageDimension + 1];
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    collapsedPhiLattices[i] = PointDataImageType::New();
    collapsedPhiLattices[i]->SetOrigin( this->m_PhiLattice->GetOrigin() );
    collapsedPhiLattices[i]->SetSpacing( this->m_PhiLattice->GetSpacing() );
    collapsedPhiLattices[i]->SetDirection( this->m_PhiLattice->GetDirection() );
    typename PointDataImageType::SizeType size;
    size.Fill(1);
    for ( unsigned int j = 0; j < i; j++ )
      {
      size[j] = this->m_PhiLattice->GetLargestPossibleRegion().GetSize()[j];
      }
    collapsedPhiLattices[i]->SetRegions(size);
    collapsedPhiLattices[i]->Allocate();
    }
  collapsedPhiLattices[ImageDimension] = this->m_PhiLattice;
  ArrayType totalNumberOfSpans;
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    if ( this->m_CloseDimension[i] )
      {
      totalNumberOfSpans[i] =
        this->m_PhiLattice->GetLargestPossibleRegion().GetSize()[i];
      }
    else
      {
      totalNumberOfSpans[i] =
        this->m_PhiLattice->GetLargestPossibleRegion().GetSize()[i]
        - this->m_SplineOrder[i];
      }
    }
  FixedArray< RealType, ImageDimension > U;
  FixedArray< RealType, ImageDimension > currentU;
  currentU.Fill(-1);
  typename ImageType::IndexType startIndex =
    this->GetOutput()->GetRequestedRegion().GetIndex();
  typename PointDataImageType::IndexType startPhiIndex =
    this->m_PhiLattice->GetLargestPossibleRegion().GetIndex();

  ImageRegionIteratorWithIndex< ImageType >
  It( this->GetOutput(), this->GetOutput()->GetRequestedRegion() );
  for ( It.GoToBegin(); !It.IsAtEnd(); ++It )
    {
    typename ImageType::IndexType idx = It.GetIndex();
    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      U[i] = static_cast< RealType >( totalNumberOfSpans[i] )
             * static_cast< RealType >( idx[i] - startIndex[i] )
             / static_cast< RealType >( this->m_Size[i] - 1 );
      if ( vnl_math_abs( U[i] - static_cast< RealType >( totalNumberOfSpans[i] ) )
           <= this->m_BSplineEpsilon )
        {
        U[i] = static_cast< RealType >( totalNumberOfSpans[i] )
               - this->m_BSplineEpsilon;
        }
      if ( U[i] >= static_cast< RealType >( totalNumberOfSpans[i] ) )
        {
        itkExceptionMacro("The collapse point component " << U[i]
                                                          << " is outside the corresponding parametric domain of [0, "
                                                          << totalNumberOfSpans[i] << "].");
        }
      }
    for ( int i = ImageDimension - 1; i >= 0; i-- )
      {
      if ( U[i] != currentU[i] )
        {
        for ( int j = i; j >= 0; j-- )
          {
          this->CollapsePhiLattice(collapsedPhiLattices[j + 1],
                                   collapsedPhiLattices[j], U[j], j);
          currentU[j] = U[j];
          }
        break;
        }
      }
    It.Set( collapsedPhiLattices[0]->GetPixel(startPhiIndex) );
    }
}

template< class TInputPointSet, class TOutputImage >
void
BSplineScatteredDataPointSetToImageFilter< TInputPointSet, TOutputImage >
::CollapsePhiLattice(PointDataImageType *lattice,
                     PointDataImageType *collapsedLattice, RealType u, unsigned int dimension)
{
  ImageRegionIteratorWithIndex< PointDataImageType > It
    ( collapsedLattice, collapsedLattice->GetLargestPossibleRegion() );

  for ( It.GoToBegin(); !It.IsAtEnd(); ++It )
    {
    PointDataType data;
    data.Fill(0.0);
    typename PointDataImageType::IndexType idx = It.GetIndex();
    for ( unsigned int i = 0; i < this->m_SplineOrder[dimension] + 1; i++ )
      {
      idx[dimension] = static_cast< unsigned int >( u ) + i;
      RealType v = u - idx[dimension]
                   + 0.5 * static_cast< RealType >( this->m_SplineOrder[dimension] - 1 );
      RealType B;
      switch ( this->m_SplineOrder[dimension] )
        {
        case 0:
          {
          B = this->m_KernelOrder0->Evaluate(v);
          break;
          }
        case 1:
          {
          B = this->m_KernelOrder1->Evaluate(v);
          break;
          }
        case 2:
          {
          B = this->m_KernelOrder2->Evaluate(v);
          break;
          }
        case 3:
          {
          B = this->m_KernelOrder3->Evaluate(v);
          break;
          }
        default:
          {
          B = this->m_Kernel[dimension]->Evaluate(v);
          break;
          }
        }
      if ( this->m_CloseDimension[dimension] )
        {
        idx[dimension] %=
          lattice->GetLargestPossibleRegion().GetSize()[dimension];
        }
      data += ( lattice->GetPixel(idx) * B );
      }
    It.Set(data);
    }
}

template< class TInputPointSet, class TOutputImage >
void
BSplineScatteredDataPointSetToImageFilter< TInputPointSet, TOutputImage >
::EvaluateAtPoint(PointType point, PointDataType & data)
{
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    point[i] -= this->m_Origin[i];
    point[i] /=
      ( static_cast< RealType >( this->m_Size[i] - 1 ) * this->m_Spacing[i] );
    }
  this->Evaluate(point, data);
}

template< class TInputPointSet, class TOutputImage >
void
BSplineScatteredDataPointSetToImageFilter< TInputPointSet, TOutputImage >
::EvaluateAtIndex(IndexType idx, PointDataType & data)
{
  PointType point;

  this->GetOutput()->TransformIndexToPhysicalPoint(idx, point);
  this->EvaluateAtPoint(point, data);
}

template< class TInputPointSet, class TOutputImage >
void
BSplineScatteredDataPointSetToImageFilter< TInputPointSet, TOutputImage >
::EvaluateAtContinuousIndex(ContinuousIndexType idx, PointDataType & data)
{
  PointType point;

  this->GetOutput()->TransformContinuousIndexToPhysicalPoint(idx, point);
  this->EvaluateAtPoint(point, data);
}

template< class TInputPointSet, class TOutputImage >
void
BSplineScatteredDataPointSetToImageFilter< TInputPointSet, TOutputImage >
::Evaluate(PointType params, PointDataType & data)
{
  vnl_vector< RealType > p(ImageDimension);
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    if ( params[i] == NumericTraits< RealType >::One )
      {
      params[i] = NumericTraits< RealType >::One - this->m_BSplineEpsilon;
      }
    if ( params[i] < 0.0 || params[i] >= 1.0 )
      {
      itkExceptionMacro("The specified point " << params
                                               << " is outside the reparameterized domain [0, 1].");
      }
    p[i] = static_cast< RealType >( params[i] )
           * static_cast< RealType >( this->m_CurrentNumberOfControlPoints[i]
                                      - this->m_SplineOrder[i] );
    }

  typename RealImageType::RegionType::SizeType size;
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    size[i] = this->m_SplineOrder[i] + 1;
    }
  typename RealImageType::Pointer w;
  w = RealImageType::New();
  w->SetRegions(size);
  w->Allocate();

  PointDataType val;
  data.Fill(0.0);

  ImageRegionIteratorWithIndex< RealImageType >
  Itw( w, w->GetLargestPossibleRegion() );

  for ( Itw.GoToBegin(); !Itw.IsAtEnd(); ++Itw )
    {
    RealType B = 1.0;
    typename RealImageType::IndexType idx = Itw.GetIndex();
    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      RealType u = p[i] - static_cast< RealType >( static_cast< unsigned >( p[i] )
                                                   + idx[i] ) + 0.5
                   * static_cast< RealType >( this->m_SplineOrder[i] - 1 );
      switch ( this->m_SplineOrder[i] )
        {
        case 0:
          {
          B *= this->m_KernelOrder0->Evaluate(u);
          break;
          }
        case 1:
          {
          B *= this->m_KernelOrder1->Evaluate(u);
          break;
          }
        case 2:
          {
          B *= this->m_KernelOrder2->Evaluate(u);
          break;
          }
        case 3:
          {
          B *= this->m_KernelOrder3->Evaluate(u);
          break;
          }
        default:
          {
          B *= this->m_Kernel[i]->Evaluate(u);
          break;
          }
        }
      }
    for ( unsigned int i = 0; i < ImageDimension; i++ )
      {
      idx[i] += static_cast< unsigned int >( p[i] );
      if ( this->m_CloseDimension[i] )
        {
        idx[i] %= this->m_PhiLattice->GetLargestPossibleRegion().GetSize()[i];
        }
      }
    if ( this->m_PhiLattice->GetLargestPossibleRegion().IsInside(idx) )
      {
      val = this->m_PhiLattice->GetPixel(idx);
      val *= B;
      data += val;
      }
    }
}

template< class TInputPointSet, class TOutputImage >
void
BSplineScatteredDataPointSetToImageFilter< TInputPointSet, TOutputImage >
::EvaluateGradientAtPoint(PointType point, GradientType & gradient)
{
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    point[i] -= this->m_Origin[i];
    point[i] /=
      ( static_cast< RealType >( this->m_Size[i] - 1 ) * this->m_Spacing[i] );
    }
  this->EvaluateGradient(point, gradient);
}

template< class TInputPointSet, class TOutputImage >
void
BSplineScatteredDataPointSetToImageFilter< TInputPointSet, TOutputImage >
::EvaluateGradientAtIndex(IndexType idx, GradientType & gradient)
{
  PointType point;

  this->GetOutput()->TransformIndexToPhysicalPoint(idx, point);
  this->EvaluateGradientAtPoint(point, gradient);
}

template< class TInputPointSet, class TOutputImage >
void
BSplineScatteredDataPointSetToImageFilter< TInputPointSet, TOutputImage >
::EvaluateGradientAtContinuousIndex(
  ContinuousIndexType idx, GradientType & gradient)
{
  PointType point;

  this->GetOutput()->TransformContinuousIndexToPhysicalPoint(idx, gradient);
  this->EvaluateGradientAtPoint(point, gradient);
}

template< class TInputPointSet, class TOutputImage >
void
BSplineScatteredDataPointSetToImageFilter< TInputPointSet, TOutputImage >
::EvaluateGradient(PointType params, GradientType & gradient)
{
  vnl_vector< RealType > p(ImageDimension);
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    if ( params[i] == NumericTraits< RealType >::One )
      {
      params[i] = NumericTraits< RealType >::One - this->m_BSplineEpsilon;
      }
    if ( params[i] < 0.0 || params[i] >= 1.0 )
      {
      itkExceptionMacro("The specified point " << params
                                               << " is outside the reparameterized domain [0, 1].");
      }
    p[i] = static_cast< RealType >( params[i] )
           * static_cast< RealType >( this->m_CurrentNumberOfControlPoints[i]
                                      - this->m_SplineOrder[i] );
    }

  typename RealImageType::RegionType::SizeType size;
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    size[i] = this->m_SplineOrder[i] + 1;
    }
  typename RealImageType::Pointer w;
  w = RealImageType::New();
  w->SetRegions(size);
  w->Allocate();

  PointDataType val;
  gradient.SetSize(val.Size(), ImageDimension);
  gradient.Fill(0.0);

  ImageRegionIteratorWithIndex< RealImageType >
  Itw( w, w->GetLargestPossibleRegion() );

  for ( unsigned int j = 0; j < gradient.Cols(); j++ )
    {
    for ( Itw.GoToBegin(); !Itw.IsAtEnd(); ++Itw )
      {
      RealType B = 1.0;
      typename RealImageType::IndexType idx = Itw.GetIndex();
      for ( unsigned int i = 0; i < ImageDimension; i++ )
        {
        RealType u = p[i] - static_cast< RealType >( static_cast< unsigned >( p[i] )
                                                     + idx[i] ) + 0.5
                     * static_cast< RealType >( this->m_SplineOrder[i] - 1 );
        if ( j == i )
          {
          B *= this->m_Kernel[i]->EvaluateDerivative(u);
          }
        else
          {
          B *= this->m_Kernel[i]->Evaluate(u);
          }
        }
      for ( unsigned int i = 0; i < ImageDimension; i++ )
        {
        idx[i] += static_cast< unsigned int >( p[i] );
        if ( this->m_CloseDimension[i] )
          {
          idx[i] %= this->m_PhiLattice->GetLargestPossibleRegion().GetSize()[i];
          }
        }
      if ( this->m_PhiLattice->GetLargestPossibleRegion().IsInside(idx) )
        {
        val = this->m_PhiLattice->GetPixel(idx);
        val *= B;
        for ( unsigned int i = 0; i < val.Size(); i++ )
          {
          gradient(i, j) += val[i];
          }
        }
      }
    }
}

/**
 * Standard "PrintSelf" method
 */
template< class TInputPointSet, class TOutputImage >
void
BSplineScatteredDataPointSetToImageFilter< TInputPointSet, TOutputImage >
::PrintSelf(
  std::ostream & os,
  Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  for ( unsigned int i = 0; i < ImageDimension; i++ )
    {
    this->m_Kernel[i]->Print(os, indent);
    }
  os << indent << "B-spline order: "
     << this->m_SplineOrder << std::endl;
  os << indent << "Number Of control points: "
     << this->m_NumberOfControlPoints << std::endl;
  os << indent << "Close dimension: "
     << this->m_CloseDimension << std::endl;
  os << indent << "Number of levels "
     << this->m_NumberOfLevels << std::endl;
}
} // end namespace itk

#endif
