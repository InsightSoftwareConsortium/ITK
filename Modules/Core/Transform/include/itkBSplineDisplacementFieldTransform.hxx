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
#ifndef __itkBSplineDisplacementFieldTransform_hxx
#define __itkBSplineDisplacementFieldTransform_hxx

/* ***
 * This class isn't ready yet. Include files need module dependencies upate
 * and itkBSplineScatteredDataPointSetToImageFilter.h, include from .h, is
 * still in Review.
 * The test for this class is not yet included in TransformTestDriver. But,
 * since new paradigm creates TransformHeaderTest files automatically, this
 * header gets included and causes compilation errors, so comment out.
 */
#if 0

/* *** */

#include "itkBSplineDisplacementFieldTransform.h"

#include "itkContinuousIndex.h"
#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk
{

/**
 * Constructor
 */
template<class TScalar, unsigned int NDimensions>
BSplineDisplacementFieldTransform<TScalar, NDimensions>::
BSplineDisplacementFieldTransform() : Superclass()
{
  this->m_SplineOrder = 3;
  this->m_NumberOfFittingLevels.Fill( 3 );
  this->m_NumberOfControlPoints.Fill( 4 );

  this->m_CalculateApproximateInverseDisplacementField = false;
}

/**
 * Destructor
 */
template<class TScalar, unsigned int NDimensions>
BSplineDisplacementFieldTransform<TScalar, NDimensions>::
~BSplineDisplacementFieldTransform()
{
}

/**
 * set displacement field and project it onto the space of b-spline transforms
 */
template<class TScalar, unsigned int NDimensions>
void
BSplineDisplacementFieldTransform<TScalar, NDimensions>
::SetDisplacementField( DisplacementFieldType *displacementField )
{
  typename PointSetType::Pointer fieldPoints = PointSetType::New();
  fieldPoints->Initialize();

  ImageRegionConstIteratorWithIndex<DisplacementFieldType>
    It( displacementField, displacementField->GetRequestedRegion() );

  itkDebugMacro( "Extracting points from input displacement field. " )

  // Temporarily set the direction cosine to identity since the B-spline
  // approximation algorithm works in parametric space and not physical
  // space.

  typename DisplacementFieldType::DirectionType identity;
  identity.SetIdentity();

  typename DisplacementFieldType::DirectionType originalDirection =
    displacementField->GetDirection();

  displacementField->SetDirection( identity );

  unsigned int N = 0;
  for( It.GoToBegin(); !It.IsAtEnd(); ++It )
    {
    DisplacementVectorType data = It.Get();

    typename PointSetType::PointType point;
    displacementField->TransformIndexToPhysicalPoint( It.GetIndex(), point );

    fieldPoints->SetPointData( N, data );
    fieldPoints->SetPoint( N, point );
    N++;
    }
  displacementField->SetDirection( originalDirection );

  itkDebugMacro( "Calculating the B-spline displacement field. " );

  ArrayType close;
  close.Fill( false );

  typename BSplineFilterType::Pointer bspliner = BSplineFilterType::New();
  bspliner->SetOrigin( displacementField->GetOrigin() );
  bspliner->SetSpacing( displacementField->GetSpacing() );
  bspliner->SetSize( displacementField->GetLargestPossibleRegion().GetSize() );
  bspliner->SetDirection( displacementField->GetDirection() );
  bspliner->SetNumberOfLevels( this->m_NumberOfFittingLevels );
  bspliner->SetSplineOrder( this->m_SplineOrder );
  bspliner->SetNumberOfControlPoints( this->m_NumberOfControlPoints );
  bspliner->SetCloseDimension( close );
  bspliner->SetInput( fieldPoints );
  bspliner->SetGenerateOutputImage( true );
  bspliner->Update();

  this->m_DisplacementFieldControlPointLattice = bspliner->GetPhiLattice();

  typename DisplacementFieldType::Pointer bsplineDisplacementField =
    bspliner->GetOutput();
  bsplineDisplacementField->DisconnectPipeline();

  Superclass::SetDisplacementField( bsplineDisplacementField );

  if( this->m_CalculateApproximateInverseDisplacementField )
    {
    typename PointSetType::Pointer inverseFieldPoints = PointSetType::New();
    inverseFieldPoints->Initialize();

    typedef typename DisplacementFieldType::PointType
                                                    DisplacementFieldPointType;
    typedef typename DisplacementFieldPointType::CoordRepType     CoordRepType;

    displacementField->SetDirection( identity );

    N = 0;
    for( It.GoToBegin(); !It.IsAtEnd(); ++It )
      {
      DisplacementVectorType data = It.Get();

      typename PointSetType::PointType point;
      displacementField->TransformIndexToPhysicalPoint( It.GetIndex(), point );
      DisplacementFieldPointType inversePoint = point + data;

      ContinuousIndex<CoordRepType> cidx;
      displacementField->TransformPhysicalPointToContinuousIndex(
        inversePoint, cidx );
      if( !displacementField->GetRequestedRegion().IsInside( cidx ) )
        {
        continue;
        }

      inverseFieldPoints->SetPointData( N, -data );
      inverseFieldPoints->SetPoint( N, inversePoint );
      N++;
      }
    displacementField->SetDirection( originalDirection );

    itkDebugMacro( "Calculating the inverse approximation of the "
      << "B-spline displacement field. " );

    typename BSplineFilterType::Pointer
                                  inverseBSpliner = BSplineFilterType::New();
    inverseBSpliner->SetOrigin( displacementField->GetOrigin() );
    inverseBSpliner->SetSpacing( displacementField->GetSpacing() );
    inverseBSpliner->SetSize(
                     displacementField->GetLargestPossibleRegion().GetSize() );
    inverseBSpliner->SetDirection( displacementField->GetDirection() );
    inverseBSpliner->SetNumberOfLevels( this->m_NumberOfFittingLevels );
    inverseBSpliner->SetSplineOrder( this->m_SplineOrder );
    inverseBSpliner->SetNumberOfControlPoints( this->m_NumberOfControlPoints );
    inverseBSpliner->SetCloseDimension( close );
    inverseBSpliner->SetInput( inverseFieldPoints );
    inverseBSpliner->SetGenerateOutputImage( true );
    inverseBSpliner->Update();

    this->m_InverseDisplacementFieldControlPointLattice =
      inverseBSpliner->GetPhiLattice();

    typename DisplacementFieldType::Pointer
                inverseBSplineDisplacementField = inverseBSpliner->GetOutput();
    inverseBSplineDisplacementField->DisconnectPipeline();

    Superclass::SetInverseDisplacementField( inverseBSplineDisplacementField );
    }
}

template <class TScalar, unsigned int NDimensions>
void
BSplineDisplacementFieldTransform<TScalar, NDimensions>::
PrintSelf( std::ostream& os, Indent indent ) const
{
  Superclass::PrintSelf( os,indent );

  std::cout << indent << "B-spline parameters: " << std::endl;
  std::cout << indent << "  spline order = " << this->m_SplineOrder
            << std::endl;
  std::cout << indent << "  number of control points = "
    << this->m_NumberOfControlPoints << std::endl;
  std::cout << indent << "  number of fitting levels = "
    << this->m_NumberOfFittingLevels << std::endl;
}
} // namespace itk

#endif //#if 0

#endif
