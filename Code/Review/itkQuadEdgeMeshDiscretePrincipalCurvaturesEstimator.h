/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshDiscretePrincipalCurvaturesEstimator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkQuadEdgeMeshDiscretePrincipalCurvaturesEstimator_h
#define __itkQuadEdgeMeshDiscretePrincipalCurvaturesEstimator_h

#include "itkQuadEdgeMeshDiscreteCurvatureEstimator.h"
#include "itkQuadEdgeMeshParamMatrixCoefficients.h"

namespace itk
{
/**
 * \class QuadEdgeMeshDiscretePrincipalCurvaturesEstimator
 *
 * \brief  FIXME   add documentation here
 *
 */
template< class TInputMesh, class TOutputMesh >
class QuadEdgeMeshDiscretePrincipalCurvaturesEstimator:
  public QuadEdgeMeshDiscreteCurvatureEstimator< TInputMesh, TOutputMesh >
{
public:
  typedef QuadEdgeMeshDiscretePrincipalCurvaturesEstimator Self;
  typedef SmartPointer< Self >                             Pointer;
  typedef SmartPointer< const Self >                       ConstPointer;
  typedef QuadEdgeMeshDiscreteCurvatureEstimator<
    TInputMesh, TOutputMesh >                                   Superclass;

  typedef typename Superclass::InputMeshType                 InputMeshType;
  typedef typename Superclass::InputMeshPointer              InputMeshPointer;
  typedef typename Superclass::OutputMeshType                OutputMeshType;
  typedef typename Superclass::OutputMeshPointer             OutputMeshPointer;
  typedef typename Superclass::OutputPointsContainerPointer  OutputPointsContainerPointer;
  typedef typename Superclass::OutputPointsContainerIterator OutputPointsContainerIterator;
  typedef typename Superclass::OutputPointType               OutputPointType;
  typedef typename Superclass::OutputVectorType              OutputVectorType;
  typedef typename Superclass::OutputCoordType               OutputCoordType;
  typedef typename Superclass::OutputPointIdentifier         OutputPointIdentifier;
  typedef typename Superclass::OutputCellIdentifier          OutputCellIdentifier;
  typedef typename Superclass::OutputQEType                  OutputQEType;
  typedef typename Superclass::OutputMeshTraits              OutputMeshTraits;
  typedef typename Superclass::OutputCurvatureType           OutputCurvatureType;

  typedef typename Superclass::TriangleType TriangleType;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(QuadEdgeMeshDiscretePrincipalCurvaturesEstimator, QuadEdgeMeshDiscreteCurvatureEstimator);

  typedef ConformalMatrixCoefficients< OutputMeshType > CoefficientType;

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro( OutputIsFloatingPointCheck,
                   ( Concept::IsFloatingPoint< OutputCurvatureType > ) );
  /** End concept checking */
#endif

protected:
  QuadEdgeMeshDiscretePrincipalCurvaturesEstimator():
    m_Gaussian(0.0), m_Mean(0.0){}
  ~QuadEdgeMeshDiscretePrincipalCurvaturesEstimator() {}

  OutputCurvatureType m_Gaussian;
  OutputCurvatureType m_Mean;

  void ComputeMeanAndGaussianCurvatures(const OutputPointType & iP)
  {
    OutputMeshPointer output = this->GetOutput();

    OutputQEType *qe = iP.GetEdge();

    m_Mean = 0.;
    m_Gaussian = 0.;

    if ( qe != 0 )
      {
      OutputVectorType Laplace;
      Laplace.Fill(0.);

      OutputQEType *qe_it = qe;

      OutputCurvatureType area(0.), sum_theta(0.);

      if ( qe_it != qe_it->GetOnext() )
        {
        qe_it = qe;
        OutputQEType *qe_it2;

        OutputPointType  q0, q1;
        OutputVectorType face_normal;

        OutputVectorType normal;
        normal.Fill(0.);

        OutputCurvatureType temp_area;
        OutputCoordType     temp_coeff;

        CoefficientType coefficent;

        do
          {
          qe_it2 = qe_it->GetOnext();
          q0 = output->GetPoint( qe_it->GetDestination() );
          q1 = output->GetPoint( qe_it2->GetDestination() );

          temp_coeff = coefficent(output, qe_it);
          Laplace += temp_coeff * ( iP - q0 );

          // Compute Angle;
          sum_theta += static_cast< OutputCurvatureType >(
            TriangleType::ComputeAngle(q0, iP, q1) );

          temp_area = ComputeMixedArea(qe_it, qe_it2);
          area += temp_area;

          face_normal = TriangleType::ComputeNormal(q0, iP, q1);
          normal += face_normal;

          qe_it = qe_it2;
          }
        while ( qe_it != qe );

        if ( area > 1e-10 )
          {
          area = 1. / area;
          Laplace *= 0.25 * area;
          m_Mean = Laplace * normal;
          m_Gaussian = ( 2. * vnl_math::pi - sum_theta ) * area;
          }
        }
      }
  }

  virtual OutputCurvatureType ComputeDelta()
  {
    return vnl_math_max( static_cast<OutputCurvatureType>( 0. ),
                         m_Mean * m_Mean - m_Gaussian );
  }

private:
  QuadEdgeMeshDiscretePrincipalCurvaturesEstimator(const Self &); // purposely
                                                                  // not
                                                                  // implemented
  void operator=(const Self &);                                   // purposely
                                                                  // not
                                                                  // implemented
};
}

#endif
