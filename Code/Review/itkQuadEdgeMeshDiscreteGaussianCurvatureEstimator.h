/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkQuadEdgeMeshDiscreteGaussianCurvatureEstimator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkQuadEdgeMeshDiscreteGaussianCurvatureEstimator_h
#define __itkQuadEdgeMeshDiscreteGaussianCurvatureEstimator_h

#include "itkQuadEdgeMeshDiscreteCurvatureEstimator.h"
#include <vnl/vnl_math.h>

//#include "itkTriangleHelper.h"

namespace itk
{
/**
 * \class QuadEdgeMeshDiscreteGaussianCurvatureEstimator
 * \brief see the following paper
 * title: Discrete Differential-Geometry Operators for Triangulated 2-Manifolds
 * authors: Mark Meyer, Mathieu Desbrun, Peter Schroder, Alan H. Barr
 * conference: VisMath '02
 * location: Berlin (Germany)
 * \author: Arnaud Gelas, Alexandre Gouaillard
 */
template< class TInputMesh, class TOutputMesh >
class QuadEdgeMeshDiscreteGaussianCurvatureEstimator:
  public QuadEdgeMeshDiscreteCurvatureEstimator< TInputMesh, TOutputMesh >
{
public:
  typedef QuadEdgeMeshDiscreteGaussianCurvatureEstimator Self;
  typedef SmartPointer< Self >                           Pointer;
  typedef SmartPointer< const Self >                     ConstPointer;
  typedef QuadEdgeMeshDiscreteCurvatureEstimator<
    TInputMesh, TOutputMesh >                                   Superclass;

  typedef typename Superclass::InputMeshType    InputMeshType;
  typedef typename Superclass::InputMeshPointer InputMeshPointer;

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
  typedef typename Superclass::TriangleType                  TriangleType;

  /** Run-time type information (and related methods).   */
  itkTypeMacro(QuadEdgeMeshDiscreteGaussianCurvatureEstimator, QuadEdgeMeshDiscreteCurvatureEstimator);

  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro(Self);

#ifdef ITK_USE_CONCEPT_CHECKING
  /** Begin concept checking */
  itkConceptMacro( OutputIsFloatingPointCheck,
                   ( Concept::IsFloatingPoint< OutputCurvatureType > ) );
  /** End concept checking */
#endif

protected:
  QuadEdgeMeshDiscreteGaussianCurvatureEstimator() {}
  ~QuadEdgeMeshDiscreteGaussianCurvatureEstimator() {}

  virtual OutputCurvatureType EstimateCurvature(const OutputPointType & iP)
  {
    OutputMeshPointer output = this->GetOutput();

    OutputQEType *qe = iP.GetEdge();

    if ( qe != 0 )
      {
      OutputQEType *qe_it = qe;
      OutputQEType *qe_it2;

      OutputPointType q0, q1;

      OutputCurvatureType sum_theta = 0.;
      OutputCurvatureType area = 0.;

      do
        {
        // cell_id = qe_it->GetLeft();
        qe_it2 = qe_it->GetOnext();
        q0 = output->GetPoint( qe_it->GetDestination() );
        q1 = output->GetPoint( qe_it2->GetDestination() );

        // Compute Angle;
        sum_theta += static_cast< OutputCurvatureType >(
          TriangleType::ComputeAngle(q0, iP, q1) );
        area += ComputeMixedArea(qe_it, qe_it2);
        qe_it = qe_it2;
        }
      while ( qe_it != qe );

      return ( 2.0 * vnl_math::pi - sum_theta ) / area;
      }

    return 0.;
  }

private:
  QuadEdgeMeshDiscreteGaussianCurvatureEstimator(const Self &); // purposely not
                                                                // implemented
  void operator=(const Self &);                                 // purposely not
                                                                // implemented
};
}

#endif
