#ifndef __itkQuadEdgeMeshDiscreteMeanCurvatureEstimator_h
#define __itkQuadEdgeMeshDiscreteMeanCurvatureEstimator_h

#include "itkQuadEdgeMeshDiscreteCurvatureEstimator.h"
#include "itkQuadEdgeMeshParamMatrixCoefficients.h"

namespace itk
{
/**
 * \class QuadEdgeMeshDiscreteMeanCurvatureEstimator
 * \brief see the following paper
 * title: Discrete Differential-Geometry Operators for Triangulated 2-Manifolds
 * authors: Mark Meyer, Mathieu Desbrun, Peter Schroder, Alan H. Barr
 * conference: VisMath '02
 * location: Berlin (Germany)
 * \author: Arnaud Gelas, Alexandre Gouaillard
*/
template< class TInputMesh, class TOutputMesh >
class QuadEdgeMeshDiscreteMeanCurvatureEstimator :
  public QuadEdgeMeshDiscreteCurvatureEstimator< TInputMesh, TOutputMesh >
{
public:
  typedef QuadEdgeMeshDiscreteMeanCurvatureEstimator Self;
  typedef SmartPointer< Self > Pointer;
  typedef SmartPointer< const Self > ConstPointer;
  typedef QuadEdgeMeshDiscreteCurvatureEstimator< TInputMesh, TOutputMesh >
    Superclass;

  typedef typename Superclass::InputMeshType    InputMeshType;
  typedef typename Superclass::InputMeshPointer InputMeshPointer;

  typedef typename Superclass::OutputMeshType   OutputMeshType;
  typedef typename Superclass::OutputMeshPointer OutputMeshPointer;
  typedef typename Superclass::OutputPointsContainerPointer 
    OutputPointsContainerPointer;
  typedef typename Superclass::OutputPointsContainerIterator
    OutputPointsContainerIterator;
  typedef typename Superclass::OutputPointType OutputPointType;
  typedef typename Superclass::OutputVectorType OutputVectorType;
  typedef typename Superclass::OutputCoordType OutputCoordType;
  typedef typename Superclass::OutputPointIdentifier OutputPointIdentifier;
  typedef typename Superclass::OutputCellIdentifier OutputCellIdentifier;
  typedef typename Superclass::OutputQEType OutputQEType;
  typedef typename Superclass::OutputMeshTraits OutputMeshTraits;
  typedef typename Superclass::OutputCurvatureType OutputCurvatureType;
  
  typedef typename Superclass::TriangleType TriangleType;

  /** Run-time type information (and related methods).   */
  itkTypeMacro( QuadEdgeMeshDiscreteMeanCurvatureEstimator,
      QuadEdgeMeshDiscreteCurvatureEstimator );
  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro( Self );

  typedef ConformalMatrixCoefficients< OutputMeshType > CoefficientType;

protected:
  QuadEdgeMeshDiscreteMeanCurvatureEstimator() : Superclass() {}
  ~QuadEdgeMeshDiscreteMeanCurvatureEstimator() {}

  virtual OutputCurvatureType EstimateCurvature( const OutputPointType& iP )
    {
      OutputMeshPointer output = this->GetOutput();

      OutputQEType* qe = iP.GetEdge( );

      OutputCurvatureType oH( 0. );
      OutputVectorType Laplace;
      Laplace.Fill( 0. );
      
      if( qe != 0 )
        {
        CoefficientType coefficent;

        OutputQEType* qe_it = qe;
        OutputQEType* qe_it2;
        OutputCurvatureType area( 0. );
        OutputPointType q0, q1;
        OutputVectorType u;

        if( qe_it != qe_it->GetOnext() )
        {
          qe_it = qe;
          do
            {
            qe_it2 = qe_it->GetOnext();
            q0 = output->GetPoint( qe_it->GetDestination() );
            Laplace += coefficent( output, qe_it ) * ( iP - q0 );
            area += ComputeMixedArea( qe_it, qe_it2 );
            qe_it = qe_it2;
            } while( qe_it != qe );
          Laplace *= ( area > 1e-6 ? 0.25 /area : 0. );
          oH = Laplace.GetNorm();
        }
      }
      return oH;
    }

private:
  QuadEdgeMeshDiscreteMeanCurvatureEstimator( const Self& );
  void operator = ( const Self& );
};
}
#endif
