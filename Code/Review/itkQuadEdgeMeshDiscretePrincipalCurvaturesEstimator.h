#ifndef __itkQuadEdgeMeshDiscretePrincipalCurvaturesEstimator_h
#define __itkQuadEdgeMeshDiscretePrincipalCurvaturesEstimator_h

#include "itkQuadEdgeMeshDiscreteCurvatureEstimator.h"
#include "itkQuadEdgeMeshParamMatrixCoefficients.h"

namespace itk
{
/**
 * \class QuadEdgeMeshDiscretePrincipalCurvaturesEstimator
 * \brief
*/
template< class TInputMesh, class TOutputMesh >
class QuadEdgeMeshDiscretePrincipalCurvaturesEstimator :
  public QuadEdgeMeshDiscreteCurvatureEstimator< TInputMesh, TOutputMesh >
{
public:
  typedef QuadEdgeMeshDiscretePrincipalCurvaturesEstimator Self;
  typedef SmartPointer< Self > Pointer;
  typedef SmartPointer< const Self > ConstPointer;
  typedef QuadEdgeMeshDiscreteCurvatureEstimator< TInputMesh, TOutputMesh > Superclass;
  
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
  itkTypeMacro( QuadEdgeMeshDiscretePrincipalCurvaturesEstimator,
    QuadEdgeMeshDiscreteCurvatureEstimator );
    
  typedef ConformalMatrixCoefficients< OutputMeshType > CoefficientType;

protected:
  QuadEdgeMeshDiscretePrincipalCurvaturesEstimator() : Superclass(), 
    m_Gaussian( 0. ), m_Mean( 0. ){}
  ~QuadEdgeMeshDiscretePrincipalCurvaturesEstimator() {}

  OutputCurvatureType m_Gaussian;
  OutputCurvatureType m_Mean;
  
  void ComputeMeanAndGaussianCurvatures( const OutputPointType& iP )
    {
      OutputMeshPointer output = this->GetOutput();

      OutputQEType* qe = iP.GetEdge( );
      m_Mean = 0.;
      m_Gaussian = 0.;
      
      if( qe != 0 )
        {
        OutputVectorType Laplace;
        Laplace.Fill( 0. );
        
        CoefficientType coefficent;

        OutputQEType* qe_it = qe;
        OutputQEType* qe_it2;
        OutputCurvatureType area( 0. ), sum_theta( 0. );
        OutputPointType q0, q1;
        OutputVectorType u;

        if( qe_it != qe_it->GetOnext() )
          {
          qe_it = qe;
          do
            {
            qe_it2 = qe_it->GetOnext();
            q0 = output->GetPoint( qe_it->GetDestination() );
            q1 = output->GetPoint( qe_it2->GetDestination() );
            
            Laplace += coefficent( output, qe_it ) * ( iP - q0 );
            // Compute Angle;
            sum_theta += static_cast< OutputCurvatureType >(
              TriangleType::ComputeAngle( q0, iP, q1 ) );
              
            area += ComputeMixedArea( qe_it, qe_it2 );
            qe_it = qe_it2;
            } while( qe_it != qe );
          area = ( area > 1e-10 ? ( 1. / area ) : 0. );
          Laplace *= 0.25 * area;
          m_Mean = Laplace.GetNorm();
          m_Gaussian = ( 2. * vnl_math::pi - sum_theta ) * area;
          }
        }
    }
    
  virtual OutputCurvatureType ComputeDelta( )
    {
    return  vnl_math_max( 0., m_Mean * m_Mean - m_Gaussian );
    }
    
private:
  QuadEdgeMeshDiscretePrincipalCurvaturesEstimator( const Self& );
  void operator = ( const Self& );
};
}
#endif
