#ifndef __itkQuadEdgeMeshDiscreteCurvatureTensorEstimator_h
#define __itkQuadEdgeMeshDiscreteCurvatureTensorEstimator_h

namespace itk
{
/**
 * \class QuadEdgeMeshDiscreteCurvatureTensorEstimator
 * \brief
*/
template< class TInputMesh, class TOutputMesh >
class QuadEdgeMeshDiscreteCurvatureTensorEstimator :
  public QuadEdgeMeshToQuadEdgeMeshFilter< TInputMesh, TOutputMesh >
{
public:
  typedef QuadEdgeMeshDiscreteCurvatureTensorEstimator Self;
  typedef SmartPointer< Self > Pointer;
  typedef SmartPointer< const Self > ConstPointer;
  typedef QuadEdgeMeshToQuadEdgeMeshFilter Superclass;
  /** Run-time type information (and related methods).   */
  itkTypeMacro( QuadEdgeMeshDiscreteCurvatureTensorEstimator,
    QuadEdgeMeshToQuadEdgeMeshFilter );
  /** New macro for creation of through a Smart Pointer   */
  itkNewMacro( Self );

protected:
  QuadEdgeMeshDiscreteCurvatureTensorEstimator() : Superclass() {}
  ~QuadEdgeMeshDiscreteCurvatureTensorEstimator() {}

  ///TODO to be implemented  
  virtual void GenerateData()
  {
    
  }
private:
  QuadEdgeMeshDiscreteCurvatureTensorEstimator( const Self& );
  void operator = ( const Self& );
};
}
#endif
