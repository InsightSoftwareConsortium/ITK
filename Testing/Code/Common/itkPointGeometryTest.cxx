/**
 *  
 *  This program illustrates the use of Geometric objects
 *
 */


#include "itkVector.h"
#include "itkPoint.h"
#include <vnl/vnl_vector_ref.h>
#include <iostream>

  // Dimension & Type
  const     unsigned int    N = 3;
  typedef   double          ValueType;

  //  Vector & Point Classes
  typedef    itk::Vector< ValueType, N >    VectorType;
  typedef    itk::Point<  ValueType, N >    PointType;



//-------------------------
//
//   Main code
//
//-------------------------
int main() 
{

  VectorType va;
  
  va[0] = 1.0;
  va[1] = 3.0;
  va[2] = 5.0;

  std::cout << "initial vector va = ";
  std::cout << va << std::endl;

  // Tests for Point Type

  PointType   pa;
  pa[0] =  1.0;
  pa[1] =  5.0;
  pa[2] = 11.0;

  std::cout << "initial point pa = ";
  std::cout << pa << std::endl;

  PointType   pb( pa );
  std::cout << "copy constructor pb(pa) = ";
  std::cout << pb << std::endl;

  PointType   pc = pa;
  std::cout << "copy constructor pc=pa  = ";
  std::cout << pc << std::endl;

  PointType   pd = pa + va;
  std::cout << "vector sum pd = pa + va = ";
  std::cout << pd << std::endl;

  pb = pd + va;
  std::cout << "vector sum pb = pd + va = ";
  std::cout << pb << std::endl;
  
  VectorType  diff = pa - pb;
  std::cout << "diff = pa - pb = ";
  std::cout << diff << std::endl;

  pc -= va;
  std::cout << "pc -= va = ";
  std::cout << pc << std::endl;

  pc += va;
  std::cout << "pc += va = ";
  std::cout << pc << std::endl;

  ValueType distance = pc.EuclideanDistanceTo( pb );
  std::cout << "Euclidean distance between pc and pb = ";
  std::cout << distance << std::endl;

  ValueType distance2 = pc.SquaredEuclideanDistanceTo( pb );
  std::cout << "Squared Euclidean distance between pc and pb = ";
  std::cout << distance2 << std::endl;

  vnl_vector_ref< ValueType > vnlVector = pa.Get_vnl_vector();
  std::cout << "vnl_vector = ";
  {
    for( unsigned int i=0; i<N; i++ )
    {
      std::cout << vnlVector[i] << ", ";
    }
    std::cout << std::endl;
  }


  return 0;
}



