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


// Functions for printing
void Print(const VectorType & vector )
{
    for( unsigned int i=0; i<VectorType::VectorDimension; i++ )
    {
      std::cout << vector[i] << ", ";
    }
    std::cout << std::endl;
}

void Print(const PointType & point )
{
    for( unsigned int i=0; i<PointType::PointDimension; i++ )
    {
      std::cout << point[i] << ", ";
    }
    std::cout << std::endl;
}


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
  Print( va );

  // Tests for Point Type

  PointType   pa;
  pa[0] =  1.0;
  pa[1] =  5.0;
  pa[2] = 11.0;

  std::cout << "initial point pa = ";
  Print( pa );

  PointType   pb( pa );
  std::cout << "copy constructor pb(pa) = ";
  Print( pb );

  PointType   pc = pa;
  std::cout << "copy constructor pc=pa  = ";
  Print( pc );

  PointType   pd = pa + va;
  std::cout << "vector sum pd = pa + va = ";
  Print( pd );

  pb = pd + va;
  std::cout << "vector sum pb = pd + va = ";
  Print( pb );
  
  VectorType  diff = pa - pb;
  std::cout << "diff = pa - pb = ";
  Print( diff );

  pc -= va;
  std::cout << "pc -= va = ";
  Print( pc );

  pc += va;
  std::cout << "pc += va = ";
  Print( pc );


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



