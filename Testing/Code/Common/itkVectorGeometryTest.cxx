/**
 *  
 *  This program illustrates the use of Geometric objects
 *
 */


#include "itkVector.h"
#include <vnl/vnl_vector_ref.h>
#include <iostream>

  // Dimension & Type
  const     unsigned int    N = 3;
  typedef   double          ValueType;

  //  Vector type
  typedef    itk::Vector< ValueType, N >    VectorType;


// Function for printing
void Print(const VectorType & vector )
{
    for( unsigned int i=0; i<VectorType::VectorDimension; i++ )
    {
      std::cout << vector[i] << ", ";
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
  va[1] = 2.0;
  va[2] = 7.0;

  std::cout << "va = { 1.0, 2.0, 7.0 } = ";
  Print( va );

  VectorType vb;
  
  vb[0] = 1.0;
  vb[1] = 3.0;
  vb[2] = 5.0;

  std::cout << "vb = (1,3,5)   = ";
  Print( vb );

  VectorType   vc  =  vb - va;
  std::cout << "vc  =  vb - va  = ";
  Print( vc );

  VectorType   vd  =  va * 5.0;
  std::cout << "vd  =  va * 5.0 = ";
  Print( vd );

  VectorType   ve  =  vd / 5.0;
  std::cout << "ve  =  vd * 5.0 = ";
  Print( ve );

  vd += va;
  std::cout << "vd  +=  va      = ";
  Print( vd );

  ve -= vb;
  std::cout << "ve  -=  vb      = ";
  Print( ve );

  VectorType   vh  =  vb;
  std::cout << "vh   =  vb      = ";
  Print( vh );


  VectorType   vg( va );
  std::cout << "vg( va )        = ";
  Print( vg );


  ValueType norm2 = vg.GetSquaredNorm();
  std::cout << "vg squared norm = ";
  std::cout << norm2 << std::endl;

  ValueType norm  = vg.GetNorm();
  std::cout << "vg norm = ";
  std::cout << norm << std::endl;


  // Test for vnl interface
  vnl_vector_ref< ValueType > vnlVector = va.Get_vnl_vector();
  {
    std::cout << "vnl_vector = va ";
    for( unsigned int i=0; i<N; i++ )
    {
      std::cout << vnlVector[i] << ", ";
    }
    std::cout << std::endl;
  }



  return 0;
}



