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
  std::cout << va << std::endl;

  VectorType vb;
  
  vb[0] = 1.0;
  vb[1] = 3.0;
  vb[2] = 5.0;

  std::cout << "vb = (1,3,5)   = ";
  std::cout << vb << std::endl;

  VectorType   vc  =  vb - va;
  std::cout << "vc  =  vb - va  = ";
  std::cout << vc << std::endl;

  VectorType   vd  =  va * 5.0;
  std::cout << "vd  =  va * 5.0 = ";
  std::cout << vd << std::endl;

  VectorType   ve  =  vd / 5.0;
  std::cout << "ve  =  vd * 5.0 = ";
  std::cout << ve << std::endl;

  vd += va;
  std::cout << "vd  +=  va      = ";
  std::cout << vd << std::endl;

  ve -= vb;
  std::cout << "ve  -=  vb      = ";
  std::cout << ve << std::endl;

  VectorType   vh  =  vb;
  std::cout << "vh   =  vb      = ";
  std::cout << vh << std::endl;


  VectorType   vg( va );
  std::cout << "vg( va )        = ";
  std::cout << vg << std::endl;


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



