/**
 *  
 *  This program illustrates the use of Versors
 *  
 *  Versors are Unit Quaternions used to represent
 *  rotations. 
 *
 */


#include "itkVersor.h"
#include <iostream>



//-------------------------
//
//   Main code
//
//-------------------------
int main() 
{

  typedef   double          ValueType;

  //  Versor type
  typedef    itk::Versor< ValueType >    VersorType;


  //  Vector type
  typedef    VersorType::VectorType      VectorType;


  const ValueType epsilon = 1e-15;


  {
    std::cout << "Test default constructor... ";
    VersorType qa;
    if( fabs(qa.GetX()) > epsilon ) 
      {
      std::cout << "Error ! ";
      return EXIT_FAILURE;
      } 
    if( fabs(qa.GetY()) > epsilon ) 
      {
      std::cout << "Error ! ";
      return EXIT_FAILURE;
      } 
    if( fabs(qa.GetZ()) > epsilon ) 
      {
      std::cout << "Error ! ";
      return EXIT_FAILURE;
      } 
    if( fabs(qa.GetW()-1.0) > epsilon ) 
      {
      std::cout << "Error ! ";
      return EXIT_FAILURE;
      } 
    std::cout << " PASSED !" << std::endl;
  }

  {
    std::cout << "Test for setting Axis and Angle...";
    VersorType qa;
    VectorType xa;
    xa[0] = 2.5;
    xa[1] = 1.5;
    xa[2] = 0.5;
    ValueType angle = atan(1.0)/3.0; // 15 degrees in radians
    
    qa.Set( xa, angle );
        
    xa.Normalize();

    ValueType cosangle = cos( angle / 2.0 );
    ValueType sinangle = sin( angle / 2.0 );

    VectorType xb;

    xb =  xa * sinangle;

    if( fabs(qa.GetX()-xb[0]) > epsilon ) 
      {
      std::cout << "Error in X ! ";
      return EXIT_FAILURE;
      } 
    if( fabs(qa.GetY()-xb[1]) > epsilon ) 
      {
      std::cout << "Error in Y ! ";
      return EXIT_FAILURE;
      } 
    if( fabs(qa.GetZ()-xb[2]) > epsilon ) 
      {
      std::cout << "Error in Z ! ";
      return EXIT_FAILURE;
      } 
    if( fabs(qa.GetW()-cosangle) > epsilon ) 
      {
      std::cout << "Error in W ! ";
      return EXIT_FAILURE;
      } 
    if( fabs(qa.GetAngle()-angle) > epsilon ) 
      {
      std::cout << "Error in Angle ! ";
      return EXIT_FAILURE;
      } 

    std::cout << " PASSED !" << std::endl;
  }
  return EXIT_SUCCESS;

}



