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

  const ValueType epsilon = 1e-12;


  //  Versor type
  typedef    itk::Versor< ValueType >    VersorType;


  //  Vector type
  typedef    VersorType::VectorType      VectorType;


  //  Point type
  typedef    VersorType::PointType      PointType;


  //  Covariant Vector type
  typedef    VersorType::CovariantVectorType      CovariantVectorType;


  //  VnlVector type
  typedef    VersorType::VnlVectorType      VnlVectorType;


  {
    std::cout << "Test default constructor... ";
    VersorType qa;
    if( fabs(qa.GetX()) > epsilon ) 
      {
      std::cout << "Error ! " << std::endl;
      return EXIT_FAILURE;
      } 
    if( fabs(qa.GetY()) > epsilon ) 
      {
      std::cout << "Error ! " << std::endl;
      return EXIT_FAILURE;
      } 
    if( fabs(qa.GetZ()) > epsilon ) 
      {
      std::cout << "Error ! " << std::endl;
      return EXIT_FAILURE;
      } 
    if( fabs(qa.GetW()-1.0) > epsilon ) 
      {
      std::cout << "Error ! " << std::endl;
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
      std::cout << "Error in X ! " << std::endl;
      return EXIT_FAILURE;
      } 
    if( fabs(qa.GetY()-xb[1]) > epsilon ) 
      {
      std::cout << "Error in Y ! " << std::endl;
      return EXIT_FAILURE;
      } 
    if( fabs(qa.GetZ()-xb[2]) > epsilon ) 
      {
      std::cout << "Error in Z ! " << std::endl;
      return EXIT_FAILURE;
      } 
    if( fabs(qa.GetW()-cosangle) > epsilon ) 
      {
      std::cout << "Error in W ! " << std::endl;
      return EXIT_FAILURE;
      } 
    if( fabs(qa.GetAngle()-angle) > epsilon ) 
      {
      std::cout << "Error in Angle ! " << std::endl;
      return EXIT_FAILURE;
      } 

    std::cout << " PASSED !" << std::endl;
  }

  {
    std::cout << "Test for Transforming a vector...";
    VersorType qa;
    VectorType xa;
    xa[0] = 2.5;
    xa[1] = 2.5;
    xa[2] = 2.5;
    ValueType angle = 8.0*atan(1.0)/3.0; // 120 degrees in radians
    
    qa.Set( xa, angle );
        
    VectorType xb;
    xb = 3.0, 7.0, 9.0;

    VectorType xc;

    xc = qa.Transform( xb );

    // This rotation will just permute the axis

    if( fabs(xc[1]-xb[0]) > epsilon ) 
      {
      std::cout << "Error in X ! " << std::endl;
      return EXIT_FAILURE;
      } 
    if( fabs(xc[2]-xb[1]) > epsilon ) 
      {
      std::cout << "Error in Y ! " << std::endl;
      return EXIT_FAILURE;
      } 
    if( fabs(xc[0]-xb[2]) > epsilon ) 
      {
      std::cout << "Error in Z ! " << std::endl;
      return EXIT_FAILURE;
      } 
    std::cout << " PASSED !" << std::endl;
  }

  {
    std::cout << "Test for Transforming a point...";
    VersorType qa;
    VectorType xa;
    xa[0] = 2.5;
    xa[1] = 2.5;
    xa[2] = 2.5;
    ValueType angle = 8.0*atan(1.0)/3.0; // 120 degrees in radians
    
    qa.Set( xa, angle );
        
    PointType xb;
    xb = 3.0, 7.0, 9.0;

    PointType xc;

    xc = qa.Transform( xb );

    // This rotation will just permute the axis

    if( fabs(xc[1]-xb[0]) > epsilon ) 
      {
      std::cout << "Error in X ! " << std::endl;
      return EXIT_FAILURE;
      } 
    if( fabs(xc[2]-xb[1]) > epsilon ) 
      {
      std::cout << "Error in Y ! " << std::endl;
      return EXIT_FAILURE;
      } 
    if( fabs(xc[0]-xb[2]) > epsilon ) 
      {
      std::cout << "Error in Z ! " << std::endl;
      return EXIT_FAILURE;
      } 
    std::cout << " PASSED !" << std::endl;
  }


  {
    std::cout << "Test for Transforming a covariantvector...";
    VersorType qa;
    VectorType xa;
    xa[0] = 2.5;
    xa[1] = 2.5;
    xa[2] = 2.5;
    ValueType angle = 8.0*atan(1.0)/3.0; // 120 degrees in radians
    
    qa.Set( xa, angle );
        
    CovariantVectorType xb;
    xb = 3.0, 7.0, 9.0;

    CovariantVectorType xc;

    xc = qa.Transform( xb );

    // This rotation will just permute the axis

    if( fabs(xc[1]-xb[0]) > epsilon ) 
      {
      std::cout << "Error in X ! " << std::endl;
      return EXIT_FAILURE;
      } 
    if( fabs(xc[2]-xb[1]) > epsilon ) 
      {
      std::cout << "Error in Y ! " << std::endl;
      return EXIT_FAILURE;
      } 
    if( fabs(xc[0]-xb[2]) > epsilon ) 
      {
      std::cout << "Error in Z ! " << std::endl;
      return EXIT_FAILURE;
      } 
    std::cout << " PASSED !" << std::endl;
  }

  {
    std::cout << "Test for Transforming a vnl_vector...";
    VersorType qa;
    VectorType xa;
    xa[0] = 2.5;
    xa[1] = 2.5;
    xa[2] = 2.5;
    ValueType angle = 8.0*atan(1.0)/3.0; // 120 degrees in radians
    
    qa.Set( xa, angle );
        
    VnlVectorType xb;
    xb[0] = 3.0;
    xb[1] = 7.0;
    xb[2] = 9.0;

    VnlVectorType xc;

    xc = qa.Transform( xb );

    // This rotation will just permute the axis

    if( fabs(xc[1]-xb[0]) > epsilon ) 
      {
      std::cout << "Error in X ! " << std::endl;
      return EXIT_FAILURE;
      } 
    if( fabs(xc[2]-xb[1]) > epsilon ) 
      {
      std::cout << "Error in Y ! " << std::endl;
      return EXIT_FAILURE;
      } 
    if( fabs(xc[0]-xb[2]) > epsilon ) 
      {
      std::cout << "Error in Z ! " << std::endl;
      return EXIT_FAILURE;
      } 
    std::cout << " PASSED !" << std::endl;
  }
  
  return EXIT_SUCCESS;

}



