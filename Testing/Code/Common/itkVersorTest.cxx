/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVersorTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
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
int itkVersorTest(int, char* [] ) 
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
  typedef    VersorType::VnlVectorType       VnlVectorType;


  //  VnlQuaternion type
  typedef    VersorType::VnlQuaternionType   VnlQuaternionType;


  //  Matrix type
  typedef    VersorType::MatrixType          MatrixType;




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
    std::cout << "Test initialization and GetMatrix()... ";
    VersorType qa;
    qa.SetIdentity();
    MatrixType ma = qa.GetMatrix();
    std::cout << "Matrix = " << std::endl;
    std::cout <<    ma       << std::endl;
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
    std::cout << "Test for setting Right part...";
    VersorType qa;
    VectorType xa;
    
    ValueType angle = atan(1.0)*30.0/45.0;
    ValueType sin2a = sin( angle/2.0 );
    
    xa[0] = 0.7;
    xa[1] = 0.3;
    xa[2] = 0.1;
    
    xa.Normalize();

    xa *= sin2a;

    qa.Set( xa, angle );
        
    ValueType cos2a = cos( angle/2.0 );

    if( fabs(qa.GetW()-cos2a) > epsilon ) 
      {
      std::cout << "Error in W ! " << std::endl;
      std::cout << "W= " << qa.GetW();
      std::cout << " it should be " << cos2a << std::endl;
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
    std::cout << "Test for Square Root...";
    VersorType qa;
    VectorType xa;
    
    ValueType angle = atan(1.0)*30.0/45.0;
    ValueType sin2a = sin( angle/2.0 );
    
    xa[0] = 0.7;
    xa[1] = 0.3;
    xa[2] = 0.1;
    
    xa.Normalize();

    xa *= sin2a;

    qa.Set( xa, angle );
        
    VersorType qb;
 
    qb = qa.SquareRoot();

    if( fabs( qa.GetAngle() - 2.0 * qb.GetAngle() ) > epsilon ) 
      {
      std::cout << "Error in Square Root ! " << std::endl;
      std::cout << "Angle = " << qb.GetAngle();
      std::cout << " it should be " << qa.GetAngle() / 2.0 << std::endl;
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
        
    VectorType::ValueType xbInit[3] = {3.0, 7.0, 9.0};
    VectorType xb = xbInit;

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
        
    PointType::ValueType xbInit[3] = {3.0, 7.0, 9.0};
    PointType xb = xbInit;

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
        
    CovariantVectorType::ValueType xbInit[3] = {3.0, 7.0, 9.0};
    CovariantVectorType xb = xbInit;

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
  


  {

    std::cout << "Test for Set components operations ...";

    // First, create a known versor
    VersorType v1;

    VectorType::ValueType x1Init[3] = {2.5f, 1.5f, 3.5f};
    VectorType x1 = x1Init;

    ValueType angle1 = atan(1.0)/3.0; // 15 degrees in radians
    
    v1.Set( x1, angle1 );
 
    // Get the components and scale them
    ValueType scale = 5.5;
    ValueType x = v1.GetX() * scale;
    ValueType y = v1.GetY() * scale;
    ValueType z = v1.GetZ() * scale;
    ValueType w = v1.GetW() * scale;
 
    VersorType v2;
    v2.Set( x, y, z, w );

    // Compare both versors
    if( fabs(v1.GetX() - v2.GetX() ) > epsilon ||
        fabs(v1.GetY() - v2.GetY() ) > epsilon ||
        fabs(v1.GetZ() - v2.GetZ() ) > epsilon ||
        fabs(v1.GetW() - v2.GetW() ) > epsilon )
      {
      std::cout << "Error in Versor Set(x,y,z,w) ! " << std::endl;
      std::cout << "v1  = " << v1 << std::endl;
      std::cout << "v2  = " << v2 << std::endl;
      return EXIT_FAILURE;
      } 
    std::cout << " PASSED !" << std::endl;


    std::cout << "Test for Set quaternion ...";
    // Get a vnl_quaternion
    VnlQuaternionType vnlq = v1.GetVnlQuaternion();
   
    vnlq *= scale;

    v2.Set( vnlq );

    // Compare both versors
    if( fabs(v1.GetX() - v2.GetX() ) > epsilon ||
        fabs(v1.GetY() - v2.GetY() ) > epsilon ||
        fabs(v1.GetZ() - v2.GetZ() ) > epsilon ||
        fabs(v1.GetW() - v2.GetW() ) > epsilon )
      {
      std::cout << "Error in Versor Set( vnl_quaternion ) ! " << std::endl;
      std::cout << "v1  = " << v1 << std::endl;
      std::cout << "v2  = " << v2 << std::endl;
      return EXIT_FAILURE;
      } 
    std::cout << " PASSED !" << std::endl;



  }



  {

    std::cout << "Test for Reciprocal and Conjugate Operations...";

    VersorType  v1;
    VersorType  v2;

    VectorType::ValueType x1Init[3] = {2.5f, 1.5f, 0.5f};
    VectorType x1 = x1Init;

    ValueType angle1 = atan(1.0)/3.0; // 15 degrees in radians
    
    VectorType::ValueType x2Init[3] = {1.5f, 0.5f, 0.5f};
    VectorType x2 = x2Init;

    ValueType angle2 = atan(1.0)/1.0; // 45 degrees in radians
    
    v1.Set( x1, angle1 );
    v2.Set( x2, angle2 );
        

    VersorType unit;
    VersorType v2r;

    v2r  = v2.GetReciprocal();
    unit = v2 * v2r;

    if( fabs( unit.GetX() ) > epsilon ||
        fabs( unit.GetY() ) > epsilon ||
        fabs( unit.GetZ() ) > epsilon ||
        fabs( unit.GetW() - 1.0 ) > epsilon )
      {
      std::cout << "Error in Reciprocal ! " << std::endl;
      std::cout << "Versor     = " << v2    << std::endl;
      std::cout << "Reciprocal = " << v2r   << std::endl;
      std::cout << "Product    = " << unit  << std::endl;

      return EXIT_FAILURE;
      }  


    unit = v2 / v2;

    if( fabs( unit.GetX() ) > epsilon ||
        fabs( unit.GetY() ) > epsilon ||
        fabs( unit.GetZ() ) > epsilon ||
        fabs( unit.GetW() - 1.0 ) > epsilon )
      {
      std::cout << "Error in Division ! " << std::endl;
      std::cout << "Versor          = " << v2    << std::endl;
      std::cout << "Self Division   = " << unit  << std::endl;

      return EXIT_FAILURE;
      }  

    unit =  v2;
    unit /= v2;

    if( fabs( unit.GetX() ) > epsilon ||
        fabs( unit.GetY() ) > epsilon ||
        fabs( unit.GetZ() ) > epsilon ||
        fabs( unit.GetW() - 1.0 ) > epsilon )
      {
      std::cout << "Error in Division operator/= ! " << std::endl;
      std::cout << "Versor          = " << v2    << std::endl;
      std::cout << "Self Division   = " << unit  << std::endl;

      return EXIT_FAILURE;
      }  





    x1.Normalize();
    x2.Normalize();

   
    VersorType  v3;

    v3 = v1 * v2;

    VersorType v4;

    v4 = v3 * v2r;


    if( fabs(v1.GetX() - v4.GetX() ) > epsilon ||
        fabs(v1.GetY() - v4.GetY() ) > epsilon ||
        fabs(v1.GetZ() - v4.GetZ() ) > epsilon ||
        fabs(v1.GetW() - v4.GetW() ) > epsilon )
      {
      std::cout << "Error in Versor division ! " << std::endl;
      std::cout << "v1  = " << v1 << std::endl;
      std::cout << "v1' = " << v4 << std::endl;
      return EXIT_FAILURE;
      } 
    std::cout << " PASSED !" << std::endl;


  }


  return EXIT_SUCCESS;

}



