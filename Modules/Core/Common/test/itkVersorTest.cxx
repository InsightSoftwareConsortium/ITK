/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

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

itk::Matrix<double,3,3> TestCreateRotationMatrixFromAngles(const double alpha, const double beta, const double gamma)
{
  //alpha is rotate the X axis -- Attitude
  //beta is rotate the Y axis  -- Bank
  //gamma is rotate the Z axis -- Heading
  const double ca=std::cos(alpha);
  const double sa=std::sin(alpha);
  const double cb=std::cos(beta);
  const double sb=std::sin(beta);
  const double cg=std::cos(gamma);
  const double sg=std::sin(gamma);

  itk::Matrix<double,3,3> R;

  R(0,0)=cb*cg;  R(0,1)=-ca*sg+sa*sb*cg; R(0,2)=sa*sg+ca*sb*cg;
  R(1,0)=cb*sg;  R(1,1)=ca*cg+sa*sb*sg;  R(1,2)=-sa*cg+ca*sb*sg;
  R(2,0)=-sb;    R(2,1)=sa*cb;           R(2,2)=ca*cb;
  itk::Matrix<double,3,3>::InternalMatrixType test =
    R.GetVnlMatrix() * R.GetTranspose();
   if( !test.is_identity( 1.0e-10 ) )
    {
    std::cout << "Computed matrix is not orthogonal!!!" << std::endl;
    std::cout << R << std::endl;
    }
  return R;
}


itk::Versor<double> TestCreateRotationVersorFromAngles(const double alpha, const double beta, const double gamma)
{
  //http://en.wikipedia.org/wiki/Conversion_between_quaternions_and_Euler_angles
  //psi = alpha is rotate the X axis -- Attitude
  //theta= beta is rotate the Y axis  -- Bank
  //phi=  gamma is rotate the Z axis -- Heading
  const double cha=std::cos(alpha*0.5);
  const double chb=std::cos(beta*0.5);
  const double chg=std::cos(gamma*0.5);
  const double sha=std::sin(alpha*0.5);
  const double shb=std::sin(beta*0.5);
  const double shg=std::sin(gamma*0.5);

  vnl_vector_fixed<double,4> q;
  q[0]=cha*chb*chg+sha*shb*shg;
  q[1]=sha*chb*chg-cha*shb*shg;
  q[2]=cha*shb*chg+sha*chb*shg;
  q[3]=cha*chb*shg-sha*shb*chg;

  itk::Versor<double> v;
  v.Set(q[1],q[2],q[3],q[0]);
  std::cout << "versor: " << v << std::endl;
  return v;
}

/**
 * This test that the conversion to and from Rotaion Matrix and
 * Versor produces consistent results.
 */
int RotationMatrixToVersorTest(void)
{
  int errorCount=0;
  //const double onedegree=1e-10*itk::Math::pi/180.0;
  const double onedegree=itk::Math::pi/180.0;
  //const double td=180.0/itk::Math::pi;
  double centers[6];
  centers[0]=0;
  centers[1]=itk::Math::pi*0.25;
  centers[2]=itk::Math::pi*0.5;
  centers[3]=itk::Math::pi;
  centers[4]=itk::Math::pi*1.5;
  centers[5]=itk::Math::pi*2.0;

  const double steps=0;
  const double small_degree_steps=onedegree/1000.0; //1/1000 of a degree
  for(int j = 0; j < 6; j++)
    {
    for(double alpha=centers[j]-steps*small_degree_steps; alpha <= centers[j]+steps*small_degree_steps; alpha += small_degree_steps)
      {
      for(double beta=centers[j]-steps*small_degree_steps; beta <= centers[j]+steps*small_degree_steps; beta += small_degree_steps)
        {
        for(double gamma=centers[j]-steps*small_degree_steps; gamma <= centers[j]+steps*small_degree_steps; gamma += small_degree_steps)
          {
          itk::Matrix<double,3,3> MR=TestCreateRotationMatrixFromAngles(alpha, beta, gamma);
          itk::Versor<double> VR=TestCreateRotationVersorFromAngles(alpha, beta, gamma);

          itk::Point<double,3> testPoint;
          testPoint[0]=-1020.27;
          testPoint[1]=3.21;
          testPoint[2]=1000.786432;

          itk::Versor<double> VFROMMR;
          VFROMMR.Set(MR);
          itk::Matrix<double,3,3> VRMatrix = VR.GetMatrix();
          const itk::Point<double,3> newMRtestPoint=(MR)*testPoint;
          const itk::Point<double,3> newVRtestPoint=(VRMatrix)*testPoint;

          const itk::Point<double,3> newVRFROMMRPoint=(VFROMMR.GetMatrix())*testPoint;
          const itk::Point<double,3> newVRFROMMRTransformPoint=VFROMMR.Transform(testPoint);

          const double error_newMRtestPoint_newVRtestPoint=(newMRtestPoint-newVRtestPoint).GetNorm();
          const double error_newMRtestPoint_newVRFROMMRPoint=(newMRtestPoint-newVRFROMMRPoint).GetNorm();
          const double error_newVRFROMMRPoint_newVRFROMMRTransformPoint=(newVRFROMMRPoint-newVRFROMMRTransformPoint).GetNorm();

          const double maxAllowedPointError=1e-5;
          if( ( error_newMRtestPoint_newVRtestPoint + error_newMRtestPoint_newVRFROMMRPoint
                + error_newVRFROMMRPoint_newVRFROMMRTransformPoint) > maxAllowedPointError)
            {
            std::cout << "(alpha,beta,gamma)= (" << alpha << ","<< beta << "," << gamma << ")" << std::endl;

            std::cout << newMRtestPoint << " " << newVRtestPoint << " " << newVRFROMMRPoint << " " << newVRFROMMRTransformPoint << std::endl;
            std::cout << "ERRORS: " << error_newMRtestPoint_newVRtestPoint << " "
                      << error_newMRtestPoint_newVRFROMMRPoint << " "
                      << error_newVRFROMMRPoint_newVRFROMMRTransformPoint << std::endl;
            std::cout << "MR=\n"<< MR << "\nVR=\n" << VR.GetMatrix() << "\nVFROMMR=\n" << VFROMMR.GetMatrix() << std::endl;
            errorCount++;
            }

          }
        }
      }
    }
  return errorCount;
}

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
    if( std::abs(qa.GetX()) > epsilon )
      {
      std::cout << "Error ! " << std::endl;
      return EXIT_FAILURE;
      }
    if( std::abs(qa.GetY()) > epsilon )
      {
      std::cout << "Error ! " << std::endl;
      return EXIT_FAILURE;
      }
    if( std::abs(qa.GetZ()) > epsilon )
      {
      std::cout << "Error ! " << std::endl;
      return EXIT_FAILURE;
      }
    if( std::abs(qa.GetW()-1.0) > epsilon )
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
    std::cout << "Test for setting Axis (0,0,0) and Angle...";
    VersorType qa;
    VectorType xa;
    xa[0] = 0.0;
    xa[1] = 0.0;
    xa[2] = 0.0;
    ValueType angle = 0;
    try
      {
      qa.Set( xa, angle );
      return EXIT_FAILURE;
      }    //setting the axis to (0,0,0) should throw an exception
    catch(itk::ExceptionObject &excp)
      {
      std::cout << "Caught expected exception: " << excp;
      std::cout << " PASSED !" << std::endl;
      }
  }

  {
    std::cout << "Test for setting Axis and Angle...";
    VersorType qa;
    VectorType xa;
    xa[0] = 2.5;
    xa[1] = 1.5;
    xa[2] = 0.5;
    ValueType angle = std::atan(1.0)/3.0; // 15 degrees in radians
    qa.Set( xa, angle );

    xa.Normalize();

    ValueType cosangle = std::cos( angle / 2.0 );
    ValueType sinangle = std::sin( angle / 2.0 );

    VectorType xb;

    xb =  xa * sinangle;

    if( std::abs(qa.GetX()-xb[0]) > epsilon )
      {
      std::cout << "Error in X ! " << std::endl;
      return EXIT_FAILURE;
      }
    if( std::abs(qa.GetY()-xb[1]) > epsilon )
      {
      std::cout << "Error in Y ! " << std::endl;
      return EXIT_FAILURE;
      }
    if( std::abs(qa.GetZ()-xb[2]) > epsilon )
      {
      std::cout << "Error in Z ! " << std::endl;
      return EXIT_FAILURE;
      }
    if( std::abs(qa.GetW()-cosangle) > epsilon )
      {
      std::cout << "Error in W ! " << std::endl;
      return EXIT_FAILURE;
      }
    if( std::abs(qa.GetAngle()-angle) > epsilon )
      {
      std::cout << "Error in Angle ! " << std::endl;
      return EXIT_FAILURE;
      }

    std::cout << " PASSED !" << std::endl;
  }

  {
    std::cout << "Test for setting Right part...";
    ValueType angle = std::atan(1.0)*30.0/45.0;
    ValueType sin2a = std::sin( angle/2.0 );

    VectorType xa;
    xa[0] = 0.7;
    xa[1] = 0.3;
    xa[2] = 0.1;

    xa.Normalize();
    xa *= sin2a;

    VersorType qa;
    qa.Set( xa, angle );
    ValueType cos2a = std::cos( angle/2.0 );

    if( std::abs(qa.GetW()-cos2a) > epsilon )
      {
      std::cout << "Error in W ! " << std::endl;
      std::cout << "W= " << qa.GetW();
      std::cout << " it should be " << cos2a << std::endl;
      return EXIT_FAILURE;
      }
    if( std::abs(qa.GetAngle()-angle) > epsilon )
      {
      std::cout << "Error in Angle ! " << std::endl;
      return EXIT_FAILURE;
      }
    std::cout << " PASSED !" << std::endl;
  }

 {
    std::cout << "Test for Square Root...";

    ValueType angle = std::atan(1.0)*30.0/45.0;
    ValueType sin2a = std::sin( angle/2.0 );

    VectorType xa;
    xa[0] = 0.7;
    xa[1] = 0.3;
    xa[2] = 0.1;

    xa.Normalize();
    xa *= sin2a;

    VersorType qa;
    qa.Set( xa, angle );

    VersorType qb;
    qb = qa.SquareRoot();

    if( std::abs( qa.GetAngle() - 2.0 * qb.GetAngle() ) > epsilon )
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
    VectorType xa;
    xa[0] = 2.5;
    xa[1] = 2.5;
    xa[2] = 2.5;
    ValueType angle = 8.0*std::atan(1.0)/3.0; // 120 degrees in radians

    VersorType qa;
    qa.Set( xa, angle );

    VectorType::ValueType xbInit[3] = {3.0, 7.0, 9.0};
    VectorType xb = xbInit;

    VectorType xc= qa.Transform( xb );

    // This rotation will just permute the axis
    if( std::abs(xc[1]-xb[0]) > epsilon )
      {
      std::cout << "Error in X ! " << std::endl;
      return EXIT_FAILURE;
      }
    if( std::abs(xc[2]-xb[1]) > epsilon )
      {
      std::cout << "Error in Y ! " << std::endl;
      return EXIT_FAILURE;
      }
    if( std::abs(xc[0]-xb[2]) > epsilon )
      {
      std::cout << "Error in Z ! " << std::endl;
      return EXIT_FAILURE;
      }
    std::cout << " PASSED !" << std::endl;
  }

  {
    std::cout << "Test for Transforming a point...";
    VectorType xa;
    xa[0] = 2.5;
    xa[1] = 2.5;
    xa[2] = 2.5;
    ValueType angle = 8.0*std::atan(1.0)/3.0; // 120 degrees in radians

    VersorType qa;
    qa.Set( xa, angle );

    PointType::ValueType xbInit[3] = {3.0, 7.0, 9.0};
    PointType xb = xbInit;

    PointType xc = qa.Transform( xb );

    // This rotation will just permute the axis
    if( std::abs(xc[1]-xb[0]) > epsilon )
      {
      std::cout << "Error in X ! " << std::endl;
      return EXIT_FAILURE;
      }
    if( std::abs(xc[2]-xb[1]) > epsilon )
      {
      std::cout << "Error in Y ! " << std::endl;
      return EXIT_FAILURE;
      }
    if( std::abs(xc[0]-xb[2]) > epsilon )
      {
      std::cout << "Error in Z ! " << std::endl;
      return EXIT_FAILURE;
      }
    std::cout << " PASSED !" << std::endl;
  }


  {
    std::cout << "Test for Transforming a covariantvector...";
    VectorType xa;
    xa[0] = 2.5;
    xa[1] = 2.5;
    xa[2] = 2.5;
    ValueType angle = 8.0*std::atan(1.0)/3.0; // 120 degrees in radians

    VersorType qa;
    qa.Set( xa, angle );

    CovariantVectorType::ValueType xbInit[3] = {3.0, 7.0, 9.0};
    CovariantVectorType xb = xbInit;

    CovariantVectorType xc = qa.Transform( xb );

    // This rotation will just permute the axis
    if( std::abs(xc[1]-xb[0]) > epsilon )
      {
      std::cout << "Error in X ! " << std::endl;
      return EXIT_FAILURE;
      }
    if( std::abs(xc[2]-xb[1]) > epsilon )
      {
      std::cout << "Error in Y ! " << std::endl;
      return EXIT_FAILURE;
      }
    if( std::abs(xc[0]-xb[2]) > epsilon )
      {
      std::cout << "Error in Z ! " << std::endl;
      return EXIT_FAILURE;
      }
    std::cout << " PASSED !" << std::endl;
  }

  {
    std::cout << "Test for Transforming a vnl_vector...";
    VectorType xa;
    xa[0] = 2.5;
    xa[1] = 2.5;
    xa[2] = 2.5;
    ValueType angle = 8.0*std::atan(1.0)/3.0; // 120 degrees in radians

    VersorType qa;
    qa.Set( xa, angle );

    VnlVectorType xb;
    xb[0] = 3.0;
    xb[1] = 7.0;
    xb[2] = 9.0;

    VnlVectorType xc = qa.Transform( xb );

    // This rotation will just permute the axis
    if( std::abs(xc[1]-xb[0]) > epsilon )
      {
      std::cout << "Error in X ! " << std::endl;
      return EXIT_FAILURE;
      }
    if( std::abs(xc[2]-xb[1]) > epsilon )
      {
      std::cout << "Error in Y ! " << std::endl;
      return EXIT_FAILURE;
      }
    if( std::abs(xc[0]-xb[2]) > epsilon )
      {
      std::cout << "Error in Z ! " << std::endl;
      return EXIT_FAILURE;
      }
    std::cout << " PASSED !" << std::endl;
  }

  {
    std::cout << "Test for Set components operations ...";

    // First, create a known versor
    VectorType::ValueType x1Init[3] = {2.5f, 1.5f, 3.5f};
    VectorType x1 = x1Init;

    ValueType angle1 = std::atan(1.0)/3.0; // 15 degrees in radians

    VersorType v1;
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
    if( std::abs(v1.GetX() - v2.GetX() ) > epsilon ||
        std::abs(v1.GetY() - v2.GetY() ) > epsilon ||
        std::abs(v1.GetZ() - v2.GetZ() ) > epsilon ||
        std::abs(v1.GetW() - v2.GetW() ) > epsilon )
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
    if( std::abs(v1.GetX() - v2.GetX() ) > epsilon ||
        std::abs(v1.GetY() - v2.GetY() ) > epsilon ||
        std::abs(v1.GetZ() - v2.GetZ() ) > epsilon ||
        std::abs(v1.GetW() - v2.GetW() ) > epsilon )
      {
      std::cout << "Error in Versor Set( vnl_quaternion ) ! " << std::endl;
      std::cout << "v1  = " << v1 << std::endl;
      std::cout << "v2  = " << v2 << std::endl;
      return EXIT_FAILURE;
      }
    std::cout << " PASSED !" << std::endl;

    std::cout << "Test for Set(x,y,z,w) with negative W.";
    // Check that a negative W results in negating
    // all the versor components.
    x = - v1.GetX();
    y = - v1.GetY();
    z = - v1.GetZ();
    w = - v1.GetW();

    VersorType v3;
    v3.Set( x, y, z, w );

     // Compare both versors
    if( std::abs( v1.GetX() - v3.GetX() ) > epsilon ||
        std::abs( v1.GetY() - v3.GetY() ) > epsilon ||
        std::abs( v1.GetZ() - v3.GetZ() ) > epsilon ||
        std::abs( v1.GetW() - v3.GetW() ) > epsilon )
      {
      std::cout << "Error in Versor Set() with negative W ! " << std::endl;
      std::cout << "v1  = " << v1 << std::endl;
      std::cout << "v3  = " << v3 << std::endl;
      return EXIT_FAILURE;
      }
    std::cout << " PASSED !" << std::endl;
  }

  {
    std::cout << "Test for Reciprocal and Conjugate Operations...";

    VectorType::ValueType x1Init[3] = {2.5f, 1.5f, 0.5f};
    VectorType x1 = x1Init;

    ValueType angle1 = std::atan(1.0)/3.0; // 15 degrees in radians

    VectorType::ValueType x2Init[3] = {1.5f, 0.5f, 0.5f};
    VectorType x2 = x2Init;

    ValueType angle2 = std::atan(1.0)/1.0; // 45 degrees in radians

    VersorType  v1;
    v1.Set( x1, angle1 );
    VersorType  v2;
    v2.Set( x2, angle2 );

    VersorType v2r = v2.GetReciprocal();
    VersorType unit = v2 * v2r;

    if( std::abs( unit.GetX() ) > epsilon ||
        std::abs( unit.GetY() ) > epsilon ||
        std::abs( unit.GetZ() ) > epsilon ||
        std::abs( unit.GetW() - 1.0 ) > epsilon )
      {
      std::cout << "Error in Reciprocal ! " << std::endl;
      std::cout << "Versor     = " << v2    << std::endl;
      std::cout << "Reciprocal = " << v2r   << std::endl;
      std::cout << "Product    = " << unit  << std::endl;

      return EXIT_FAILURE;
      }

    unit = v2 / v2;

    if( std::abs( unit.GetX() ) > epsilon ||
        std::abs( unit.GetY() ) > epsilon ||
        std::abs( unit.GetZ() ) > epsilon ||
        std::abs( unit.GetW() - 1.0 ) > epsilon )
      {
      std::cout << "Error in Division ! " << std::endl;
      std::cout << "Versor          = " << v2    << std::endl;
      std::cout << "Self Division   = " << unit  << std::endl;

      return EXIT_FAILURE;
      }

    unit =  v2;
    unit /= v2;
    if( std::abs( unit.GetX() ) > epsilon ||
        std::abs( unit.GetY() ) > epsilon ||
        std::abs( unit.GetZ() ) > epsilon ||
        std::abs( unit.GetW() - 1.0 ) > epsilon )
      {
      std::cout << "Error in Division operator/= ! " << std::endl;
      std::cout << "Versor          = " << v2    << std::endl;
      std::cout << "Self Division   = " << unit  << std::endl;

      return EXIT_FAILURE;
      }

    x1.Normalize();
    x2.Normalize();


    VersorType v3= v1 * v2;
    VersorType v4= v3 * v2r;

    if( std::abs(v1.GetX() - v4.GetX() ) > epsilon ||
        std::abs(v1.GetY() - v4.GetY() ) > epsilon ||
        std::abs(v1.GetZ() - v4.GetZ() ) > epsilon ||
        std::abs(v1.GetW() - v4.GetW() ) > epsilon )
      {
      std::cout << "Error in Versor division ! " << std::endl;
      std::cout << "v1  = " << v1 << std::endl;
      std::cout << "v1' = " << v4 << std::endl;
      return EXIT_FAILURE;
      }
    std::cout << " PASSED !" << std::endl;
  }


  { // Test for the Set() matrix method
    std::cout << "Test for Set( MatrixType ) method ..." << std::endl;
    MatrixType mm;
    // Setting the matrix of a 90 degrees rotation around Z
    mm[0][0] =  0.0;
    mm[0][1] =  1.0;
    mm[0][2] =  0.0;

    mm[1][0] =  -1.0;
    mm[1][1] =  0.0;
    mm[1][2] =  0.0;

    mm[2][0] =  0.0;
    mm[2][1] =  0.0;
    mm[2][2] =  1.0;

    VersorType vv;
    vv.Set( mm );

    const double halfSqrtOfTwo = std::sqrt( 2.0 ) / 2.0;

    if( std::abs(vv.GetX() -             0.0  ) > epsilon ||
        std::abs(vv.GetY() -             0.0  ) > epsilon ||
        std::abs(vv.GetZ() - (-halfSqrtOfTwo) ) > epsilon ||
        std::abs(vv.GetW() -   halfSqrtOfTwo  ) > epsilon )
      {
      std::cout << "Error in Versor Set(Matrix) method ! " << std::endl;
      std::cout << "vv  = " << vv << std::endl;
      return EXIT_FAILURE;
      }
      //matrix no longer represents a rotation
    mm[0][0] = 1.0;
    try
      {
      vv.Set( mm );
      return EXIT_FAILURE;
      }    //should always get here, mm isn't a rotation
    catch(itk::ExceptionObject &excp)
      {
      std::cout << "Caught expected exception: " << excp;
      }
    std::cout << " PASSED !" << std::endl;
  }
  {
  std::cout << "Test for Set( MatrixType ) method with rotations that are susceptible to errors in conversion to/from the rotation matrix...";

  const int RotationMatrixStabilityTestErrors=RotationMatrixToVersorTest();
  if( RotationMatrixStabilityTestErrors > 0 )
    {
    std::cout << "Error in stability of converting to/from RotationMatrix with Set(Matrix) method ! " << std::endl;
    std::cout << "Errors Found  = " << RotationMatrixStabilityTestErrors << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << " PASSED !" << std::endl;

  }

  return EXIT_SUCCESS;

}
