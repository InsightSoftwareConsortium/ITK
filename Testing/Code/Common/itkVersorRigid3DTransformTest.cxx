/**
 *  
 *  This program illustrates the use of VersorsRigid3DTransform
 *  
 *  Versors are Unit Quaternions used to represent rotations. 
 *  VersorRigid3DTransform is a Rigid 3D Transform that support
 *  Versors and Vectors in its interface.
 *
 */


#include "itkVersorRigid3DTransform.h"
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


  //  Versor Transform type
  typedef    itk::VersorRigid3DTransform< ValueType >   TransformType;

  //  Versor type
  typedef    TransformType::VersorType      VersorType;


  //  Vector type
  typedef    TransformType::InputVectorType      VectorType;


  //  Point type
  typedef    TransformType::InputPointType      PointType;


  //  Covariant Vector type
  typedef    TransformType::InputCovariantVectorType      CovariantVectorType;


  //  VnlVector type
  typedef    TransformType::InputVnlVectorType      VnlVectorType;


  //  Parameters type
  typedef    TransformType::ParametersType      ParametersType;


  {
    std::cout << "Test default constructor... ";
    
    TransformType::Pointer transform = TransformType::New();

    VectorType axis;
    axis = 1.5, 1.5, 1.5;

    ValueType angle = 120.0*atan(1.0)/45.0;

    VersorType versor;
    versor.Set( axis, angle );
    
    ParametersType parameters;

    parameters[0] = versor.GetX();
    parameters[1] = versor.GetY();
    parameters[2] = versor.GetZ();
    parameters[3] = versor.GetW();
    parameters[4] = 0.0;
    parameters[5] = 0.0;
    parameters[6] = 0.0;

    transform->SetParameters( parameters );

    if( 0.0 > epsilon ) 
      {
      std::cout << "Error ! " << std::endl;
      return EXIT_FAILURE;
      } 
    std::cout << " PASSED !" << std::endl;

  }



  /* Create a Rigid 3D transform with rotation */

  {
    bool Ok = true;

    TransformType::Pointer  rotation = TransformType::New();

    itk::Vector<double,3> axis;
    axis = 1,1,1;

    const double angle = (atan(1.0)/45.0)*120.0; // turn 120 degrees

    // this rotation will permute the axis x->y, y->z, z->x
    rotation->SetRotation( axis, angle );

    TransformType::OffsetType offset = rotation->GetOffset();
    std::cout << "pure Rotation test:  ";
    std::cout << offset << std::endl;

    for(unsigned int i=0; i<3; i++)
    {
      if( fabs( offset[i] - 0.0 ) > epsilon )
      {
        Ok = false;
        break;    
      }
    }
    if( !Ok )
    { 
      std::cerr << "Get Offset  differs from null in rotation " << std::endl;
      return EXIT_FAILURE;
    }

    {
      // Rotate an itk::Point
      TransformType::InputPointType p;
      p = 1,4,9;
      TransformType::OutputPointType q;
      q[0] = p[1];
      q[1] = p[2];
      q[2] = p[0];

      TransformType::OutputPointType r;
      r = rotation->TransformPoint( p );
      for(unsigned int i=0; i<3; i++)
      {
        if( fabs( q[i]- r[i] ) > epsilon )
        {
          Ok = false;
          break;    
        }
      }
      if( !Ok )
      { 
        std::cerr << "Error rotating point : " << p << std::endl;
        std::cerr << "Result should be     : " << q << std::endl;
        std::cerr << "Reported Result is   : " << r << std::endl;
        return EXIT_FAILURE;
      }
      else
      {
        std::cout << "Ok rotating an itk::Point " << std::endl;
      }
    }

    {
      // Translate an itk::Vector
      TransformType::InputVectorType p;
      p = 1,4,9;
      TransformType::OutputVectorType q;
      q[0] = p[1];
      q[1] = p[2];
      q[2] = p[0];

      TransformType::OutputVectorType r;
      r = rotation->TransformVector( p );
      for(unsigned int i=0; i<3; i++)
      {
        if( fabs( q[i] - r[i] ) > epsilon )
        {
          Ok = false;
          break;    
        }
      }
      if( !Ok )
      { 
        std::cerr << "Error rotating vector : " << p << std::endl;
        std::cerr << "Result should be      : " << q << std::endl;
        std::cerr << "Reported Result is    : " << r << std::endl;
        return EXIT_FAILURE;
      }
      else
      {
        std::cout << "Ok rotating an itk::Vector " << std::endl;
      }
    }


    {
      // Translate an itk::CovariantVector
      TransformType::InputCovariantVectorType p;
      p = 1,4,9;
      TransformType::OutputCovariantVectorType q;
      q[0] = p[1];
      q[1] = p[2];
      q[2] = p[0];

      TransformType::OutputCovariantVectorType r;
      r = rotation->TransformCovariantVector( p );
      for(unsigned int i=0; i<3; i++)
      {
        if( fabs( q[i] - r[i] ) > epsilon )
        {
          Ok = false;
          break;    
        }
      }
      if( !Ok )
      { 
        std::cerr << "Error rotating covariant vector : " << p << std::endl;
        std::cerr << "Result should be                : " << q << std::endl;
        std::cerr << "Reported Result is              : " << r << std::endl;
        return EXIT_FAILURE;
      }
      else
      {
        std::cout << "Ok rotating an itk::CovariantVector " << std::endl;
      }
    }

    
    {
      // Translate a vnl_vector
      TransformType::InputVnlVectorType p;
      p[0] = 1;
      p[1] = 4;
      p[2] = 9;

      TransformType::OutputVnlVectorType q;

      q[0] = p[1];
      q[1] = p[2];
      q[2] = p[0];

      TransformType::OutputVnlVectorType r;
      r = rotation->TransformVnlVector( p );
      for(unsigned int i=0; i<3; i++)
      {
        if( fabs( q[i] - r[i] ) > epsilon )
        {
          Ok = false;
          break;    
        }
      }
      if( !Ok )
      { 
        std::cerr << "Error rotating vnl_vector : " << p << std::endl;
        std::cerr << "Result should be          : " << q << std::endl;
        std::cerr << "Reported Result is        : " << r << std::endl;
        return EXIT_FAILURE;
      }
      else
      {
        std::cout << "Ok rotating an vnl_Vector " << std::endl;
      }
    }




  }


  
  return EXIT_SUCCESS;

}



