//-------------------------------------------
//
//  Example of the registration hierarchy
//
//-------------------------------------------


#include <itkImage.h>
#include <itkPoint.h>
#include <itkVectorContainer.h>
#include <itkRegistrationMethod.h>
#include <itkRegistrationTransformation.h>
#include <itkRegistrationMapper.h>
#include <itkRegistrationMetric.h>
#include <itkRegistrationOptimizer.h>


int main()
{

  typedef itk::Image<unsigned char,2>             ImageType;

  typedef itk::Point<3,float>                    Point3DType;

  typedef itk::VectorContainer< unsigned short,
                                double >      ParameterType;

  typedef itk::RegistrationTransformation< ParameterType 
                                             > TransfromType;
  
  typedef itk::VectorContainer< unsigned long, 
                                Point3DType >    ReferenceType;


  typedef itk::RegistrationMapper< ReferenceType,
                                   TransfromType
                                             >  MapperType;


  typedef itk::RegistrationMetric< ReferenceType,
                                             ImageType,
                                             MapperType,
                                             double,
                                             double >  MetricType;

  typedef itk::RegistrationOptimizer< MetricType
                                              > OptimizerType;

  typedef itk::RegistrationMethod< MetricType,
                                   OptimizerType >   RegistrationType;

  ReferenceType::Pointer      reference         = ReferenceType::New();
  ImageType::Pointer          target            = ImageType::New();
  TransfromType::Pointer      transformation    = TransfromType::New();
  RegistrationType::Pointer   registration      = RegistrationType::New();

  ImageType::SizeType size;
  size[0] = 200;
  size[1] = 200;

  ImageType::IndexType index;
  index[0] = 0;
  index[1] = 0;

  ImageType::RegionType region;
  region.SetIndex( index );
  region.SetSize( size );

  target->SetLargestPossibleRegion( region );
  target->SetRequestedRegion( region );
  target->SetBufferedRegion( region );
  target->Allocate();

  registration->SetTarget( target );
  registration->SetReference( reference );
  registration->SetTransformation( transformation );

  return 0;

}



