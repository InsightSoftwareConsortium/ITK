//-------------------------------------------
//
//  Example of the registration hierarchy
//
//-------------------------------------------


#include <itkPoint.h>
#include <vnl/vnl_vector.h>
#include <vnl/vnl_matrix.h>
#include <itkVectorContainer.h>
#include <itkRegistrationMethod.h>
#include <itkRegistrationTransformation.h>
#include <itkRegistrationMetric.h>
#include <itkRegistrationMapper.h>
#include <itkRegistrationOptimizer.h>


int main()
{

  typedef itk::Point<2,float>                    Point2DType;
  typedef itk::Point<3,float>                    Point3DType;

  typedef itk::VectorContainer< unsigned long, 
                                Point2DType    > TargetType;

  typedef itk::VectorContainer< unsigned long, 
                                Point3DType >    ReferenceType;

  typedef itk::VectorContainer< unsigned short, 
                                double >         ParameterType;

  typedef vnl_vector< double >                   MeasureType;
  typedef vnl_matrix< double >                   DerivativeType;
                                 
  typedef itk::RegistrationTransformation< ParameterType
                                            >     TransfromType;

   typedef itk::RegistrationMapper< ReferenceType,
                                    TransfromType
                                               >  MapperType;
 
  typedef itk::RegistrationMetric< ReferenceType,
                                             TargetType,
                                             MapperType,
                                             MeasureType,
                                             DerivativeType >  MetricType;

  typedef itk::RegistrationOptimizer< MetricType >
                                                       OptimizerType;

  typedef itk::RegistrationMethod< MetricType,
                                   OptimizerType >   RegistrationType;

  TargetType::Pointer         target            = TargetType::New();
  ReferenceType::Pointer      reference         = ReferenceType::New();
  TransfromType::Pointer      transformation    = TransfromType::New();
  RegistrationType::Pointer   registration      = RegistrationType::New();

  registration->SetTarget( target );
  registration->SetReference( reference );
  registration->SetTransformation( transformation );

  registration->StartRegistration();

  return 0;

}



