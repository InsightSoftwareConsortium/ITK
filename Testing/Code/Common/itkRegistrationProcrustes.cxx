//-------------------------------------------
//
//  Example of the registration hierarchy
//
//-------------------------------------------


#include <itkPoint.h>
#include <itkVectorContainer.h>
#include <itkRegistrationMethod.h>
#include <itkRegistrationOptimizer.h>
#include <itkRegistrationTransformationAffine.h>
#include <itkRegistrationMetricProcrustes.h>


int main()
{

  const unsigned int Dimension = 3;

  typedef itk::VectorContainer< 
                          unsigned short, 
                          double >               ParameterType;

  typedef vnl_vector< double >                   MeasureType;
  typedef vnl_matrix< double >                   DerivativeType;
                                 
  typedef itk::RegistrationTransformationAffine< 
                          ParameterType,
                          Dimension      >       TransformationType;
  

  typedef itk::RegistrationMetricProcrustes< TransformationType,
                                Dimension >       MetricType;

  typedef MetricType::TargetType         TargetType;
  typedef MetricType::ReferenceType      ReferenceType;

  typedef itk::RegistrationOptimizer< 
                                MetricType >      OptimizerType;

  typedef itk::RegistrationMethod< MetricType,
                               OptimizerType >   RegistrationType;

  TargetType::Pointer         target            = TargetType::New();
  ReferenceType::Pointer      reference         = ReferenceType::New();
  TransformationType::Pointer transformation    = TransformationType::New();
  RegistrationType::Pointer   registration      = RegistrationType::New();

  registration->SetTarget( target );
  registration->SetReference( reference );
  registration->SetTransformation( transformation );

  registration->StartRegistration();

  return 0;

}



