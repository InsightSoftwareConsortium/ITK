//-------------------------------------------
//
//  Example of the registration hierarchy
//
//-------------------------------------------


#include <itkVectorContainer.h>
#include <itkRegistrationTransform.h>
#include <itkLBFGSOptimizer.h>
#include <itkAffineTransform.h>
#include <itkProcrustesSimilarityRegistrationMetric.h>


int main()
{

  const unsigned int Dimension = 3;

  typedef itk::VectorContainer< 
                          unsigned short, 
                          double >               ParameterType;

  typedef vnl_vector< double >                   MeasureType;
  typedef vnl_matrix< double >                   DerivativeType;
                                 
  typedef itk::AffineTransform< 
                          Dimension      >       TransformationType;
  

  typedef itk::ProcrustesSimilarityRegistrationMetric< TransformationType,
                                Dimension >       MetricType;

  typedef MetricType::TargetType         TargetType;
  typedef MetricType::ReferenceType      ReferenceType;

  typedef itk::LBFGSOptimizer< 
                                MetricType >      OptimizerType;

  typedef itk::RegistrationTransform< MetricType,
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



