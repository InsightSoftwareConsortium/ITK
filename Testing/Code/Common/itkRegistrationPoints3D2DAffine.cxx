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
#include <itkRegistrationTransformationAffine.h>
#include <itkRegistrationMetric.h>
#include <itkRegistrationMapper.h>
#include <itkRegistrationOptimizer.h>


int main()
{

  typedef itk::Point<float,2>                    Point2DType;
  typedef itk::Point<float,3>                    Point3DType;

  typedef itk::VectorContainer< unsigned long, 
                                Point2DType    > TargetType;

  typedef itk::VectorContainer< unsigned long, 
                                Point3DType >    ReferenceType;

  typedef itk::VectorContainer< unsigned short, 
                                double >         ParameterType;

  typedef vnl_vector< double >                   MeasureType;
  typedef vnl_matrix< double >                   DerivativeType;
                                 
  typedef itk::RegistrationTransformationAffine< 3
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



