//-------------------------------------------
//
//  Example of the registration hierarchy
//
//-------------------------------------------


#include <itkImage.h>
#include <itkVectorContainer.h>
#include <itkRegistrationTransform.h>
#include <itkAffineTransform.h>
#include <itkRegistrationMapperImage.h>
#include <itkSimilarityRegistrationMetric.h>
#include <itkOptimizer.h>


int main()
{

  typedef itk::Image<unsigned char,2>             ImageType;

  typedef itk::VectorContainer< unsigned short,
                                double >      ParameterType;

  typedef itk::AffineTransform< 2 > TransfromType;
  
  typedef itk::RegistrationMapperImage< ImageType,
                                   TransfromType
                                             >  MapperType;

  typedef itk::SimilarityRegistrationMetric< ImageType,
                                             ImageType,
                                             MapperType,
                                             double,
                                             double >  MetricType;

  typedef itk::Optimizer< MetricType
                                              > OptimizerType;

  typedef itk::RegistrationTransform< MetricType,
                                   OptimizerType >   RegistrationType;

  ImageType::Pointer          reference         = ImageType::New();
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

  reference->SetLargestPossibleRegion( region );
  reference->SetRequestedRegion( region );
  reference->SetBufferedRegion( region );
  reference->Allocate();

  target->SetLargestPossibleRegion( region );
  target->SetRequestedRegion( region );
  target->SetBufferedRegion( region );
  target->Allocate();

  registration->SetTarget( target );
  registration->SetReference( reference );
  registration->SetTransformation( transformation );

  return 0;

}



