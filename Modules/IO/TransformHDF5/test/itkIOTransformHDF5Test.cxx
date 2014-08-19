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

#include <iostream>
#include <fstream>
#include "itkHDF5TransformIOFactory.h"
#include "itkTransformFileWriter.h"
#include "itkTransformFileReader.h"
#include "itkAffineTransform.h"
#include "itkTransformFactory.h"
#include "itkSimilarity2DTransform.h"
#include "itkBSplineTransform.h"
#include "itksys/SystemTools.hxx"

//Transforms from Filtering/DisplacementField/include
#include "itkBSplineExponentialDiffeomorphicTransform.h"
#include "itkBSplineSmoothingOnUpdateDisplacementFieldTransform.h"
#include "itkConstantVelocityFieldTransform.h"
#include "itkDisplacementFieldTransform.h"
#include "itkGaussianExponentialDiffeomorphicTransform.h"
#include "itkGaussianSmoothingOnUpdateDisplacementFieldTransform.h"

template < typename ScalarType, typename DisplacementTransformType >
static int ReadWriteTest(const char * const fileName)
{
  // Now test reading/writing many different transform types.
  typename itk::TransformFileReaderTemplate<ScalarType>::Pointer
    reader = itk::TransformFileReaderTemplate<ScalarType>::New();

  typename itk::TransformFileWriterTemplate<ScalarType>::Pointer
    writer = itk::TransformFileWriterTemplate<ScalarType>::New();

  writer->SetFileName( fileName );
  reader->SetFileName( fileName );

  typename DisplacementTransformType::Pointer displacementTransform = DisplacementTransformType::New();
    {
    typedef typename DisplacementTransformType::DisplacementFieldType FieldType;
    typename FieldType::Pointer field = FieldType::New(); //This is based on itk::Image

    const int dimLength = 20;
    typename FieldType::SizeType size;
    size.Fill( dimLength );
    typename FieldType::IndexType start;
    start.Fill( 0 );
    typename FieldType::RegionType region;
    region.SetSize( size );
    region.SetIndex( start );
    field->SetRegions( region );
    typename FieldType::SpacingType spacing;
    spacing.Fill( 1.2 );
    field->SetSpacing( spacing );
    field->Allocate();

    typename DisplacementTransformType::OutputVectorType zeroVector;
    zeroVector.Fill( 0 );
    field->FillBuffer( zeroVector );

    displacementTransform->SetDisplacementField( field );
    }

  try
    {
    writer->AddTransform( displacementTransform );
    writer->Update();
    //std::cout << std::endl;
    //std::cout << "Testing read : " << std::endl;
    reader->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error while saving the transforms" << std::endl;
    std::cerr << excp << std::endl;
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;
}

template<typename ScalarType>
static int oneTest(const char *const goodname,const char *const badname)
{
  typedef typename itk::AffineTransform<ScalarType,4>  AffineTransformType;
  typedef typename itk::AffineTransform<ScalarType,10> AffineTransformTypeNotRegistered;
  typename AffineTransformType::Pointer        affine = AffineTransformType::New();

  itk::ObjectFactoryBase::RegisterFactory(itk::HDF5TransformIOFactory::New() );

  // Set it's parameters
  typename AffineTransformType::ParametersType p = affine->GetParameters();
  for ( unsigned int i = 0; i < p.GetSize(); i++ )
    {
    p[i] = i;
    }
  affine->SetParameters ( p );
  p = affine->GetFixedParameters ();
  for ( unsigned int i = 0; i < p.GetSize(); i++ )
    {
    p[i] = i;
    }
  affine->SetFixedParameters ( p );
  typename itk::TransformFileWriterTemplate<ScalarType>::Pointer
    writer = itk::TransformFileWriterTemplate<ScalarType>::New();
  typename itk::TransformFileReaderTemplate<ScalarType>::Pointer
    reader = itk::TransformFileReaderTemplate<ScalarType>::New();

  writer->AddTransform(affine);

  writer->SetFileName( goodname );
  reader->SetFileName( goodname );

  // Testing writing std::cout << "Testing write : ";
  affine->Print ( std::cout );
  try
    {
    writer->Update();
    std::cout << std::endl;
    std::cout << "Testing read : " << std::endl;
    reader->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error while saving the transforms" << std::endl;
    std::cerr << excp << std::endl;
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }


  try
    {
    typename itk::TransformFileReaderTemplate<ScalarType>::TransformListType * list = reader->GetTransformList();
    typename itk::TransformFileReaderTemplate<ScalarType>::TransformListType::iterator lit = list->begin();
    while ( lit != list->end() )
      {
      (*lit)->Print ( std::cout );
      ++lit;
      }
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error while saving the transforms" << std::endl;
    std::cerr << excp << std::endl;
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }


  std::cout << "Creating bad writer" << std::endl;
  typename AffineTransformTypeNotRegistered::Pointer Bogus = AffineTransformTypeNotRegistered::New();

  // Set it's parameters
  p = Bogus->GetParameters();
  for ( unsigned int i = 0; i < p.GetSize(); i++ )
    {
    p[i] = i;
    }
  Bogus->SetParameters ( p );
  p = Bogus->GetFixedParameters ();
  for ( unsigned int i = 0; i < p.GetSize(); i++ )
    {
    p[i] = i;
    }
  Bogus->SetFixedParameters ( p );

  typename itk::TransformFileWriterTemplate<ScalarType>::Pointer
    badwriter = itk::TransformFileWriterTemplate<ScalarType>::New();
  typename itk::TransformFileReaderTemplate<ScalarType>::Pointer
    badreader = itk::TransformFileReaderTemplate<ScalarType>::New();
  badwriter->AddTransform(Bogus);
  badwriter->SetFileName(badname);
  badreader->SetFileName(badname);

  // Testing writing
  std::cout << "Testing write of non register transform : " << std::endl;
  std::cout << std::flush;
  try
    {
    badwriter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error while saving the transforms" << std::endl;
    std::cerr << excp << std::endl;
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }

  // Testing writing
  std::cout << "Testing read of non register transform : " << std::endl;
  std::cout << std::flush;
  bool caught = false;
  try
    {
    badreader->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    caught = true;
    std::cout << "Caught exception as expected" << std::endl;
    std::cout << excp << std::endl;
    }
  if ( !caught )
    {
    std::cerr << "Did not catch non registered transform" << std::endl;
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }

  int error_sum = 0;
  error_sum += ReadWriteTest< float, itk::BSplineSmoothingOnUpdateDisplacementFieldTransform<float,2> >(goodname);
  error_sum += ReadWriteTest< float, itk::BSplineSmoothingOnUpdateDisplacementFieldTransform<float,3> >(goodname);
  error_sum += ReadWriteTest< double, itk::BSplineSmoothingOnUpdateDisplacementFieldTransform<double,2> >(goodname);
  error_sum += ReadWriteTest< double, itk::BSplineSmoothingOnUpdateDisplacementFieldTransform<double,3> >(goodname);

  error_sum += ReadWriteTest< float, itk::ConstantVelocityFieldTransform<float,2> >(goodname);
  error_sum += ReadWriteTest< float, itk::ConstantVelocityFieldTransform<float,3> >(goodname);
  error_sum += ReadWriteTest< double, itk::ConstantVelocityFieldTransform<double,2> >(goodname);
  error_sum += ReadWriteTest< double, itk::ConstantVelocityFieldTransform<double,3> >(goodname);

  error_sum += ReadWriteTest< float, itk::DisplacementFieldTransform<float, 2> >(goodname);
  error_sum += ReadWriteTest< float, itk::DisplacementFieldTransform<float, 3> >(goodname);
  error_sum += ReadWriteTest< double, itk::DisplacementFieldTransform<double, 2> >(goodname);
  error_sum += ReadWriteTest< double, itk::DisplacementFieldTransform<double, 3> >(goodname);

  error_sum += ReadWriteTest< float, itk::BSplineSmoothingOnUpdateDisplacementFieldTransform<float,2> >(goodname);
  error_sum += ReadWriteTest< float, itk::BSplineSmoothingOnUpdateDisplacementFieldTransform<float,3> >(goodname);
  error_sum += ReadWriteTest< double, itk::BSplineSmoothingOnUpdateDisplacementFieldTransform<double,2> >(goodname);
  error_sum += ReadWriteTest< double, itk::BSplineSmoothingOnUpdateDisplacementFieldTransform<double,3> >(goodname);

  error_sum += ReadWriteTest< float, itk::GaussianExponentialDiffeomorphicTransform<float,2> >(goodname);
  error_sum += ReadWriteTest< float, itk::GaussianExponentialDiffeomorphicTransform<float,3> >(goodname);
  error_sum += ReadWriteTest< double, itk::GaussianExponentialDiffeomorphicTransform<double,2> >(goodname);
  error_sum += ReadWriteTest< double, itk::GaussianExponentialDiffeomorphicTransform<double,3> >(goodname);

  error_sum += ReadWriteTest< float, itk::GaussianSmoothingOnUpdateDisplacementFieldTransform<float,2> >(goodname);
  error_sum += ReadWriteTest< float, itk::GaussianSmoothingOnUpdateDisplacementFieldTransform<float,3> >(goodname);
  error_sum += ReadWriteTest< double, itk::GaussianSmoothingOnUpdateDisplacementFieldTransform<double,2> >(goodname);
  error_sum += ReadWriteTest< double, itk::GaussianSmoothingOnUpdateDisplacementFieldTransform<double,3> >(goodname);

  if( error_sum > 0 )
    {
    std::cerr << "Atleast 1 transform type could not be read/written " << error_sum << std::endl;
    std::cout << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "[PASSED]" << std::endl;

  return EXIT_SUCCESS;
}

int itkIOTransformHDF5Test(int argc, char* argv[])
{
  if (argc > 1)
    {
    itksys::SystemTools::ChangeDirectory(argv[1]);
    }
  const int result1 =  oneTest<float>("Transforms_float.h5", "TransformsBad_float.h5" );
  const int result2 =  oneTest<double>("Transforms_double.hdf5", "TransformsBad_double.hdf5" );

  return ( !( result1 == EXIT_SUCCESS && result2 == EXIT_SUCCESS) );
}
