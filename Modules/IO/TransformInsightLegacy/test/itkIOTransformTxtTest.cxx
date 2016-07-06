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
#include "itkTestingMacros.h"
#include "itkTxtTransformIOFactory.h"
#include "itkTransformFileWriter.h"
#include "itkTransformFileReader.h"
#include "itkAffineTransform.h"
#include "itkTransformFactory.h"
#include "itkSimilarity2DTransform.h"
#include "itkBSplineTransform.h"
#include "itksys/SystemTools.hxx"

template<typename ScalarType>
static int oneTest(const std::string & outputDirectory, const char *goodname,const char *badname)
{
  unsigned int i;
  typedef itk::AffineTransform<ScalarType,4>  AffineTransformType;
  typedef itk::AffineTransform<ScalarType,10> AffineTransformTypeNotRegistered;
  typename AffineTransformType::Pointer affine = AffineTransformType::New();
  typename AffineTransformType::InputPointType cor;

  itk::ObjectFactoryBase::RegisterFactory(itk::TxtTransformIOFactory::New() );

  // Set it's parameters
    {
    typename AffineTransformType::ParametersType p = affine->GetParameters();
    for ( i = 0; i < p.GetSize(); i++ )
      {
      p[i] = i;
      }
    affine->SetParameters ( p );
    }
    {
    typename AffineTransformType::FixedParametersType p = affine->GetFixedParameters ();
    for ( i = 0; i < p.GetSize(); i++ )
      {
      p[i] = i;
      }
    affine->SetFixedParameters ( p );
    }
  typename itk::TransformFileWriterTemplate<ScalarType>::Pointer writer;
  typename itk::TransformFileReaderTemplate<ScalarType>::Pointer reader;

  reader = itk::TransformFileReaderTemplate<ScalarType>::New();
  writer = itk::TransformFileWriterTemplate<ScalarType>::New();
  writer->AddTransform(affine);

  writer->SetFileName( outputDirectory + goodname );
  reader->SetFileName( outputDirectory + goodname );

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
    const typename itk::TransformFileReaderTemplate<ScalarType>::TransformListType * list = reader->GetTransformList();
    typename itk::TransformFileReaderTemplate<ScalarType>::TransformListType::const_iterator lit = list->begin();
    while ( lit != list->end() )
      {
      (*lit)->Print ( std::cout );
      ++lit;
      }

    if ( list->size() != 1 )
      {
      std::cerr << "Failure: Read too many transforms!" << std::endl;
      return EXIT_FAILURE;
      }

    if( dynamic_cast<AffineTransformType*>(list->front().GetPointer()) == ITK_NULLPTR )
      {
      std::cerr << "Failure to dynamic_cast read transform!" << std::endl;
      return EXIT_FAILURE;
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
    {
    typename AffineTransformType::ParametersType p = Bogus->GetParameters();
    for ( i = 0; i < p.GetSize(); i++ )
      {
      p[i] = i;
      }
    Bogus->SetParameters ( p );
    }
    {
    typename AffineTransformType::FixedParametersType p = Bogus->GetFixedParameters ();
    for ( i = 0; i < p.GetSize(); i++ )
      {
      p[i] = i;
      }
    Bogus->SetFixedParameters ( p );
    }

  typename itk::TransformFileWriterTemplate<ScalarType>::Pointer badwriter;
  typename itk::TransformFileReaderTemplate<ScalarType>::Pointer badreader;
  badreader = itk::TransformFileReaderTemplate<ScalarType>::New();
  badwriter = itk::TransformFileWriterTemplate<ScalarType>::New();
  badwriter->AddTransform( Bogus );
  badwriter->SetFileName( outputDirectory + badname );
  badreader->SetFileName( outputDirectory + badname );

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
  std::cout << "[PASSED]" << std::endl;

  return EXIT_SUCCESS;
}

//
// test endless loop bug in transform reader, triggered by no
// EOL at end of file.
// This test will exercise this reported bug:
// http://public.kitware.com/Bug/view.php?id=7028
template<typename ScalarType>
int
secondTest( const std::string & outputDirectory )
{
  std::filebuf fb;
  fb.open( (outputDirectory + "IllegalTransform.txt").c_str(), std::ios::out );
  std::ostream os( &fb );
  os << "#Insight Transform File V1.0"
     << std::endl
     << "#Transform 0"
     << std::endl
     << "Transform: AffineTransform_double_10_10"
     << std::endl
     << "Parameters: "
     << "  0 1 2 3 4 5 6 7 8 9 10 11 12"
     << " 13 14 15 16 17 18 19 20 21 22"
     << std::endl
     << "FixedParameters: 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18";
  fb.close();
  typename itk::TransformFileReaderTemplate<ScalarType>::Pointer reader;
  reader = itk::TransformFileReaderTemplate<ScalarType>::New();
  reader->SetFileName( outputDirectory + "IllegalTransform.txt" );
  try
    {
    reader->Update();
    std::cerr << "FAILED to throw expected exception" << std::endl;
    const typename itk::TransformFileReaderTemplate<ScalarType>::TransformListType * list = reader->GetTransformList();
    typename itk::TransformFileReaderTemplate<ScalarType>::TransformListType::const_iterator lit =
      list->begin();
    while ( lit != list->end() )
      {
      (*lit)->Print ( std::cout );
      ++lit;
      }
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "EXPECTED Error while reading the transforms" << std::endl;
    std::cerr << excp << std::endl;
    std::cout << "[SUCCESS]" << std::endl;
    return EXIT_SUCCESS;
    }
  return EXIT_FAILURE;
}


int
templatelessTest( const std::string & outputDirectory )
{
  const std::string outputFile = outputDirectory + "itkIOTransformTxtTestRigid2DTransform.tfm";

  typedef itk::Rigid2DTransform< float > TransformType;
  TransformType::Pointer transform = TransformType::New();

  itk::TransformFileWriter::Pointer writer = itk::TransformFileWriter::New();
  writer->SetInput( transform );
  writer->SetFileName( outputFile );
  TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  return EXIT_SUCCESS;
}


int itkIOTransformTxtTest(int argc, char* argv[])
{
  if (argc < 2)
    {
    std::cerr << "Usage: "
              << argv[0]
              << " outputDirectory"
              << std::endl;

    itksys::SystemTools::ChangeDirectory(argv[1]);
    }
  const std::string outputDirectory = std::string(argv[1]) + "/";
  const int result1 =  oneTest<float>( outputDirectory, "Transforms_float.txt", "TransformsBad_float.txt" );
  const int result2 =  secondTest<float>( outputDirectory );

  const int result3 =  oneTest<double>( outputDirectory, "Transforms_double.txt", "TransformsBad_double.txt" );
  const int result4 =  secondTest<double>( outputDirectory );

  const int result5 = templatelessTest( outputDirectory );

  return  ( result1 == EXIT_SUCCESS && result2 == EXIT_SUCCESS &&
            result3 == EXIT_SUCCESS && result4 == EXIT_SUCCESS &&
            result5 == EXIT_SUCCESS )? EXIT_SUCCESS : EXIT_FAILURE
          ;
}
