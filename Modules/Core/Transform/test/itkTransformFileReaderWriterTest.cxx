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


#include "itkTransformFileReader.h"
#include "itkTransformFileWriter.h"
#include "itkTransformIOFactory.h"
#include "itkAffineTransform.h"
#include "itkTransformFactory.h"
#include "itkScaleVersor3DTransform.h"
#include "itkBSplineTransform.h"

int itkTransformFileReaderWriterTest( int argc, char *argv[] )
{
  itk::TransformFactory<itk::ScaleVersor3DTransform<float> >::RegisterDefaultTransforms();
  itk::TransformFactory<itk::ScaleVersor3DTransform<float> >::RegisterTransform ();
  if( argc < 3 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputTransformFile ";
    std::cerr << " outputTransformFile ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  itk::TransformIOBase::Pointer transformIO =
    itk::TransformIOFactory::CreateTransformIO(argv[1],itk::TransformIOFactory::ReadMode);
  transformIO->Print(std::cout,0);
  transformIO = itk::TransformIOFactory::CreateTransformIO(argv[1],itk::TransformIOFactory::WriteMode);
  transformIO->Print(std::cout,0);

  typedef itk::TransformFileReader        TransformReaderType;
  typedef itk::TransformFileWriter        TransformWriterType;

  typedef itk::AffineTransform<double, 3> AffineTransformType;
  typedef AffineTransformType::Pointer    AffineTransformPointer;

  TransformReaderType::Pointer transformReader = TransformReaderType::New();
  TransformWriterType::Pointer transformWriter = TransformWriterType::New();

  std::cout << "Reader class = "
            << transformReader->GetNameOfClass()
            << " Writer class = "
            << transformWriter->GetNameOfClass()
            << "Reader base = "
            << dynamic_cast<TransformReaderType::Superclass *>(transformReader.GetPointer())->GetNameOfClass()
            << dynamic_cast<TransformWriterType::Superclass *>(transformWriter.GetPointer())->GetNameOfClass()
            << std::endl;
  std::cout << "Loading Transform: " << argv[1] << std::endl;

  try
    {
    // first try, to trigger exception
    transformReader->Update();
    }
  catch(itk::ExceptionObject &excp)
    {
    std::cerr << "Expected exception (no filename)" << std::endl
              << excp << std::endl;
    }
  transformReader->SetFileName("transform.garbage");
  try
    {
    // first try, trigger exception for transformio not found
    transformReader->Update();
    }
  catch(itk::ExceptionObject &excp)
    {
    std::cerr << "Expected exception (no transformio that can read file)"
              << excp << std::endl;
    }

  //DEBUG
  std::cout << "Reading " << argv[1] << std::endl;


  transformReader->SetFileName( argv[1] );
  std::cout << "Filename: " << transformReader->GetFileName() << std::endl;
  transformReader->Update();

  typedef TransformReaderType::TransformListType * TransformListType;
  typedef TransformReaderType::TransformPointer    TransformPointer;

  TransformListType transforms = transformReader->GetTransformList();

  TransformReaderType::TransformListType::const_iterator tit = transforms->begin();

  AffineTransformPointer affine_transform1 = AffineTransformType::New();

  if( !strcmp((*tit)->GetNameOfClass(),"AffineTransform") )
    {

    TransformPointer transform_read = tit->GetPointer();
    affine_transform1 = dynamic_cast< AffineTransformType * >( transform_read.GetPointer() );

    if( affine_transform1 )
      {
      std::cout << "Successful Read" << std::endl;
      }

    else
      {
      std::cerr << "Error reading Affine Transform" << std::endl;
      return EXIT_FAILURE;
      }
    }

  //
  // Now Write the transform:
  //
  try
    {
    // first try, to trigger exception
    transformWriter->Update();
    }
  catch(itk::ExceptionObject &excp)
    {
    std::cerr << "Expected exception (no filename)" << std::endl
              << excp << std::endl;
    }
  transformWriter->SetFileName("transform.garbage");
  try
    {
    // first try, trigger exception for transformio not found
    transformWriter->Update();
    }
  catch(itk::ExceptionObject &excp)
    {
    std::cerr << "Expected exception (no transformio that can write file)"
              << excp << std::endl;
    }

  transformWriter->SetFileName( argv[2] );
  std::cout << "Filename: " << transformWriter->GetFileName() << std::endl;
  unsigned int precision(transformWriter->GetPrecision());
  std::cout << "transform writer precision " << precision;
  transformWriter->SetPrecision(precision);
  bool appendMode = transformWriter->GetAppendMode();
  std::cout << " Append mode " << appendMode;
  transformWriter->SetAppendOn();
  transformWriter->SetAppendOff();
  transformWriter->SetAppendMode(appendMode);

  transformWriter->SetInput( affine_transform1 );

  transformWriter->Update();

  //
  // And read it again to compare
  //
  TransformReaderType::Pointer transformReader2 = TransformReaderType::New();

  std::cout << "Loading Transform back: " << argv[2] << std::endl;

  transformReader2->SetFileName( argv[2] );
  transformReader2->Update();

  TransformListType transforms2 = transformReader2->GetTransformList();

  TransformReaderType::TransformListType::const_iterator tit2 = transforms2->begin();

  AffineTransformPointer affine_transform2;

  if( !strcmp((*tit2)->GetNameOfClass(),"AffineTransform") )
    {
    typedef AffineTransformType::Pointer AffineTransformPointer;

    TransformPointer transform_read = tit->GetPointer();
    affine_transform2 = dynamic_cast< AffineTransformType * >( transform_read.GetPointer() );

    if( affine_transform2 )
      {
      std::cout << "Successful Read" << std::endl;
      }
    else
      {
      std::cerr << "Error reading Affine Transform" << std::endl;
      return EXIT_FAILURE;
      }
    }

  const double tolerance = 1e-6;

  std::cout << "Testing STANDARD parameters" << std::endl;

  AffineTransformType::ParametersType parameters1 = affine_transform1->GetParameters();
  AffineTransformType::ParametersType parameters2 = affine_transform2->GetParameters();

  for( unsigned int k = 0; k < parameters1.Size(); k++ )
    {
    if( vnl_math_abs( parameters1[k] - parameters2[k] ) > tolerance )
      {
      std::cerr << "ERROR: parameter " << k << " differs above tolerace" << std::endl;
      std::cerr << "Expected parameters  = " << parameters1 << std::endl;
      std::cerr << "but Found parameters = " << parameters2 << std::endl;
      return EXIT_FAILURE;
      }
    }


  std::cout << "Testing FIXED parameters" << std::endl;

  AffineTransformType::ParametersType fixedparameters1 = affine_transform1->GetFixedParameters();
  AffineTransformType::ParametersType fixedparameters2 = affine_transform2->GetFixedParameters();

  for( unsigned int j = 0; j < fixedparameters1.Size(); j++ )
    {
    if( vnl_math_abs( fixedparameters1[j] - fixedparameters2[j] ) > tolerance )
      {
      std::cerr << "ERROR: parameter " << j << " differs above tolerace" << std::endl;
      std::cerr << "Expected parameters  = " << fixedparameters1 << std::endl;
      std::cerr << "but Found parameters = " << fixedparameters2 << std::endl;
      return EXIT_FAILURE;
      }
    }


  std::cout << "Test PASSED!" << std::endl;

  return EXIT_SUCCESS;
}
