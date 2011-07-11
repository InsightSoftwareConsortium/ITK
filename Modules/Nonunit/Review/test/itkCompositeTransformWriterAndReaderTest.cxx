/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkCompositeTransformWriteAndReadTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include <iostream>

#include "itkAffineTransform.h"
#include "itkCompositeTransform.h"
#include "itkCompositeTransformWriter.h"
#include "itkCompositeTransformReader.h"
#include "itkArray2D.h"
#include "itkTransformFileReader.h"

namespace {

const double epsilon = 1e-10;

template <typename TMatrix>
bool testMatrix( const TMatrix & m1, const TMatrix & m2 )
  {
  bool pass=true;
  for ( unsigned int i = 0; i < TMatrix::RowDimensions; i++ )
    {
    for ( unsigned int j = 0; j < TMatrix::ColumnDimensions; j++ )
      {
      if( vcl_fabs( m1[i][j] - m2[i][j] ) > epsilon )
        pass=false;
      }
  }
  return pass;
  }

} //namespace

/******/

int itkCompositeTransformWriterAndReaderTest(int argc, char *argv[] )
{
  const unsigned int NDimensions = 2;

  /* Temporary file path */
  if( argc < 2 )
    {
    std::cout << "Usage: " << argv[0] << "TemporaryFilePath" << std::endl;
    return EXIT_FAILURE;
    }
  std::string filePath( argv[1] );
  std::string filePrefix( "/compositeTransformWriteAndReadTest_" );

  /* Create composite transform */
  typedef itk::CompositeTransform<double, NDimensions>  CompositeType;
  typedef CompositeType::ScalarType                     ScalarType;

  CompositeType::Pointer compositeTransform = CompositeType::New();

  /* Test obects */
  typedef  itk::Matrix<ScalarType,NDimensions,NDimensions>   Matrix2Type;
  typedef  itk::Vector<ScalarType,NDimensions>               Vector2Type;

  /* Add an affine transform */
  typedef itk::AffineTransform<ScalarType, NDimensions> AffineType;
  AffineType::Pointer affine = AffineType::New();
  Matrix2Type matrix2;
  Vector2Type vector2;
  matrix2[0][0] = 1;
  matrix2[0][1] = 2;
  matrix2[1][0] = 3;
  matrix2[1][1] = 4;
  vector2[0] = 5;
  vector2[1] = 6;
  affine->SetMatrix(matrix2);
  affine->SetOffset(vector2);

  compositeTransform->AddTransform( affine );

  /* 2nd transform */
  AffineType::Pointer affine2 = AffineType::New();
  matrix2[0][0] = 11;
  matrix2[0][1] = 22;
  matrix2[1][0] = 33;
  matrix2[1][1] = 44;
  vector2[0] = 55;
  vector2[1] = 65;
  affine2->SetMatrix(matrix2);
  affine2->SetOffset(vector2);

  compositeTransform->AddTransform( affine2 );

  typedef itk::CompositeTransformWriter< CompositeType::ScalarType, NDimensions >
    WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( compositeTransform );

  /* Write out */
  //Set the filename with full path for the main transform file.
  std::string masterFileName( filePath + filePrefix + "Master1.txt" );
  writer->SetMasterFullPath( masterFileName );
  /* Set the filenames and types for components by just setting their
   * extensions. Each component's number is appended to the master filename,
   * and used for the component's filename.
   * Test writing out components in both text and binary formats. */
  WriterType::FileNamesContainer extensions;
  extensions.push_back(".txt");
  extensions.push_back(".mat");
  writer->SetComponentFileNamesByExtensions( extensions );
  try
    {
    writer->Update();
    }
  catch (itk::ExceptionObject &ex)
    {
    std::cout << "Exception while writing composite transform: " << std::endl;
    std::cout << ex;
    return EXIT_FAILURE;
    }

  /* Read back in */
  typedef itk::CompositeTransformReader<ScalarType, NDimensions> ReaderType;
  ReaderType::Pointer                   reader = ReaderType::New();
  CompositeType::Pointer                readCompositeTransform;
  reader->SetFileName( masterFileName );
  try
    {
    reader->Update();
    }
  catch (itk::ExceptionObject &ex)
    {
    std::cout << "Exception while reading composite transform: " << std::endl;
    std::cout << ex;
    return EXIT_FAILURE;
    }
  readCompositeTransform = reader->GetCompositeTransform();
  if( readCompositeTransform.IsNull() )
    {
    std::cout << "Error getting composite transform from reader." << std::endl;
    return EXIT_FAILURE;
    }

  /* Compare */
  std::cout << "Comparing transforms...";
  if ( readCompositeTransform->GetNumberOfTransforms() !=
        compositeTransform->GetNumberOfTransforms() )
    {
    std::cout << "Number of transforms does not match." << std::endl;
    return EXIT_FAILURE;
    }
  AffineType::ConstPointer origAffine, readAffine;
  for( size_t tn = 0; tn < 2; tn++ )
    {
    origAffine = dynamic_cast<AffineType const *>
      ( compositeTransform->GetNthTransform( tn ).GetPointer() );
    readAffine = dynamic_cast<AffineType const *>
      ( readCompositeTransform->GetNthTransform( tn ).GetPointer() );
    if( ! testMatrix( origAffine->GetMatrix(), readAffine->GetMatrix() ) )
      {
      std::cout << "Transform matrices don't match for tranform "
                << tn << "." << std::endl;
      return EXIT_FAILURE;
      }
    }

  /* Test using regular TranformFileWriter and TransformFileReader with
   * a CompositeTransform. They should throw an exception. This is
   * a tempoarary behavior, until CompositeTransform read/write is moved
   * into those classes directly. Then this test can be removed.
   * This test is here because TransformFileReaderTest cannot include
   * CompositeTransform since it's in the Review module. */
  itk::TransformFileReader::Pointer wrongreader = itk::TransformFileReader::New();
  wrongreader->SetFileName( masterFileName );
  bool caughtException = false;
  try
    {
    wrongreader->Update();
    }
  catch( itk::ExceptionObject & e )
    {
    std::string description( e.GetDescription() );
    if( description.find("CompositeTransformReader") != std::string::npos )
      {
      caughtException = true;
      }
    }
  if( ! caughtException )
    {
    std::cout << "Expected exception while trying to read CompositeTransform "
                 " file with TransformFileReader. " << std::endl;
    return EXIT_FAILURE;
    }

  itk::TransformFileWriter::Pointer wrongwriter =
                                              itk::TransformFileWriter::New();
  caughtException = false;
  try
    {
    wrongwriter->SetInput( compositeTransform );
    }
  catch( itk::ExceptionObject & e )
    {
    std::string description( e.GetDescription() );
    if( description.find("CompositeTransformWriter") != std::string::npos )
      {
      caughtException = true;
      }
    }
  if( ! caughtException )
    {
    std::cout << "Expected exception while trying to set CompositeTransform "
                 " transform in TransformFileWriter. " << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "...success" << std::endl;

  return EXIT_SUCCESS;
}
