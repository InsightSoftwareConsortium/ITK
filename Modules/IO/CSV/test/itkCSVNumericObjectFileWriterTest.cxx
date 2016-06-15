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

#include "itkCSVNumericObjectFileWriter.h"
#include "itkLightProcessObject.h"
#include "itkTestingMacros.h"

int itkCSVNumericObjectFileWriterTest( int argc, char *argv[] )
{
  if ( argc < 2 )
    {
    std::cerr << "Usage: " << argv[0] << " Filename" << std::endl;
    return EXIT_FAILURE;
    }

  double nan = std::numeric_limits<double>::quiet_NaN();
  const unsigned int ARows = 3;
  const unsigned int ACols = 6;

  typedef itk::Array2D<double> ArrayType;
  ArrayType array(ARows,ACols);
  array[0][0] = nan;
  array[0][1] = 1e+09;
  array[0][2] = 5;
  array[0][3] = 9;
  array[0][4] = 6.1;
  array[0][5] = nan;
  array[1][0] = 99;
  array[1][1] = 0;
  array[1][2] = 3.75;
  array[1][3] = 0.008;
  array[1][4] = nan;
  array[1][5] = nan;
  array[2][0] = 1;
  array[2][1] = 24.22231242343532;
  array[2][2] = 9;
  array[2][3] = 5.6;
  array[2][4] = nan;
  array[2][5] = 3e+10;

  typedef itk::CSVNumericObjectFileWriter
                          <double, ARows, ACols > Array2DWriterType;
  Array2DWriterType::Pointer array_writer = Array2DWriterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( array_writer, CSVNumericObjectFileWriter, LightProcessObject );

  // should throw an exception as there is no input file nor any object
  // to write out
  bool caught = false;
  try
    {
    array_writer->Update();
    }
  catch (itk::ExceptionObject& exp)
    {
    caught = true;
    std::cerr << "Exception caught!" << std::endl;
    std::cerr << "This is an expected exception as there is no input"
              << "file provided." << std::endl;
    std::cerr << exp << std::endl;
    }
  if ( !caught )
    {
    std::cerr << "An exception should have been caught here as there"
              << "is no input file provided. Test fails." << std::endl;
    return EXIT_FAILURE;
    }

  std::string filename = argv[1];
  array_writer->SetFileName( filename );

  // should throw an exception as there is no input object
  caught = false;
  try
    {
    array_writer->Update();
    }
  catch (itk::ExceptionObject & exp)
    {
    caught = true;
    std::cerr << "Exception caught!" << std::endl;
    std::cerr << "This is an expected exception as there is no"
              << " object to write out." << std::endl;
    std::cerr << exp << std::endl;
    }
  if ( !caught )
    {
    std::cerr << "An exception should have been caught here as there is no"
              << "input object to write out. Test fails." << std::endl;
    return EXIT_FAILURE;
    }

  array_writer->SetInput( &array );

  // write out the Array2D object
  try
    {
    array_writer->Update();
    }
  catch (itk::ExceptionObject& exp)
    {
    std::cerr << "Exception caught!" << std::endl;
    std::cerr << exp << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int VMRows = 3;
  const unsigned int VMCols = 4;

  typedef vnl_matrix<double> vnlMatrixType;
  vnlMatrixType vnlmatrix(VMRows,VMCols);
  vnlmatrix[0][0] = nan;
  vnlmatrix[0][1] = 1e+09;
  vnlmatrix[0][2] = 5;
  vnlmatrix[0][3] = 9;
  vnlmatrix[1][0] = 99;
  vnlmatrix[1][1] = 0;
  vnlmatrix[1][2] = 3.75;
  vnlmatrix[1][3] = 0.008;
  vnlmatrix[2][0] = 1;
  vnlmatrix[2][1] = 2.2;
  vnlmatrix[2][2] = 9;
  vnlmatrix[2][3] = 5.6;

  typedef itk::CSVNumericObjectFileWriter
                          <double, VMRows, VMCols> vnlMatrixWriterType;
  vnlMatrixWriterType::Pointer vnl_matrix_writer = vnlMatrixWriterType::New();

  vnl_matrix_writer->SetFileName( filename );
  vnl_matrix_writer->SetInput( &vnlmatrix );
  vnl_matrix_writer->ColumnHeadersPushBack( "vnlMatrixObject" );
  vnl_matrix_writer->ColumnHeadersPushBack( "Col1" );
  vnl_matrix_writer->ColumnHeadersPushBack( "Col2" );
  vnl_matrix_writer->ColumnHeadersPushBack( "Col3" );
  vnl_matrix_writer->ColumnHeadersPushBack( "Col4" );
  vnl_matrix_writer->RowHeadersPushBack( "Row1" );
  vnl_matrix_writer->RowHeadersPushBack( "Row2" );

  //write out the vnl_matrix object
  try
    {
    vnl_matrix_writer->Update();
    }
  catch (itk::ExceptionObject& exp)
    {
    std::cerr << "Exception caught!" << std::endl;
    std::cerr << exp << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int NRows = 3;
  const unsigned int NCols = 3;

  typedef itk::Matrix<double,NRows,NCols> fixedMatrixType;
  fixedMatrixType fixedmatrix;
  fixedmatrix[0][0] = nan;
  fixedmatrix[0][1] = 1e+09;
  fixedmatrix[0][2] = 5;
  fixedmatrix[1][0] = 99;
  fixedmatrix[1][1] = 0;
  fixedmatrix[1][2] = 3.75;
  fixedmatrix[2][0] = 1;
  fixedmatrix[2][1] = 2.2;
  fixedmatrix[2][2] = 9;

  std::vector<std::string> ColumnHeaders;
  std::vector<std::string> RowHeaders;
  ColumnHeaders.push_back( "itkMatrixObject" );
  ColumnHeaders.push_back( "Col1" );
  ColumnHeaders.push_back( "Col2" );
  ColumnHeaders.push_back( "Col3" );
  RowHeaders.push_back( "Row1" );
  RowHeaders.push_back( "Row2" );
  RowHeaders.push_back( "Row3" );

  typedef itk::CSVNumericObjectFileWriter
          <double, NRows,NCols> fixedMatrixWriterType;
  fixedMatrixWriterType::Pointer fixed_matrix_writer = fixedMatrixWriterType::New();

  fixed_matrix_writer->SetFileName( filename );
  fixed_matrix_writer->SetInput( &fixedmatrix );
  fixed_matrix_writer->SetColumnHeaders( ColumnHeaders );
  fixed_matrix_writer->SetRowHeaders( RowHeaders );

  //write out the itkMatrix object
  try
    {
    fixed_matrix_writer->Update();
    }
  catch (itk::ExceptionObject& exp)
    {
    std::cerr << "Exception caught!" << std::endl;
    std::cerr << exp << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
