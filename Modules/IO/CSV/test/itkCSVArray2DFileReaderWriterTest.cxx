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
#include "itkCSVArray2DFileReader.h"
#include "itkCSVNumericObjectFileWriter.h"
#include <iostream>
#include "itkMath.h"

const double epsilon = 1e-20;

// function for comparing matrices
template <typename T>
bool testArray(const itk::Array2D<T> & m1, const itk::Array2D<T> & m2)
{
  bool pass = true;

  if ( m1.rows() != m2.rows() || m1.cols() != m2.cols() )
    {
    pass = false;
    return pass;
    }

  for (unsigned int i = 0; i < m1.rows(); i++)
    {
    for (unsigned int j = 0; j < m1.cols(); j++)
      {
      // We need to test whether m1 is a NaN and/or m2 is a NaN.
      // If they are both NaN, then they are the same.
      // If only one is NaN, then the comparison should fail.
      // Without such a test, the comparison of the difference being greater than epsilon will pass.
      // The equality and inequality predicates are non-signaling so x = x returning false can be used to test if x is a quiet NaN.
      bool m1_isNaN = (itk::Math::ExactlyEquals(m1[i][j], m1[i][j]));
      bool m2_isNaN = (itk::Math::ExactlyEquals(m2[i][j], m2[i][j]));
      if( (m1_isNaN && !m2_isNaN) || (!m1_isNaN && m2_isNaN) )
      {
        pass = false;
        return pass;
      }
      if (std::fabs(m1[i][j] - m2[i][j]) > epsilon)
        {
        pass = false;
        return pass;
        }
      }
    }
  return pass;
}

int itkCSVFileReaderWriterTest_Func(int argc, char *argv[], bool headers)
{
  double nan = std::numeric_limits<double>::quiet_NaN();

  typedef itk::Array2D<double> MatrixType;
  const unsigned int ARows = 3;
  const unsigned int ACols = 6;
  MatrixType matrix(ARows,ACols);
  matrix[0][0] = nan;
  matrix[0][1] = 1e+09;
  matrix[0][2] = 5;
  matrix[0][3] = 9;
  matrix[0][4] = 6.1;
  matrix[0][5] = nan;
  matrix[1][0] = 99;
  matrix[1][1] = 0;
  matrix[1][2] = 3.75;
  matrix[1][3] = 0.008;
  matrix[1][4] = nan;
  matrix[1][5] = nan;
  matrix[2][0] = 1;
  matrix[2][1] = 2.2;
  matrix[2][2] = 9;
  matrix[2][3] = 5.6;
  matrix[2][4] = nan;
  matrix[2][5] = 3e+10;

  // write out the array2D object
  typedef itk::CSVNumericObjectFileWriter<double, ARows, ACols> WriterType;
  WriterType::Pointer writer = WriterType::New();

  if (argc < 2 )
    {
    std::cout << "Wrong number of arguments given." << std::endl;
    return EXIT_FAILURE;
    }
  const std::string filename = argv[1];
  writer->SetFileName( filename );
  writer->SetInput( &matrix );

  if (headers)
    {
    std::vector<std::string> ColumnHeaders;
    std::vector<std::string> RowHeaders;
    ColumnHeaders.push_back( "itkArray2DObject" );
    ColumnHeaders.push_back( "Col1" );
    ColumnHeaders.push_back( "Col2" );
    ColumnHeaders.push_back( "Col3" );
    ColumnHeaders.push_back( "Col4" );
    ColumnHeaders.push_back( "Col5" );
    ColumnHeaders.push_back( "Col6" );
    RowHeaders.push_back( "Row1" );
    RowHeaders.push_back( "Row2" );
    RowHeaders.push_back( "Row3" );
    writer->SetRowHeaders( RowHeaders );
    writer->SetColumnHeaders( ColumnHeaders );
    }

  try
    {
    writer->Write();
    }
  catch (itk::ExceptionObject& exp)
    {
    std::cerr << "Exception caught!" << std::endl;
    std::cerr << exp << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::CSVArray2DFileReader<double > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( filename );
  reader->SetFieldDelimiterCharacter(',');
  reader->SetStringDelimiterCharacter('"');
  reader->SetHasColumnHeaders(headers);
  reader->SetHasRowHeaders(headers);
  reader->UseStringDelimiterCharacterOff();

  // read the file
  try
    {
    reader->Update();
    }
  catch (itk::ExceptionObject& exp)
    {
    std::cerr << "Exception caught!" << std::endl;
    std::cerr << exp << std::endl;
    return EXIT_FAILURE;
    }

  reader->Print(std::cout);

  typedef itk::CSVArray2DDataObject<double> DataFrameObjectType;
  DataFrameObjectType::Pointer dfo = reader->GetOutput();
  MatrixType test_matrix = dfo->GetMatrix();

  std::cout << "Actual array: " << std::endl;
  std::cout << matrix << std::endl;
  std::cout << "Test array: " << std::endl;
  std::cout << test_matrix;

  if ( !testArray(matrix,test_matrix) )
    {
    std::cerr << "Matrices are not the same! Test Failed!" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}

/** This test illustrates basic use for writing an Array2D object to a csv file
 * and reading from that file into an Array2D data frame object. There are two
 * examples: one that writes and reades a file without row and column headers,
 * and one that does. */

int itkCSVArray2DFileReaderWriterTest(int argc, char *argv[])
{
  if ( argc < 2 )
    {
    std::cerr << "Usage: " << argv[0] << " Filename" << std::endl;
    return EXIT_FAILURE;
    }

  // test reading and writing data without headers
  std::cout << std::endl << "Test Without Headers" << std::endl;
  int fail1 = itkCSVFileReaderWriterTest_Func(argc,argv, false);

  // test reading and writing data with headers
  std::cout << std::endl << "Test With Headers." << std::endl;
  int fail2 = itkCSVFileReaderWriterTest_Func(argc,argv, true);

  if ( fail1 || fail2 )
    {
    std::cerr << "Test fails!" << std::endl;
    return EXIT_FAILURE;
    }

 return EXIT_SUCCESS;
}
