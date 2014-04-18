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

const double epsilon = 1e-10;

// function for displaying vectors
template <typename T>
void PrintVector(const std::vector<T>& v1)
{
  if ( v1.empty() )
    {
    std::cout << "Empty vector." << std::endl;
    }
  else
    {
    typename std::vector<T >::const_iterator it;
    for (it = v1.begin(); it != v1.end(); ++it)
      {
      std::cout << *it << " ";
      }
    std::cout << std::endl;
    }
}

// function for comparing matrices
template <typename T>
bool testMatrix(const itk::Array2D<T>& m1, const itk::Array2D<T>& m2)
{
  bool pass = true;

  for (unsigned int i = 0; i < m1.rows(); i++)
    {
    for (unsigned int j = 0; j < m1.cols(); j++)
      {
      if (std::fabs(m1[i][j] - m2[i][j]) > epsilon)
        {
        pass = false;
        }
      }
    }
  return pass;
}

// function for comparing numeric vectors
template <typename T>
bool testVector(const std::vector<T>& v1, const std::vector<T>& v2)
{
  bool pass = true;

  for (unsigned int i = 0; i < v1.size(); i++)
    {
    if (std::fabs(v1[i] - v2[i]) > epsilon)
      {
      pass = false;
      }
    }
  return pass;
}

// function for comparing string vectors
bool testStringVector (const std::vector<std::string>& v1,
                       const std::vector<std::string>& v2)
{
  bool pass = true;

  for (unsigned int i = 0; i < v1.size(); i++)
    {
    if (v1[i].compare( v2[i] ) != 0 )
      {
      pass = false;
      }
    }
  return pass;
}

// function for comparing numeric values
template <typename T>
bool testValue (const T& test, const T& real)
{
  bool pass = true;
  if (std::fabs(test - real) > epsilon)
    {
    pass = false;
    }
  return pass;
}


int itkCSVArray2DFileReaderTest (int argc, char *argv[])
{
 if ( argc < 2 )
   {
   std::cerr << "Usage: " << argv[0] << " Filename" << std::endl;
   return EXIT_FAILURE;
   }

 double nan = std::numeric_limits<double>::quiet_NaN();

 // Read and Parse the data
 typedef itk::CSVArray2DFileReader<double> ReaderType;
 ReaderType::Pointer reader = ReaderType::New();

 std::string filename = "nonexistentfilename.csv";
 reader->SetFileName ( filename );
 reader->SetFieldDelimiterCharacter( ',' );
 reader->SetStringDelimiterCharacter( '"' );
 reader->HasColumnHeadersOn();
 reader->HasRowHeadersOn();
 reader->UseStringDelimiterCharacterOn();

 // Try Non-existent filename
 bool caught = false;
 try
   {
   reader->Parse();
   }
 catch( itk::ExceptionObject & exp )
   {
   caught = true;
   std::cerr << "Expected Exception caught!" << std::endl;
   std::cerr << exp << std::endl;
   }
 if ( !caught )
   {
   std::cerr << "An exception should have been caught here as the filename does"
             << "not exist! Test fails." << std::endl;
   return EXIT_FAILURE;
   }

 filename = argv[1];
 reader->SetFileName( filename );

 try
   {
   reader->Update();
   }
 catch(itk::ExceptionObject& exp)
   {
   std::cerr << "Exception caught!" << std::endl;
   std::cerr << exp << std::endl;
   return EXIT_FAILURE;
   }

 // Exercise the print function
 reader->Print(std::cout);

 typedef itk::CSVArray2DDataObject<double> DataFrameObjectType;
 DataFrameObjectType::Pointer dfo = reader->GetOutput();

 // Test the matrix
 typedef itk::Array2D<double> MatrixType;
 MatrixType test_matrix = dfo->GetMatrix();

 MatrixType matrix(4,6);
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
 matrix[3][0] = 3.4;
 matrix[3][1] = 497;
 matrix[3][2] = nan;
 matrix[3][3] = nan;
 matrix[3][4] = nan;
 matrix[3][5] = nan;

 if ( !testMatrix(matrix,test_matrix) )
   {
   std::cerr << "Matrices are not the same! Test Failed!" << std::endl;
   return EXIT_FAILURE;
   }

 // Test Row Names
 std::vector<std::string> test_row_names;
 test_row_names = dfo->GetRowHeaders();

 std::vector<std::string> row_names;
 row_names.push_back( "Jan" );
 row_names.push_back( "Feb" );
 row_names.push_back( "Mar,April" );
 row_names.push_back( "May" );

 if ( !testStringVector(row_names, test_row_names) )
   {
   std::cerr << "Row names do not match! Test failed!" << std::endl;
   return EXIT_FAILURE;
   }
 std::cout << "Row names: ";
 PrintVector( test_row_names );

 // Test Column Names
 std::vector<std::string> test_col_names;
 test_col_names = dfo->GetColumnHeaders();

 std::vector<std::string> col_names;
 col_names.push_back( "Africa" );
 col_names.push_back( "Asia" );
 col_names.push_back( "Aus" );
 col_names.push_back( "US,Can" );

 if ( !testStringVector(col_names, test_col_names) )
   {
   std::cerr << "Column names do not match! Test failed!" << std::endl;
   return EXIT_FAILURE;
   }
 std::cout << "Col names: ";
 PrintVector( test_col_names );


 // Test a row (using index access)
 std::vector<double> test_row_1;
 test_row_1 = dfo->GetRow( 1 );

 std::vector <double> row_1;
 row_1.push_back( 99 );
 row_1.push_back( 0 );
 row_1.push_back( 3.75 );
 row_1.push_back( 0.008 );

 if ( !testVector(row_1,test_row_1) )
   {
   std::cerr << "The vectors do not match! Test failed!" << std::endl;
   return EXIT_FAILURE;
   }
 std::cout << "Row 1 : ";
 PrintVector( test_row_1 );

 // Test a row (using string access)
 std::vector<double> test_row_Jan;
 test_row_Jan = dfo->GetRow( "Jan" );

 std::vector<double> row_Jan;
 row_Jan.push_back( nan );
 row_Jan.push_back( 1e+9 );
 row_Jan.push_back( 5 );
 row_Jan.push_back( 9 );
 row_Jan.push_back( 6.1 );

 if ( !testVector(row_Jan,test_row_Jan) )
   {
   std::cerr << "The vectors do not match! Test failed!" << std::endl;
   return EXIT_FAILURE;
   }
 std::cout << "Row Jan : ";
 PrintVector( test_row_Jan );

 // Test a column (using index)
 std::vector<double> test_col_2;
 test_col_2 = dfo->GetColumn( 2 );

 std::vector<double> col_2;
 col_2.push_back( 5 );
 col_2.push_back( 3.75 );
 col_2.push_back( 9 );

 if ( !testVector(col_2,test_col_2) )
   {
   std::cerr << "The vectors do not match! Test failed!" << std::endl;
   return EXIT_FAILURE;
   }

 std::cout << "Column 2 : ";
 PrintVector( col_2 );

 // Test a column (using string access)
 std::vector<double> test_col_Africa;
 test_col_Africa = dfo->GetColumn( "Africa" );

 std::vector<double> col_Africa;
 col_Africa.push_back( nan );
 col_Africa.push_back( 99 );
 col_Africa.push_back( 1 );

 if ( !testVector(col_Africa,test_col_Africa) )
   {
   std::cerr << "The vectors do not match! Test failed!" << std::endl;
   return EXIT_FAILURE;
   }
 std::cout << "Column Africa : ";
 PrintVector( col_Africa );


 // Test a row that does not exist
 try
   {
   std::vector <double> test_row_Oct;
   test_row_Oct = dfo->GetRow( "Oct" );
   if( !test_row_Oct.empty() )
     {
     std::cerr << "Row should be empty! Test Failed!";
     return EXIT_FAILURE;
     }
   }
 catch (itk::ExceptionObject & exp)
   {
   std::cerr << "Expected Exception caught!" << std::endl;
   std::cerr << exp << std::endl;
   }

 try
   {
   // Test column that does not exist
   std::vector<double> test_col_Eur;
   test_col_Eur = dfo->GetColumn( "Eur" );
   if( !test_col_Eur.empty() )
     {
     std::cerr << "Column should be empty! Test Failed!";
     return EXIT_FAILURE;
     }
   }
 catch (itk::ExceptionObject &exp)
   {
   std::cerr << "Expected Exception caught!" << std::endl;
   std::cerr << exp << std::endl;
   }

 double test_item;
 double actual_item;

 test_item = dfo->GetData(2,2);
 actual_item = 9;
 if ( !testValue(test_item,actual_item) )
   {
   std::cerr << "Wrong value! Test Failed!";
   return EXIT_FAILURE;
   }
 std::cout << "Data(2,2) : " << test_item << std::endl;

 test_item = dfo->GetData("Mar,April","US,Can");
 actual_item = 5.6;
 if ( !testValue(test_item,actual_item) )
   {
   std::cerr << "Wrong value! Test failed!";
   return EXIT_FAILURE;
   }
 std::cout << "Data('Mar,April','US,Can') : " << test_item << std::endl;

 test_item = dfo->GetRowData("Feb",1);
 actual_item = 0;
 if ( !testValue(test_item,actual_item) )
   {
   std::cerr << "Wrong value! Test failed!";
   return EXIT_FAILURE;
   }
 std::cout << "Data(Feb,1) : " << test_item << std::endl;

 test_item = dfo->GetColumnData("Asia",0);
 actual_item = 1e+09;
 if ( !testValue(test_item,actual_item) )
   {
   std::cerr << "Wrong value! Test failed!";
   return EXIT_FAILURE;
   }
 std::cout << "Data(Asia,0) : " <<test_item << std::endl;

 // Test using non existing data items
 try
   {
   test_item = dfo->GetData(5,3);
   if ( !testValue(test_item,nan) )
     {
     std::cerr << "Wrong value! Test failed!";
     return EXIT_FAILURE;
     }
   }
 catch (itk::ExceptionObject & exp)
   {
   std::cerr << "Expected Exception caught!" << std::endl;
   std::cerr << exp << std::endl;
   }

 try
   {
   double test_item1 = dfo->GetData("Feb","Europe");
   if ( !testValue(test_item1, nan) )
     {
     std::cerr << "Wrong value! Test failed!";
     }
   }
 catch (itk::ExceptionObject & exp)
   {
   std::cerr << "Expected Exception caught!" << std::endl;
   std::cerr << exp << std::endl;
   }

 // Test the () operator
 test_item = (*dfo)(0,4);
 if ( !testValue(test_item, 6.1) )
   {
   std::cerr << "Wrong value! Test failed!";
   return EXIT_FAILURE;
   }
 std::cout << "Data(0,4) : " << test_item << std::endl;

 test_item = (*dfo)("Feb","Aus");
 if ( !testValue(test_item, 3.75) )
   {
   std::cerr << "Wrong value! Test failed!";
   return EXIT_FAILURE;
   }
 std::cout << "Data(Feb,Aus) : " << test_item << std::endl;

 return EXIT_SUCCESS;
}
