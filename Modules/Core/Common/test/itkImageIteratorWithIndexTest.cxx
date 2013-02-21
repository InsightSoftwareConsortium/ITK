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

#include "itkImageRegionIteratorWithIndex.h"
#include "itkNumericTraits.h"


template <typename TPixelType>
class itkImageIteratorWithIndexTestIteratorTester
{

  public:
    typedef TPixelType                  PixelType;

    typedef itk::Image< PixelType, 3 > ImageType;

    typedef itk::ImageRegionIteratorWithIndex< ImageType > IteratorType;

    typedef itk::ImageRegionConstIteratorWithIndex< ImageType > ConstIteratorType;

    itkImageIteratorWithIndexTestIteratorTester( const PixelType & value )
    {
      m_Image = ImageType::New();

      typename ImageType::SizeType size;
      size.Fill(100);

      typename ImageType::IndexType start;
      start.Fill(0);

      typename ImageType::RegionType region;
      region.SetSize( size );
      region.SetIndex( start );

      m_Image->SetRegions( region );
      m_Image->Allocate();

      m_Image->FillBuffer( value );
    }

    bool TestIterator()
    {
     IteratorType it( m_Image, m_Image->GetBufferedRegion() );
     it.GoToBegin();
     while( !it.IsAtEnd() )
       {
       PixelType value = it.Get();
       PixelType testValue = value * static_cast<typename itk::NumericTraits<PixelType>::ValueType>( 2 );
       it.Set( testValue);
       if( it.Get() != testValue )
        {
        std::cerr << "TestIterator failed!" << std::endl;
        return false;
        }
       ++it;
       }
     return true;
    }

    bool TestConstIterator()
    {
     ConstIteratorType it( m_Image, m_Image->GetBufferedRegion() );
     it.GoToBegin();
     while( !it.IsAtEnd() )
       {
       PixelType value = it.Get();
       if( value != it.Get() ) // check repeatibility
         {
         std::cerr << "TestConstIterator failed!" << std::endl;
         return false;
         }
       ++it;
       }
     return true;
    }

  private:

    typename ImageType::Pointer m_Image;

};

int itkImageIteratorWithIndexTest(int, char* [] )
{

  bool testPassed = true; // let's be optimistic

  // Instantiate image of various types and
  // test the iterators on them

  std::cout << "Testing with Image< char, 3 > " << std::endl;
  itkImageIteratorWithIndexTestIteratorTester< char > TesterC( 10 );
  if( TesterC.TestIterator() == false )
    {
    testPassed = false;
    }
  if( TesterC.TestConstIterator() == false )
    {
    testPassed = false;
    }

  std::cout << "Testing with Image< uint8_t, 3 > " << std::endl;
  itkImageIteratorWithIndexTestIteratorTester< uint8_t > TesterUC( 10 );
  if( TesterUC.TestIterator() == false )
    {
    testPassed = false;
    }
  if( TesterUC.TestConstIterator() == false )
    {
    testPassed = false;
    }

  std::cout << "Testing with Image< short, 3 > " << std::endl;
  itkImageIteratorWithIndexTestIteratorTester< short > TesterS( 10 );
  if( TesterS.TestIterator() == false )
    {
    testPassed = false;
    }
  if( TesterS.TestConstIterator() == false )
    {
    testPassed = false;
    }

  std::cout << "Testing with Image< uint16_t, 3 > " << std::endl;
  itkImageIteratorWithIndexTestIteratorTester< uint16_t > TesterUS( 10 );
  if( TesterUS.TestIterator() == false )
    {
    testPassed = false;
    }
  if( TesterUS.TestConstIterator() == false )
    {
    testPassed = false;
    }

  std::cout << "Testing with Image< int, 3 > " << std::endl;
  itkImageIteratorWithIndexTestIteratorTester< int > TesterI( 10 );
  if( TesterI.TestIterator() == false )
    {
    testPassed = false;
    }
  if( TesterI.TestConstIterator() == false )
    {
    testPassed = false;
    }

  std::cout << "Testing with Image< unsigned int, 3 > " << std::endl;
  itkImageIteratorWithIndexTestIteratorTester< unsigned int > TesterUI( 10 );
  if( TesterUI.TestIterator() == false )
    {
    testPassed = false;
    }
  if( TesterUI.TestConstIterator() == false )
    {
    testPassed = false;
    }

  std::cout << "Testing with Image< float, 3 > " << std::endl;
  itkImageIteratorWithIndexTestIteratorTester< float > TesterF( 10.0 );
  if( TesterF.TestIterator() == false )
    {
    testPassed = false;
    }
  if( TesterF.TestConstIterator() == false )
    {
    testPassed = false;
    }

  std::cout << "Testing with Image< double, 3 > " << std::endl;
  itkImageIteratorWithIndexTestIteratorTester< double > TesterD( 10.0 );
  if( TesterD.TestIterator() == false )
    {
    testPassed = false;
    }
  if( TesterD.TestConstIterator() == false )
    {
    testPassed = false;
    }

  std::cout << "Testing with Image< itk::Vector<char,4>, 3 > " << std::endl;
  typedef itk::Vector<char,4> VC;
  VC vc;
  vc.Fill( 127 );
  itkImageIteratorWithIndexTestIteratorTester< VC > TesterVC( vc );
  if( TesterVC.TestIterator() == false )
    {
    testPassed = false;
    }
  if( TesterVC.TestConstIterator() == false )
    {
    testPassed = false;
    }

  std::cout << "Testing with Image< itk::Vector<uint8_t,4>, 3 > " << std::endl;
  typedef itk::Vector<uint8_t,4> VUC;
  VUC vuc;
  vuc.Fill( 10 );
  itkImageIteratorWithIndexTestIteratorTester< VUC > TesterVUC( vuc );
  if( TesterVUC.TestIterator() == false )
    {
    testPassed = false;
    }
  if( TesterVUC.TestConstIterator() == false )
    {
    testPassed = false;
    }

  std::cout << "Testing with Image< itk::Vector<short,4>, 3 > " << std::endl;
  typedef itk::Vector<short,4> VS;
  VS vs;
  vs.Fill( 10 );
  itkImageIteratorWithIndexTestIteratorTester< VS > TesterVS( vs );
  if( TesterVS.TestIterator() == false )
    {
    testPassed = false;
    }
  if( TesterVS.TestConstIterator() == false )
    {
    testPassed = false;
    }

  std::cout << "Testing with Image< itk::Vector<uint16_t,4>, 3 > " << std::endl;
  typedef itk::Vector<uint16_t,4> VUS;
  VUS vus;
  vus.Fill( 10 );
  itkImageIteratorWithIndexTestIteratorTester< VUS > TesterVUS( vus );
  if( TesterVUS.TestIterator() == false )
    {
    testPassed = false;
    }
  if( TesterVUS.TestConstIterator() == false )
    {
    testPassed = false;
    }

  std::cout << "Testing with Image< itk::Vector<int,4>, 3 > " << std::endl;
  typedef itk::Vector<int,4> VI;
  VI vi;
  vi.Fill( 10 );
  itkImageIteratorWithIndexTestIteratorTester< VI > TesterVI( vi );
  if( TesterVI.TestIterator() == false )
    {
    testPassed = false;
    }
  if( TesterVI.TestConstIterator() == false )
    {
    testPassed = false;
    }

  std::cout << "Testing with Image< itk::Vector<unsigned int,4>, 3 > " << std::endl;
  typedef itk::Vector<unsigned int,4> VUI;
  VUI vui;
  vui.Fill( 10 );
  itkImageIteratorWithIndexTestIteratorTester< VUI > TesterVUI( vui );
  if( TesterVUI.TestIterator() == false )
    {
    testPassed = false;
    }
  if( TesterVUI.TestConstIterator() == false )
    {
    testPassed = false;
    }

  std::cout << "Testing with Image< itk::Vector<float,4>, 3 > " << std::endl;
  typedef itk::Vector<float,4> VF;
  VF vf;
  vf.Fill( 10 );
  itkImageIteratorWithIndexTestIteratorTester< VF > TesterVF( vf );
  if( TesterVF.TestIterator() == false )
    {
    testPassed = false;
    }
  if( TesterVF.TestConstIterator() == false )
    {
    testPassed = false;
    }

  std::cout << "Testing with Image< itk::Vector<double,4>, 3 > " << std::endl;
  typedef itk::Vector<double,4> VD;
  VD vd;
  vd.Fill( 10 );
  itkImageIteratorWithIndexTestIteratorTester< VD > TesterVD( vd );
  if( TesterVD.TestIterator() == false )
    {
    testPassed = false;
    }
  if( TesterVD.TestConstIterator() == false )
    {
    testPassed = false;
    }

  if ( !testPassed )
    {
    std::cout << "Failed" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Success" << std::endl;
  return EXIT_SUCCESS;

}
