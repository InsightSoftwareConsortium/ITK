/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageIteratorWithIndexTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include <iostream>

#include "itkImage.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageRegionConstIteratorWithIndex.h"



template <typename TPixelType>
class IteratorTester 
{

  public:
    typedef TPixelType                  PixelType;
    
    typedef itk::Image< PixelType, 3 > ImageType;

    typedef itk::ImageRegionIteratorWithIndex< 
                                        ImageType > IteratorType;
    
    typedef itk::ImageRegionConstIteratorWithIndex< 
                                        ImageType > ConstIteratorType;

    IteratorTester( const PixelType & value )
      {
      m_Image = ImageType::New();
      
      ImageType::SizeType size;
      size.Fill(100);
      
      ImageType::IndexType start;
      start.Fill(0);
      
      ImageType::RegionType region;
      region.SetSize( size );
      region.SetIndex( start );
      
      m_Image->SetRegions( region );
      m_Image->Allocate();
      
      m_Image->FillBuffer( value );
      }

    void TestIterator()
     {
     IteratorType it( m_Image, m_Image->GetBufferedRegion() );
     it.GoToBegin();
     while( !it.IsAtEnd() )
       {
       PixelType value = it.Get();  
       it.Set( value ); 
       ++it;
       }
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
         return false;
         }
       ++it;
       }
     return true;
     }
    
  private:
    
    typename ImageType::Pointer m_Image;

};




int itkImageIteratorWithIndexTest(int, char**)
{


  bool testPassed = true; // let's be optimistic

  // Instantiate image of various types and 
  // test the iterators on them

  std::cout << "Testing with Image< char, 3 > " << std::endl;
  IteratorTester< char > TesterC( 255 );
  TesterC.TestIterator();
  TesterC.TestConstIterator();

  std::cout << "Testing with Image< unsigned char, 3 > " << std::endl;
  IteratorTester< unsigned char > TesterUC( 255 );
  TesterUC.TestIterator();
  TesterUC.TestConstIterator();

  std::cout << "Testing with Image< short, 3 > " << std::endl;
  IteratorTester< short > TesterS( 255 );
  TesterS.TestIterator();
  TesterS.TestConstIterator();

  std::cout << "Testing with Image< unsigned short, 3 > " << std::endl;
  IteratorTester< unsigned short > TesterUS( 255 );
  TesterUS.TestIterator();
  TesterUS.TestConstIterator();

  std::cout << "Testing with Image< int, 3 > " << std::endl;
  IteratorTester< int > TesterI( 255 );
  TesterI.TestIterator();
  TesterI.TestConstIterator();

  std::cout << "Testing with Image< unsigned int, 3 > " << std::endl;
  IteratorTester< unsigned int > TesterUI( 255 );
  TesterUI.TestIterator();
  TesterUI.TestConstIterator();

  std::cout << "Testing with Image< float, 3 > " << std::endl;
  IteratorTester< float > TesterF( 255.0 );
  TesterF.TestIterator();
  TesterF.TestConstIterator();

  std::cout << "Testing with Image< double, 3 > " << std::endl;
  IteratorTester< double > TesterD( 255.0 );
  TesterD.TestIterator();
  TesterD.TestConstIterator();

  std::cout << "Testing with Image< itk::Vector<char,4>, 3 > " << std::endl;
  typedef itk::Vector<char,4> VC;
  VC vc;
  vc.Fill( 255 );
  IteratorTester< VC > TesterVC( vc );
  TesterVC.TestIterator();
  TesterVC.TestConstIterator();

  std::cout << "Testing with Image< itk::Vector<unsigned char,4>, 3 > " << std::endl;
  typedef itk::Vector<unsigned char,4> VUC;
  VUC vuc;
  vuc.Fill( 255 );
  IteratorTester< VUC > TesterVUC( vuc );
  TesterVUC.TestIterator();
  TesterVUC.TestConstIterator();

  std::cout << "Testing with Image< itk::Vector<short,4>, 3 > " << std::endl;
  typedef itk::Vector<short,4> VS;
  VS vs;
  vs.Fill( 255 );
  IteratorTester< VS > TesterVS( vs );
  TesterVS.TestIterator();
  TesterVS.TestConstIterator();

  std::cout << "Testing with Image< itk::Vector<unsigned short,4>, 3 > " << std::endl;
  typedef itk::Vector<unsigned short,4> VUS;
  VUS vus;
  vus.Fill( 255 );
  IteratorTester< VUS > TesterVUS( vus );
  TesterVUS.TestIterator();
  TesterVUS.TestConstIterator();

  std::cout << "Testing with Image< itk::Vector<int,4>, 3 > " << std::endl;
  typedef itk::Vector<int,4> VI;
  VI vi;
  vi.Fill( 255 );
  IteratorTester< VI > TesterVI( vi );
  TesterVI.TestIterator();
  TesterVI.TestConstIterator();

  std::cout << "Testing with Image< itk::Vector<unsigned int,4>, 3 > " << std::endl;
  typedef itk::Vector<unsigned int,4> VUI;
  VUI vui;
  vui.Fill( 255 );
  IteratorTester< VUI > TesterVUI( vui );
  TesterVUI.TestIterator();
  TesterVUI.TestConstIterator();

  std::cout << "Testing with Image< itk::Vector<float,4>, 3 > " << std::endl;
  typedef itk::Vector<float,4> VF;
  VF vf;
  vf.Fill( 255 );
  IteratorTester< VF > TesterVF( vf );
  TesterVF.TestIterator();
  TesterVF.TestConstIterator();

  std::cout << "Testing with Image< itk::Vector<double,4>, 3 > " << std::endl;
  typedef itk::Vector<double,4> VD;
  VD vd;
  vd.Fill( 255 );
  IteratorTester< VD > TesterVD( vd );
  TesterVD.TestIterator();
  TesterVD.TestConstIterator();


  
  if ( !testPassed )
    {
    std::cout << "Failed" << std::endl;
    return EXIT_FAILURE;
    }
    
  std::cout << "Success" << std::endl;
  return EXIT_SUCCESS;

}


