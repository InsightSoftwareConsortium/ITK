/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRawImageIOTest5.cxx
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

#include <fstream>
#include "itkRawImageIO.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImage.h"
#include "itkImageRegionIterator.h"

template <class TPixel>
class RawImageReaderAndWriter
{
public:

  typedef unsigned char PixelType;

  typedef itk::RawImageIO< PixelType, 2 > RawImageIOType;

  typedef itk::Image< PixelType, 2 > ImageType;


public:
  
  RawImageReaderAndWriter()
    {
    m_Image = ImageType::New();

    typename ImageType::RegionType     region;
    typename ImageType::SizeType       size;
    typename ImageType::IndexType      start;

    start.Fill(0);
    size[0] = 16;  // To fill the range of 8 bits image 
    size[1] = 16;

    region.SetSize( size );
    region.SetIndex( start );

    m_Image->SetRegions( region );
    m_Image->Allocate();

    PixelType value = itk::NumericTraits< PixelType >::Zero;

    // Fill the image with incremental values.
    typedef itk::ImageRegionIterator< ImageType > IteratorType;
    IteratorType it( m_Image, region );

    it.GoToBegin();

    while( !it.IsAtEnd() )
      {
      it.Set( value );
      ++value;
      ++it;
      }

    m_Error = false;

    }
  
  void Write()
    {
    typedef itk::ImageFileWriter< ImageType >  WriterType;
    WriterType::Pointer  writer  = WriterType::New();

    writer->SetFileName( m_FileName.c_str() );
    writer->SetInput( m_Image );
    
    RawImageIOType::Pointer rawImageIO = RawImageIOType::New();
    writer->SetImageIO( rawImageIO );

    writer->Update();
    }

  void Read()
    {
    typedef itk::ImageFileReader< ImageType >  ReaderType;
    ReaderType::Pointer  reader  = ReaderType::New();
    reader->SetFileName( m_FileName.c_str() );
    
    RawImageIOType::Pointer rawImageIO = RawImageIOType::New();
    reader->SetImageIO( rawImageIO );

    unsigned int dim[2] = {16,16};
    double spacing[2] = {1.0, 1.0};
    double origin[2] = {0.0,0.0};

    for(unsigned int i=0; i<2; i++)
      {
      rawImageIO->SetDimensions(i,dim[i]);
      rawImageIO->SetSpacing(i,spacing[i]);
      rawImageIO->SetOrigin(i,origin[i]);
      }

    rawImageIO->SetHeaderSize(0);
    rawImageIO->SetByteOrderToLittleEndian();
    rawImageIO->SetNumberOfComponents(1);

    reader->Update();

    ImageType::ConstPointer image = reader->GetOutput();


    //
    // Verify the content of the image.
    // 
    typedef itk::ImageRegionConstIterator< ImageType > ConstIteratorType;

    ConstIteratorType it1( m_Image, m_Image->GetLargestPossibleRegion() );
    ConstIteratorType it2( image, image->GetLargestPossibleRegion() );

    it1.GoToBegin();
    it2.GoToBegin();

    m_Error = false;

    while( it1.IsAtEnd() )
      {
      if( it1.Get() != it2.Get() )
        {
        m_Error = true;
        break;
        }
      ++it1;
      ++it2;
      }


    }

  void SetFileName( const std::string & filename )
    {
    m_FileName = filename;
    }

  bool GetError() const
    {
    return m_Error;
    }
  
private:

  std::string m_FileName;

  typename ImageType::Pointer m_Image;

  bool m_Error;

};




int itkRawImageIOTest5(int argc, char*argv[])
{

  if(argc < 2)
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " TemporaryDirectoryName" << std::endl;
    return EXIT_FAILURE;
    }


  std::string directory = argv[1];


  //
  // Test the pixel type = "char"
  //
  std::cout << "Testing for pixel type = char " << std::endl;


  RawImageReaderAndWriter< char >   tester1;


  std::string filename = directory + "/RawImageIOTest5a.raw";

  tester1.SetFileName( filename );


  try
    {
    tester1.Write();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception caught while writing char type." << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  
  try
    {
    tester1.Read();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception caught while reading char type." << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  if( tester1.GetError() )
    {
    std::cerr << "Error while comparing the char type images." << std::endl;
    return EXIT_FAILURE;
    }




  //
  // Test the pixel type = "signed char"
  //
  std::cout << "Testing for pixel type = signed char " << std::endl;

  RawImageReaderAndWriter< signed char >   tester2;

  filename = directory + "/RawImageIOTest5b.raw";

  tester2.SetFileName( filename );


  try
    {
    tester2.Write();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception caught while writing signed char type." << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  
  try
    {
    tester2.Read();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception caught while reading signed char type." << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  if( tester2.GetError() )
    {
    std::cerr << "Error while comparing the signed char type images." << std::endl;
    return EXIT_FAILURE;
    }





  //
  // Test the pixel type = "unsigned char"
  //
  std::cout << "Testing for pixel type = unsigned char " << std::endl;

  RawImageReaderAndWriter< unsigned char >   tester3;


  filename = directory + "/RawImageIOTest5c.raw";

  tester3.SetFileName( filename );


  try
    {
    tester3.Write();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception caught while writing unsigned char type." << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  
  try
    {
    tester3.Read();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception caught while reading unsigned char type." << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  if( tester3.GetError() )
    {
    std::cerr << "Error while comparing the unsigned char type images." << std::endl;
    return EXIT_FAILURE;
    }






  std::cout << "Test PASSED !!" << std::endl << std::endl;

  return EXIT_SUCCESS;

}


