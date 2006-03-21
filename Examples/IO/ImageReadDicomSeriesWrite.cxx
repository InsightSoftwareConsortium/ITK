/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageReadDicomSeriesWrite.cxx
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

#ifdef __BORLANDC__
#define ITK_LEAN_AND_MEAN
#endif

#include "itkGDCMImageIO.h"
#include "itkNumericSeriesFileNames.h"
#include "itkImageFileReader.h"
#include "itkImageSeriesWriter.h"
#include "itkMetaDataObject.h"

#include <vector>
#include <itksys/SystemTools.hxx>

//  Software Guide : BeginLatex
//
//  This example illustrates how to read a 3D image from a non DICOM file and write it as a series of DICOM slices.
//  with some changed header information. Header
//  
//  Please note that modifying the content of a DICOM header is a very risky
//  operation. The Header contains fundamental information about the patient
//  and therefore its consistency must be protected from any data corruption.
//  Before attempting to modify the DICOM headers of your files, you must make
//  sure that you have a very good reason for doing so, and that you can ensure
//  that this information change will not result in a lower quality of health
//  care to be delivered to the patient.
//
//  \index{DICOM!Writing Series}
//
//  Software Guide : EndLatex 


int main( int argc, char* argv[] )
{

  if( argc < 3 )
    {
    std::cerr << "Usage: " << argv[0];
    std::cerr << "InputImage  OutputDicomDirectory" << std::endl;
    return EXIT_FAILURE;
    }


  typedef signed short    PixelType;
  const unsigned int      Dimension = 3;

  typedef itk::Image< PixelType, Dimension >      ImageType;
  typedef itk::ImageFileReader< ImageType >       ReaderType;

  ReaderType::Pointer reader = ReaderType::New();

  reader->SetFileName( argv[1] );

  try
    {
    reader->Update();
    }
  catch (itk::ExceptionObject &excp)
    {
    std::cerr << "Exception thrown while writing the image" << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }




  typedef itk::GDCMImageIO                        ImageIOType;
  typedef itk::NumericSeriesFileNames             NamesGeneratorType;

  ImageIOType::Pointer gdcmIO = ImageIOType::New();

  const char * outputDirectory = argv[2];

  itksys::SystemTools::MakeDirectory( outputDirectory );


  typedef signed short    OutputPixelType;
  const unsigned int      OutputDimension = 2;

  typedef itk::Image< OutputPixelType, OutputDimension >    Image2DType;

  typedef itk::ImageSeriesWriter< 
                         ImageType, Image2DType >  SeriesWriterType;

  NamesGeneratorType::Pointer namesGenerator = NamesGeneratorType::New();

  itk::MetaDataDictionary & dict = gdcmIO->GetMetaDataDictionary();
  std::string tagkey, value;
  tagkey = "0008|0060"; // Modality
  value = "MR";
  itk::EncapsulateMetaData<std::string>(dict, tagkey, value );
  tagkey = "0008|0008"; // Image Type
  value = "DERIVED\\SECONDARY";
  itk::EncapsulateMetaData<std::string>(dict, tagkey, value);
  tagkey = "0008|0064"; // Conversion Type
  value = "DV";
  itk::EncapsulateMetaData<std::string>(dict, tagkey, value);


  SeriesWriterType::Pointer seriesWriter = SeriesWriterType::New();

  seriesWriter->SetInput( reader->GetOutput() );
  seriesWriter->SetImageIO( gdcmIO );


  ImageType::RegionType region = 
     reader->GetOutput()->GetLargestPossibleRegion();

  ImageType::IndexType start = region.GetIndex();
  ImageType::SizeType  size  = region.GetSize();


  std::string format = outputDirectory;
  
  format += "/image%03d.dcm";

  namesGenerator->SetSeriesFormat( format.c_str() );

  namesGenerator->SetStartIndex( start[2] );
  namesGenerator->SetEndIndex( start[2] + size[2] - 1 );
  namesGenerator->SetIncrementIndex( 1 );


  seriesWriter->SetFileNames( namesGenerator->GetFileNames() );

  
  try
    {
    seriesWriter->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception thrown while writing the series " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }


  return EXIT_SUCCESS;
}

