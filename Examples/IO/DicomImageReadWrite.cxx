/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    DicomImageReadWrite.cxx
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

//  Software Guide : BeginLatex
//
//  This example illustrates how to read a single DICOM slice and write it back
//  as another DICOM slice. In the process an intensity rescaling is also
//  applied.
//
//  The following example illustrates explicit instantiating of an 
//  \doxygen{GDCMImageIO} class (in this case a DICOM file format), 
//  setting its parameters and then
//  connecting it to the \doxygen{ImageFileWriter}.
//
//  Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkGDCMImageIO.h"
// Software Guide : EndCodeSnippet

#include <list>
#include <fstream>

int main(int ac, char* av[])
{

  // Verify the number of parameters in the command line
  if(ac < 5)
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << av[0] << " DicomImage OutputDicomImage OutputImage RescalDicomImage\n";
    return EXIT_FAILURE;
    }


  // Software Guide : BeginCodeSnippet
  typedef short InputPixelType;
  typedef itk::Image< InputPixelType, 2 > InputImageType;
  typedef itk::ImageFileReader< InputImageType > ReaderType;
  // Software Guide : EndCodeSnippet

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( av[1] );

  // Software Guide : BeginLatex
  //
  // GDCMImageIO is an ImageIO class for reading and writing DICOM v3 and 
  // ACR/NEMA images. It uses GDCM, an open source package developed by the 
  // CREATIS team at INSA-Lyon \cite{CreatisINSA-Lyon}. 
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::GDCMImageIO        ImageIOType;
  ImageIOType::Pointer gdcmImageIO = ImageIOType::New();
  reader->SetImageIO( gdcmImageIO );
  // Software Guide : EndCodeSnippet

  try
    {
    reader->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in file reader " << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }

  // Software Guide : BeginCodeSnippet
  // Rewrite the image in DICOM format
  //
  typedef itk::ImageFileWriter< InputImageType >  Writer1Type;
  Writer1Type::Pointer writer1 = Writer1Type::New();
  writer1->SetFileName( av[2] );
  
  writer1->SetInput( reader->GetOutput() );
  // Software Guide : EndCodeSnippet
  
  //  Software Guide : BeginLatex
  //
  //  User need to explicitely set the proper image IO (GDCMImageIO)
  //  to the writer filter since the input DICOM dictionary is being 
  //  passed along the writting process. It contains all necessary information
  //  that a valid DICOM should contains, like Patient Name, Patient ID, Institution Name...
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  writer1->SetImageIO( gdcmImageIO );
  // Software Guide : EndCodeSnippet

  try
    {
    writer1->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in file writer " << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }

  // Software Guide : BeginLatex
  //
  //  We will now rescale the image read into a rescaled image one using the 
  //  \doxygen{RescaleIntensityImageFilter}. For this purpose we use
  //  a better suited pixel type: \emph{char} instead of short.
  //  
  // Software Guide : EndLatex

  //  Rescale intensities and rewrite the image in another format
  //  Software Guide : BeginCodeSnippet
  typedef unsigned char WritePixelType;
  typedef itk::Image< WritePixelType, 2 > WriteImageType;
  typedef itk::RescaleIntensityImageFilter< 
               InputImageType, WriteImageType > RescaleFilterType;
  RescaleFilterType::Pointer rescaler = RescaleFilterType::New();

  rescaler->SetOutputMinimum(   0 );
  rescaler->SetOutputMaximum( 255 );
  // Software Guide : EndCodeSnippet
  
  typedef itk::ImageFileWriter< WriteImageType >  Writer2Type;

  Writer2Type::Pointer writer2 = Writer2Type::New();

  writer2->SetFileName( av[3] );
 
  rescaler->SetInput( reader->GetOutput() );
  writer2->SetInput( rescaler->GetOutput() );

  try
    {
    writer2->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "exception in file writer " << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }
 
  // Software Guide : BeginLatex
  //
  // Now that we have the rescaled image. We just need to set up a \doxygen{ImageFileWriter}
  // and pass it the rescaled image as input.
  // The GDCMImageIO will automatically detect the pixel type, in this case char and update
  // accordingly the DICOM header information.
  //
  // Software Guide : EndLatex

  // Rewrite the image in DICOM format but using less bits per pixel
  // Software Guide : BeginCodeSnippet
  typedef itk::ImageFileWriter< WriteImageType >  Writer3Type;
  Writer3Type::Pointer writer3 = Writer3Type::New();
  writer3->SetFileName( av[4] );
  writer3->SetInput( rescaler->GetOutput() );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  // Again at this step we need to explicitely set the proper image IO (GDCMImageIO), but 
  // also we must tell the ImageFileWriter to not use the MetaDataDictionary from the input
  // but from the GDCMImageIO since this is the one that contains the DICOM specific information
  //
  // \index{itk::ImageFileWriter!UseInputMetaDataDictionaryOff()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  writer3->UseInputMetaDataDictionaryOff ();
  writer3->SetImageIO( gdcmImageIO );
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginCodeSnippet
  try
    {
    writer3->Update();
    }
  catch (itk::ExceptionObject & e)
    {
    std::cerr << "Exception in file writer " << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
    }
  // Software Guide : EndCodeSnippet

  return EXIT_SUCCESS;

}

