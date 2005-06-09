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
//  In order to read and write the slice we use here the \doxygen{GDCMImageIO}
//  class that encapsulates a connection to the underlying GDCM library. In
//  this way we gain access from ITK to the DICOM functionalities offered by
//  GDCM. The GDCMImageIO object is connected as the ImageIO object to be used
//  by the \doxygen{ImageFileWriter}.
//
//  We should first include the following header files.
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

int main( int argc, char* argv[] )
{

  // Verify the number of parameters in the command line
  if( argc < 5 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " DicomImage OutputDicomImage ";
    std::cerr << " OutputImage RescalDicomImage\n";
    return EXIT_FAILURE;
    }



// Software Guide : BeginLatex
//
// Then we declare the pixel type and image dimension, and use them for
// instantiating the image type to be read. 
// 
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef signed short InputPixelType;
  const unsigned int   InputDimension = 2;

  typedef itk::Image< InputPixelType, InputDimension > InputImageType;
// Software Guide : EndCodeSnippet





// Software Guide : BeginLatex
//
// With the image type we can instantiate the type of the reader, create one,
// and set the filename of the image to be read.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef itk::ImageFileReader< InputImageType > ReaderType;

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
// Software Guide : EndCodeSnippet

  



// Software Guide : BeginLatex
//
// GDCMImageIO is an ImageIO class for reading and writing DICOM v3 and
// ACR/NEMA images. The GDCMImageIO object is constructed here and connected to
// the ImageFileReader. 
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::GDCMImageIO           ImageIOType;

  ImageIOType::Pointer gdcmImageIO = ImageIOType::New();
  
  reader->SetImageIO( gdcmImageIO );
// Software Guide : EndCodeSnippet




// Software Guide : BeginLatex
//
// At this point we can trigger the reading process by invoking the Update()
// method.  Since this reading process may eventually throw an exception, we
// place the invokation inside a try/catch block.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
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
// Software Guide : EndCodeSnippet




// Software Guide : BeginLatex
//
// We have now the image in memory and can get access to it by using the
// GetOutput() method of the reader. In the remaining of this current example,
// we focus on showing how we can save this image again in DICOM format in a
// new file.
//
// Software Guide : EndLatex 




// Software Guide : BeginLatex
//
// First, we must instantiate an ImageFileWriter type. Then, we construct one,
// set the filename to be used for writing and connect the input image to be
// written. Given that in this example we write the image in different ways,
// and in each case we use a different writer, we enumerated here the variable
// names of the writer objects as well as their types.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef itk::ImageFileWriter< InputImageType >  Writer1Type;

  Writer1Type::Pointer writer1 = Writer1Type::New();

  writer1->SetFileName( argv[2] );
  writer1->SetInput( reader->GetOutput() );
// Software Guide : EndCodeSnippet
  




//  Software Guide : BeginLatex
//
//  We need to explicitly set the proper image IO (GDCMImageIO) to the writer
//  filter since the input DICOM dictionary is being passed along the writting
//  process. The dictionary contains all necessary information that a valid
//  DICOM file should contain, like Patient Name, Patient ID, Institution Name,
//  etc.
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  writer1->SetImageIO( gdcmImageIO );
// Software Guide : EndCodeSnippet





// Software Guide : BeginLatex
//
// The writing process is triggered by invoking the Update() method. Since this
// execution may result in exceptions being thrown we place the Update() call
// inside a try/catch block.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
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
// Software Guide : EndCodeSnippet




// Software Guide : BeginLatex
//
//  We will now rescale the image into a rescaled image one using the rescale
//  intensity image filter. For this purpose we use a better suited pixel type:
//  \code{unsigned char} instead of \code{signed short}.  The minimum and
//  maximum values of the output image are explicitly defined in the rescaling
//  filter.
//  
// Software Guide : EndLatex

//  Software Guide : BeginCodeSnippet
  typedef unsigned char WritePixelType;
  
  typedef itk::Image< WritePixelType, 2 > WriteImageType;
  
  typedef itk::RescaleIntensityImageFilter< 
               InputImageType, WriteImageType > RescaleFilterType;

  RescaleFilterType::Pointer rescaler = RescaleFilterType::New();

  rescaler->SetOutputMinimum(   0 );
  rescaler->SetOutputMaximum( 255 );
  // Software Guide : EndCodeSnippet
  


// Software Guide : BeginLatex
//
// We create a second writer object that will save the rescaled image into a
// file. This time not in DICOM format. This is done only for the sake of
// verifying the image against the one that will be saved in DICOM format later
// on this example.
//
// Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  typedef itk::ImageFileWriter< WriteImageType >  Writer2Type;

  Writer2Type::Pointer writer2 = Writer2Type::New();

  writer2->SetFileName( argv[3] );
 
  rescaler->SetInput( reader->GetOutput() );
  writer2->SetInput( rescaler->GetOutput() );
// Software Guide : EndCodeSnippet




// Software Guide : BeginLatex
//
// The writer can be executed by invoking the Update() method from inside a
// try/catch block.
//
// Software Guide : EndLatex 


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
// We proceed now to save the same rescaled image into a file in DICOM format.
// For this purpose we just need to set up a \doxygen{ImageFileWriter} and pass
// to it the rescaled image as input. 
// 
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::ImageFileWriter< WriteImageType >  Writer3Type;
  
  Writer3Type::Pointer writer3 = Writer3Type::New();

  writer3->SetFileName( argv[4] );
  writer3->SetInput( rescaler->GetOutput() );
// Software Guide : EndCodeSnippet





//  Software Guide : BeginLatex
//
// We now need to explicitly set the proper image IO (GDCMImageIO), but also
// we must tell the ImageFileWriter to not use the MetaDataDictionary from the
// input but from the GDCMImageIO since this is the one that contains the DICOM
// specific information
//
// The GDCMImageIO object will automatically detect the pixel type, in this
// case \code{unsigned char} and it will update the DICOM header information
// accordingly.
// \index{itk::ImageFileWriter!UseInputMetaDataDictionaryOff()}
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  writer3->UseInputMetaDataDictionaryOff ();
  writer3->SetImageIO( gdcmImageIO );
// Software Guide : EndCodeSnippet





// Software Guide : BeginLatex
//
// Finally we trigger the execution of the DICOM writer by invoking the
// Update() method from inside a try/catch block.
//
// Software Guide : EndLatex 

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

