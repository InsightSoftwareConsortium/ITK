/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageAdaptor2.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif


// Software Guide : BeginLatex
//
// This example illustrates how to use ImageAdaptors and PixelAccessors for
// getting access to the individual channels of RGB images. In this case we
// create an ImageAdaptor that will accept an RGB image as input and will
// present it as being a scalar image. The pixel data will be taken directly
// from the Red channel of the adapted image.
// 
// \index{itk::ImageAdaptor!Instantiation}
// \index{itk::ImageAdaptor!Header}
//
// Software Guide : EndLatex 

#include "itkImage.h"
#include "itkImageAdaptor.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"


//  Software Guide : BeginLatex
//
//  The bulk of the effort in creating an ImageAdaptor is associated with the
//  definition of the PixelAccessor class. Here we need to define how pixel
//  values are going to be read from the RGB image. Note that we intentionally
//  omit the \code{Set()} method, since we only expect this adaptor to be used
//  for reading data from the image. We define the input image as having pixel
//  type \doxygen{RGBPixel}.
//
//  Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
    class RedChannelPixelAccessor  
    {
    public:
      typedef itk::RGBPixel<unsigned char>   InternalType;
      typedef               unsigned char    ExternalType;

      static ExternalType Get( const InternalType & input ) 
        {
        return static_cast<ExternalType>( input.GetRed() );
        }
    };
// Software Guide : EndCodeSnippet



//  Software Guide : BeginLatex
//
//  The \code{Get()} method is simply calling the \code{GetRed()} method
//  defined in the \doxygen{RGBPixel} class.
//
//  Software Guide : EndLatex 




//-------------------------
//
//   Main code
//
//-------------------------

int main( int argc, char *argv[] ) 
{


  if( argc < 3 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << "ImageAdaptor1   inputRGBFileName outputRedChannelFileName" << std::endl;
    return -1;
    }



//  Software Guide : BeginLatex
//
//  Now we use the internal pixel type of the pixel accessor for defining the
//  input image type, and then proceed to instantiate the \code{ImageAdaptor}
//  type.
//
//  Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
  typedef RedChannelPixelAccessor::InternalType  InputPixelType;
  const   unsigned int   Dimension = 2;
  typedef itk::Image< InputPixelType, Dimension >   ImageType;

  typedef itk::ImageAdaptor<  ImageType, 
                              RedChannelPixelAccessor 
                                               > ImageAdaptorType;

  ImageAdaptorType::Pointer adaptor = ImageAdaptorType::New();
// Software Guide : EndCodeSnippet




//  Software Guide : BeginLatex
//
//  We create a reader whose output will have the appropiate type for the
//  \emph{adapted} image type.
//
//  Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
  typedef itk::ImageFileReader< ImageType >   ReaderType;
  ReaderType::Pointer reader = ReaderType::New();  
// Software Guide : EndCodeSnippet


  reader->SetFileName( argv[1] );
  reader->Update();



//  Software Guide : BeginLatex
//
//  and now connect the output of the reader as input of the image adaptor.
//
//  Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
  adaptor->SetImage( reader->GetOutput() );
// Software Guide : EndCodeSnippet

 

//  Software Guide : BeginLatex
//
//  We instantiate a writer that will send the extracted channel to an image
//  file. Note that the image type used for the writer is the
//  \code{ImageAdaptorType} itself.
//
//  Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
  typedef itk::ImageFileWriter< ImageAdaptorType >   WriterType;
  
  WriterType::Pointer writer = WriterType::New();
// Software Guide : EndCodeSnippet


  writer->SetFileName( argv[2] );




//  Software Guide : BeginLatex
//
//  Finally, we connect the adaptor as the input to the writer and invoke the
//  \code{Update()} method.
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  writer->SetInput( adaptor );
  writer->Update();
// Software Guide : EndCodeSnippet



//  Software Guide : BeginLatex
//
//  Note that the adaptor is used as an image would have been used, not as a
//  filter. The \doxygen{ImageAdaptor} conforms to the API of the
//  \doxygen{Image}.
//
//  Software Guide : EndLatex 



  return 0;

}



