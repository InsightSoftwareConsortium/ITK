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
#include "itkRescaleIntensityImageFilter.h"


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
      typedef itk::RGBPixel<float>   InternalType;
      typedef               float    ExternalType;

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
//  \index{PixelAccessor!RGB red channel}
//  \index{itk::ImageAdaptor!RGB red channel}
//  \index{ImageAdaptor!RGB red channel}
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
//  We instantiate a \doxygen{RescaleIntensityImageFilter} and a
//  \doxygen{ImageFileWriter} to rescale the dinamic range of the pixel values
//  and send the extracted channel to an image file. Note that the image type
//  used for the rescaling filter is the \code{ImageAdaptorType} itself. That
//  is, the adaptor type is used as an image type, not as a filter type.
//
//  Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
  typedef itk::Image< unsigned char, Dimension >   OutputImageType;

  typedef itk::RescaleIntensityImageFilter< 
                                      ImageAdaptorType, 
                                      OutputImageType   
                                                       > RescalerType;

  RescalerType::Pointer rescaler = RescalerType::New();

  typedef itk::ImageFileWriter< OutputImageType >   WriterType;
  
  WriterType::Pointer writer = WriterType::New();
// Software Guide : EndCodeSnippet


  writer->SetFileName( argv[2] );




//  Software Guide : BeginLatex
//
//  Finally, we connect the adaptor as the input to the rescaler and invoke the
//  \code{Update()} method in the writer.
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
  rescaler->SetOutputMinimum(  0  );
  rescaler->SetOutputMaximum( 255 );

  rescaler->SetInput( adaptor );
  writer->SetInput( rescaler->GetOutput() );
  writer->Update();
// Software Guide : EndCodeSnippet



//  Software Guide : BeginLatex
//
//  ImageAdaptors for the green and blue channels can easily be implemented by
//  slightly modifying the pixel accessor of the red channel and then using the
//  new pixel accessor for instantiating the type of an image adaptor. Here is
//  the code for a green channel pixel accessor.
//
//  \index{PixelAccessor!RGB green channel}
//  \index{itk::ImageAdaptor!RGB green channel}
//  \index{ImageAdaptor!RGB green channel}
//
//  Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
    class GreenChannelPixelAccessor  
    {
    public:
      typedef itk::RGBPixel<float>   InternalType;
      typedef               float    ExternalType;

      static ExternalType Get( const InternalType & input ) 
        {
        return static_cast<ExternalType>( input.GetGreen() );
        }
    };

// Software Guide : EndCodeSnippet




//  Software Guide : BeginLatex
//
// Here is the code for a blue channel pixel accessor.
//
//  \index{PixelAccessor!RGB blue channel}
//  \index{itk::ImageAdaptor!RGB blue channel}
//  \index{ImageAdaptor!RGB blue channel}
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
    class BlueChannelPixelAccessor  
    {
    public:
      typedef itk::RGBPixel<float>   InternalType;
      typedef               float    ExternalType;

      static ExternalType Get( const InternalType & input ) 
        {
        return static_cast<ExternalType>( input.GetBlue() );
        }
    };
// Software Guide : EndCodeSnippet




//  Software Guide : BeginLatex
//
// \begin{figure} \center
// \includegraphics[width=0.24\textwidth]{VisibleWomanEyeSlice.eps}
// \includegraphics[width=0.24\textwidth]{VisibleWomanEyeSliceRedComponent.eps}
// \includegraphics[width=0.24\textwidth]{VisibleWomanEyeSliceGreenComponent.eps}
// \includegraphics[width=0.24\textwidth]{VisibleWomanEyeSliceBlueComponent.eps}
// \itkcaption[Image Adaptor to RGB Image]{Illustration on the use of
// \doxygen{ImageAdaptor} for extracting the components of an RGB image. The
// image at left is a subregion of the Visible Woman cryogenic data set. The
// red, green and blue components are shown from left to right as scalar images
// extracted with an \doxygen{ImageAdaptor}.}
// \label{fig:ImageAdaptorToRGBImage}
// \end{figure}
//
//
//  Note that the adaptor is used as an image would have been used, not as a
//  filter. The \doxygen{ImageAdaptor} conforms to the API of the
//  \doxygen{Image}. Figure~\ref{fig:ImageAdaptorToRGBImage} shows the result
//  of extracting the red, green and blue components from a region of the
//  Visible Woman cryogenic data set. 
//
//  Software Guide : EndLatex 



  return 0;

}



