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

//  Software Guide : BeginCommandLineArgs
//    INPUTS:  {BrainProtonDensitySlice.png}
//    OUTPUTS: {ImageAdaptorThresholdingA.png}
//    ARGUMENTS:    180
//  Software Guide : EndCommandLineArgs
//
//  Software Guide : BeginCommandLineArgs
//    INPUTS:  {BrainProtonDensitySlice.png}
//    OUTPUTS: {ImageAdaptorThresholdingB.png}
//    ARGUMENTS:    220
//  Software Guide : EndCommandLineArgs

// Software Guide : BeginLatex
//
// Image adaptors can also be used to perform simple pixel-wise computations
// on image data. The following example illustrates how to use the
// \doxygen{ImageAdaptor} for image thresholding.
//
// \index{itk::ImageAdaptor!Instantiation}
// \index{itk::ImageAdaptor!Header}
// \index{itk::ImageAdaptor!performing computation}
// \index{itk::PixelAccessor!with parameters}
// \index{itk::PixelAccessor!performing computation}
//
// Software Guide : EndLatex

#include "itkImageAdaptor.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"


//  Software Guide : BeginLatex
//
//  A pixel accessor for image thresholding requires that the accessor
//  maintain the threshold value. Therefore, it must also implement the
//  assignment operator to set this internal parameter.
//
//  Software Guide : EndLatex

namespace itk
{
// Software Guide : BeginCodeSnippet
class ThresholdingPixelAccessor
{
public:
  typedef unsigned char      InternalType;
  typedef unsigned char      ExternalType;

  ThresholdingPixelAccessor() : m_Threshold(0) {};

  ExternalType Get( const InternalType & input ) const
    {
    return (input > m_Threshold) ? 1 : 0;
    }
  void SetThreshold( const InternalType threshold )
    {
    m_Threshold = threshold;
    }

  ThresholdingPixelAccessor &
    operator=( const ThresholdingPixelAccessor & vpa )
    {
    m_Threshold = vpa.m_Threshold;
    return *this;
    }

private:
  InternalType m_Threshold;
};
}

// Software Guide : EndCodeSnippet


//  Software Guide : BeginLatex
//
//  The \code{Get()} method returns one if the input pixel is above
//  the threshold and zero otherwise. The assignment operator transfers
//  the value of the threshold member
//  variable from one instance of the pixel accessor to another.
//
//  Software Guide : EndLatex


//-------------------------
//
//   Main code
//
//-------------------------

int main( int argc, char *argv[] )
{
  if( argc < 4 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << "ImageAdaptor4   inputFileName outputBinaryFileName ";
    std::cerr << " thresholdValue" << std::endl;
    return EXIT_FAILURE;
    }


//  Software Guide : BeginLatex
//
//  To create an image adaptor, we first instantiate an image type
//  whose pixel type is the same as the internal pixel type of the pixel
//  accessor.
//
//  Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
  typedef itk::ThresholdingPixelAccessor::InternalType     PixelType;
  const   unsigned int   Dimension = 2;
  typedef itk::Image< PixelType,  Dimension >   ImageType;
// Software Guide : EndCodeSnippet


//  Software Guide : BeginLatex
//
//  We instantiate the ImageAdaptor using the image type as the
//  first template parameter and the pixel accessor as the second template
//  parameter.
//
//  Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
  typedef itk::ImageAdaptor< ImageType,
                             itk::ThresholdingPixelAccessor > ImageAdaptorType;

  ImageAdaptorType::Pointer adaptor = ImageAdaptorType::New();
// Software Guide : EndCodeSnippet


//  Software Guide : BeginLatex
//
//  The threshold value is set from the command line. A threshold
//  pixel accessor is created and connected to the image adaptor
//  in the same manner as in the previous example.
//
//  Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
  itk::ThresholdingPixelAccessor  accessor;
  accessor.SetThreshold( atoi( argv[3] ) );
  adaptor->SetPixelAccessor( accessor );
// Software Guide : EndCodeSnippet


//  Software Guide : BeginLatex
//
//  We create a reader to load the input image and connect the output
//  of the reader as the input to the adaptor.
//
//  Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
  typedef itk::ImageFileReader< ImageType >   ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );
  reader->Update();

  adaptor->SetImage( reader->GetOutput() );
//  Software Guide : EndCodeSnippet


  typedef itk::RescaleIntensityImageFilter< ImageAdaptorType,
                                            ImageType > RescalerType;

  RescalerType::Pointer rescaler = RescalerType::New();
  typedef itk::ImageFileWriter< ImageType >   WriterType;
  WriterType::Pointer writer = WriterType::New();


  writer->SetFileName( argv[2] );

  rescaler->SetOutputMinimum(  0  );
  rescaler->SetOutputMaximum( 255 );

  rescaler->SetInput( adaptor );
  writer->SetInput( rescaler->GetOutput() );
  writer->Update();


  //  Software Guide : BeginLatex
  //
  // \begin{figure} \center
  // \includegraphics[width=0.32\textwidth]{BrainProtonDensitySlice}
  // \includegraphics[width=0.32\textwidth]{ImageAdaptorThresholdingA}
  // \includegraphics[width=0.32\textwidth]{ImageAdaptorThresholdingB}
  // \itkcaption[Image Adaptor for performing computations]{Using
  // ImageAdaptor to perform a simple image computation. An
  // ImageAdaptor is used to perform binary thresholding on
  // the input image on the  left. The center image was created using a
  // threshold of 180, while the
  // image on the right corresponds to a  threshold of 220.}
  // \label{fig:ImageAdaptorThresholding}
  // \end{figure}
  //
  //  As before, we rescale the emulated scalar image before writing it
  //  out to file.
  //  Figure~\ref{fig:ImageAdaptorThresholding} illustrates the result of
  //  applying the thresholding adaptor to a typical gray scale image using two
  //  different threshold values. Note that the same effect could have been
  //  achieved by using the \doxygen{BinaryThresholdImageFilter} but at the
  //  price of holding an extra copy of the image in memory.
  //
  // Software Guide : EndLatex


  return EXIT_SUCCESS;
}
