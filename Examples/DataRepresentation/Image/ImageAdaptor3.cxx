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

// Software Guide : BeginLatex
//
// This example illustrates the use of \doxygen{ImageAdaptor}
// to obtain access to the components of a vector image.
// Specifically, it shows how to manage pixel accessors containing
// internal parameters. In this example we create an image of vectors by using
// a gradient filter. Then, we use an image adaptor to extract one of the
// components of the vector image. The vector type used by the gradient filter
// is the \doxygen{CovariantVector} class.
//
// We start by including the relevant headers.
//
// \index{itk::ImageAdaptor!Instantiation}
// \index{itk::ImageAdaptor!Header}
// \index{itk::PixelAccessor!with parameters}
//
// Software Guide : EndLatex

#include "itkImageAdaptor.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"


// Software Guide : BeginCodeSnippet
#include "itkGradientRecursiveGaussianImageFilter.h"
// Software Guide : EndCodeSnippet


//  Software Guide : BeginLatex
//
//  A pixel accessors class may have internal parameters that affect the
//  operations performed on input pixel data. Image adaptors support
//  parameters in their internal pixel accessor by using
//  the assignment operator. Any pixel accessor which has internal
//  parameters must therefore implement the assignment operator.
//  The following defines a pixel accessor for extracting
//  components from a vector pixel. The
//  \code{m\_Index} member variable is used to select the vector component
//  to be returned.
//
//  Software Guide : EndLatex

namespace itk
{
// Software Guide : BeginCodeSnippet
class VectorPixelAccessor
{
public:
  typedef itk::CovariantVector<float,2>   InternalType;
  typedef                      float      ExternalType;

  VectorPixelAccessor() : m_Index(0) {}

  VectorPixelAccessor & operator=( const VectorPixelAccessor & vpa )
    {
      m_Index = vpa.m_Index;
      return *this;
    }
  ExternalType Get( const InternalType & input ) const
    {
    return static_cast<ExternalType>( input[ m_Index ] );
    }
  void SetIndex( unsigned int index )
    {
    m_Index = index;
    }

private:
  unsigned int m_Index;
};
// Software Guide : EndCodeSnippet
}

//  Software Guide : BeginLatex
//
//  The \code{Get()} method simply returns the \emph{i}-th component of
//  the vector as indicated by the index. The assignment operator transfers the
//  value of the index member variable from one instance of the pixel accessor
//  to another.
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
    std::cerr << "ImageAdaptor3   inputFileName outputComponentFileName ";
    std::cerr << " indexOfComponentToExtract" << std::endl;
    return EXIT_FAILURE;
    }


//  Software Guide : BeginLatex
//
//  In order to test the pixel accessor, we generate an image of vectors using
//  the \doxygen{GradientRecursiveGaussianImageFilter}. This
//  filter produces an output image of \doxygen{CovariantVector} pixel type.
//  Covariant vectors are the natural representation for gradients since they
//  are the equivalent of normals to iso-values manifolds.
//
//  Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
  typedef unsigned char  InputPixelType;
  const   unsigned int   Dimension = 2;
  typedef itk::Image< InputPixelType,  Dimension  >   InputImageType;
  typedef itk::CovariantVector< float, Dimension  >   VectorPixelType;
  typedef itk::Image< VectorPixelType, Dimension  >   VectorImageType;
  typedef itk::GradientRecursiveGaussianImageFilter< InputImageType,
                                        VectorImageType> GradientFilterType;

  GradientFilterType::Pointer gradient = GradientFilterType::New();
// Software Guide : EndCodeSnippet


//  Software Guide : BeginLatex
//
//  We instantiate the ImageAdaptor using the vector image type as
//  the first template parameter and the pixel accessor as the second
//  template parameter.
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::ImageAdaptor<  VectorImageType,
                              itk::VectorPixelAccessor > ImageAdaptorType;

  ImageAdaptorType::Pointer adaptor = ImageAdaptorType::New();
// Software Guide : EndCodeSnippet


//  Software Guide : BeginLatex
//
//  The index of the component to be extracted is specified
//  from the command line. In the following, we create the accessor,
//  set the index and connect the accessor to the image adaptor using
//  the \code{SetPixelAccessor()} method.
//
//  Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
  itk::VectorPixelAccessor  accessor;
  accessor.SetIndex( atoi( argv[3] ) );
  adaptor->SetPixelAccessor( accessor );
// Software Guide : EndCodeSnippet


//  Software Guide : BeginLatex
//
//  We create a reader to load the image specified from the
//  command line and pass its output as the input to the gradient filter.
//
//  Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
  typedef itk::ImageFileReader< InputImageType >   ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  gradient->SetInput( reader->GetOutput() );

  reader->SetFileName( argv[1] );
  gradient->Update();
//  Software Guide : EndCodeSnippet


//  Software Guide : BeginLatex
//
//  We now connect the output of the gradient filter as input to the
//  image adaptor.  The adaptor emulates a  scalar image whose pixel values
//  are taken from the selected component of the vector image.
//
//  Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
  adaptor->SetImage( gradient->GetOutput() );
// Software Guide : EndCodeSnippet


  typedef itk::Image< unsigned char, Dimension >   OutputImageType;
  typedef itk::RescaleIntensityImageFilter< ImageAdaptorType, OutputImageType>
    RescalerType;
  RescalerType::Pointer rescaler = RescalerType::New();
  typedef itk::ImageFileWriter< OutputImageType >   WriterType;
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
  // \includegraphics[width=0.32\textwidth]{ImageAdaptorToVectorImageComponentX}
  // \includegraphics[width=0.32\textwidth]{ImageAdaptorToVectorImageComponentY}
  // \itkcaption[Image Adaptor to Vector Image]{Using
  // ImageAdaptor to access components of a vector
  // image. The input image on the left was passed through a gradient image
  // filter and the two components of the resulting vector image were extracted
  // using an image adaptor.}
  // \label{fig:ImageAdaptorToVectorImage}
  // \end{figure}
  //
  //  As in the previous example, we rescale the scalar image before writing
  //  the image out to file. Figure~\ref{fig:ImageAdaptorToVectorImage}
  //  shows the result of applying the example code for extracting both
  //  components of a two dimensional gradient.
  //
  //  Software Guide : EndLatex

  return EXIT_SUCCESS;
}
