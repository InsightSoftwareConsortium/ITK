/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageAdaptor3.cxx
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
// This example illustrates the use of \code{ImageAdaptors} and
// \code{PixelAccessors} for getting access to the components of a vector
// image. It shows in particular how to manage \code{PixelAccessors} with
// internal parameters. In this example we create an image of vectors by using
// a gradient filter. Then, we use an image adaptor in order extract one of the
// components of the vector image. The vector type used by the gradient filter
// is the \doxygen{CovariantVector} class. 
//
// We start by including the relevant headers.
// 
// \index{itk::ImageAdaptor!Instantiation}
// \index{itk::ImageAdaptor!Header}
// \index{itk::PixelAccessors!with parameters}
//
// Software Guide : EndLatex 

#include "itkImage.h"
#include "itkImageAdaptor.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"


// Software Guide : BeginCodeSnippet
#include "itkCovariantVector.h"
#include "itkGradientRecursiveGaussianImageFilter.h"
// Software Guide : EndCodeSnippet





//  Software Guide : BeginLatex
//
//  \code{PixelAccessors} may have internal parameters that affect the
//  operations they perform on input pixel data. \code{ImageAdaptors} allow to
//  set up the parameters of their internal \code{PixelAccessors} by using the
//  assignment operator. Any \code{PixelAccessor} having internal parameters must
//  henceforth implement the assignment operator. A \code{PixelAccessor} suitable for
//  extracting components from a vector pixel is shown below. The \code{m\_Index}
//  member variable is used to select the vector component to be returned.
//
//  Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
    class VectorPixelAccessor  
    {
    public:
      typedef itk::CovariantVector<float,2>   InternalType;
      typedef                      float      ExternalType;

      void operator=( const VectorPixelAccessor & vpa )
        {
        m_Index = vpa.m_Index;
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



//  Software Guide : BeginLatex
//
//  The \code{Get()} method is simply returning the \emph{i}-th component of
//  the vector indicated by the index. The assignment operator transfers the
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
    return -1;
    }



//  Software Guide : BeginLatex
//
//  In order to test the image accessor, we generate an image of vectors using
//  the \doxygen{GradientRecursiveGaussianImageFilter}. To be precise, this
//  class produces an image having \doxygen{CovariantVector} as its pixel type.
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

  typedef itk::GradientRecursiveGaussianImageFilter< 
                                             InputImageType,
                                             VectorImageType
                                                       > GradientFilterType;

  GradientFilterType::Pointer gradient = GradientFilterType::New();
// Software Guide : EndCodeSnippet



//  Software Guide : BeginLatex
//
//  We instantiate the \doxygen{ImageAdaptor} using the Vector image type as
//  first template parameter and the pixel accessor as second template
//  parameter.
//
//  Software Guide : EndLatex 



// Software Guide : BeginCodeSnippet
  typedef itk::ImageAdaptor<  VectorImageType, 
                              VectorPixelAccessor 
                                               > ImageAdaptorType;

  ImageAdaptorType::Pointer adaptor = ImageAdaptorType::New();
// Software Guide : EndCodeSnippet



//  Software Guide : BeginLatex
//
//  In order to set the index of the vector component to be extracted, we get
//  the value from the command line, create a pixel accessor, set its index
//  value and finally assign the pixel accessor to the image adaptor using the
//  \code{SetPixelAccessor()} method.
//
//  Software Guide : EndLatex 



// Software Guide : BeginCodeSnippet
  VectorPixelAccessor  accessor;

  accessor.SetIndex( atoi( argv[3] ) );

  adaptor->SetPixelAccessor( accessor );
// Software Guide : EndCodeSnippet




//  Software Guide : BeginLatex
//
//  We create a reader that will load an image and pass it as input to the
//  gradient filter.
//
//  Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
  typedef itk::ImageFileReader< InputImageType >   ReaderType;
  ReaderType::Pointer reader = ReaderType::New();  

  gradient->SetInput( reader->GetOutput() );
// Software Guide : EndCodeSnippet


//  Software Guide : BeginLatex
//
//  The filename to be read is passed to the reader and we proceed to update
//  the gradient filter.
//
//  Software Guide : EndLatex 


//  Software Guide : BeginCodeSnippet
  reader->SetFileName( argv[1] );

  gradient->Update();
//  Software Guide : EndCodeSnippet 



//  Software Guide : BeginLatex
//
//  We now connect the output of the reader as input of the image adaptor.  The
//  adaptor appears as a scalar image whose pixel values are taken from the
//  selected component of the vector image.
//
//  Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
  adaptor->SetImage( gradient->GetOutput() );
// Software Guide : EndCodeSnippet

 

//  Software Guide : BeginLatex
//
//  We instantiate an \doxygen{RescaleIntensityImageFilter} and an
//  \doxygen{ImageFileWriter} to rescale the dynamic range of the pixel values
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
// \begin{figure} \center
// \includegraphics[width=0.32\textwidth]{BrainProtonDensitySlice.eps}
// \includegraphics[width=0.32\textwidth]{ImageAdaptorToVectorImageComponentX.eps}
// \includegraphics[width=0.32\textwidth]{ImageAdaptorToVectorImageComponentY.eps}
// \itkcaption[Image Adaptor to Vector Image]{Illustration on the use of
// \doxygen{ImageAdaptor} for getting access to the components of a vector
// image. The input image at left was passed through a gradient image filter
// and the two components of the resulting vector image were extracted using an
// image adaptor.}
// \label{fig:ImageAdaptorToVectorImage}
// \end{figure}
//
//  Note that the adaptor is used as an image would have been used, not as a
//  filter. The \doxygen{ImageAdaptor} conforms to the API of the
//  \doxygen{Image} class. Figure~\ref{fig:ImageAdaptorToVectorImage}
//  illustrates the result of applying the current code for extracting both
//  components of a two dimensional gradient.
//
//  Software Guide : EndLatex 



  return 0;

}



