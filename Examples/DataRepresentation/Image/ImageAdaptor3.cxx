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
// This example illustrates the use ImageAdaptors and PixelAccessors for
// getting access to the components of a vector image. It shows in particular
// how to manage PixelAccessors with internal parameters.
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


// Software Guide : BeginCodeSnippet
#include "itkCovariantVector.h"
#include "itkGradientRecursiveGaussianImageFilter.h"
// Software Guide : EndCodeSnippet





//  Software Guide : BeginLatex
//
//  PixelAccessors may have internal parameters that affect the operations they
//  perform on input pixel data. ImageAdaptors allow to set up the parameters
//  of their internal PixelAccessors by using the assignment operator. Any
//  PixelAccessors having internal parameters must henceforth implement the
//  assignment operator. A PixelAccessors suitable for extracting components of
//  a vector pixel is shown below. The \code{m\_Index} member variable is used
//  to select the vector component to be returned.
//
//  Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
    class VectorPixelAccessor  
    {
    public:
      typedef itk::CovariantVector<float,2>   InternalType;
      typedef                      float    ExternalType;

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


  if( argc < 3 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << "ImageAdaptor1   inputFileName outputComponentFileName" << std::endl;
    return -1;
    }



//  Software Guide : BeginLatex
//
//  In order to test the image accessor, we generate an image of vectors using
//  the \doxygen{GradientRecursiveGaussianImageFilter}. To be precise, this
//  class produces an image of \doxygen{CovariantVectors} since they represent
//  the normals to iso-values manifolds.
//
//  Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
  typedef unsigned char  InputPixelType;
  const   unsigned int   Dimension = 2;

  typedef itk::Image< InputPixelType, Dimension  >   InputImageType;

  typedef itk::CovariantVector< float, Dimension >   VectorPixelType; 
  typedef itk::Image< VectorPixelType, Dimension  >  VectorImageType;

  typedef itk::GradientRecursiveGaussianImageFilter< 
                                             InputImageType,
                                             VectorImageType
                                                       > GradientFilterType;

  GradientFilterType::Pointer gradient = GradientFilterType::New();
// Software Guide : EndCodeSnippet


// Software Guide : BeginCodeSnippet
  typedef itk::ImageAdaptor<  VectorImageType, 
                              VectorPixelAccessor 
                                               > ImageAdaptorType;

  ImageAdaptorType::Pointer adaptor = ImageAdaptorType::New();
// Software Guide : EndCodeSnippet



// Software Guide : BeginCodeSnippet
VectorPixelAccessor  accessor;

accessor.SetIndex( atoi( argv[3] ) );

adaptor->GetPixelAccessor() = accessor;
// Software Guide : EndCodeSnippet




//  Software Guide : BeginLatex
//
//  We create a reader whose output will have the appropiate type for the
//  \emph{adapted} image type.
//
//  Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
  typedef itk::ImageFileReader< InputImageType >   ReaderType;
  ReaderType::Pointer reader = ReaderType::New();  
// Software Guide : EndCodeSnippet


  reader->SetFileName( argv[1] );

  gradient->SetInput( reader->GetOutput() );
  gradient->Update();



//  Software Guide : BeginLatex
//
//  and now connect the output of the reader as input of the image adaptor.
//
//  Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
  adaptor->SetImage( gradient->GetOutput() );
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
//  Note that the adaptor is used as an image would have been used, not as a
//  filter. The \doxygen{ImageAdaptor} conforms to the API of the
//  \doxygen{Image}.
//
//  Software Guide : EndLatex 



  return 0;

}



