/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    ImageAdaptor4.cxx
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
// Image adaptors can also be used for performing simple computations on image
// data. They are particularly interesting in the cases where basic pixel-wise
// operations are to be computed. The following example illustrates how to use
// the \doxygen{ImageAdaptor} for performing image thresholding. This is one of
// those simple image operations that can hardly justify to hold an extra copy
// of the image in memory.
//
// \index{itk::ImageAdaptor!Instantiation}
// \index{itk::ImageAdaptor!Header}
// \index{itk::ImageAdaptor!performing computation}
// \index{itk::PixelAccessors!with parameters}
// \index{itk::PixelAccessors!performing computation}
//
// Software Guide : EndLatex 

#include "itkImage.h"
#include "itkImageAdaptor.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"



//  Software Guide : BeginLatex
//
//  A pixel accessor for performing thresholding requires to hold internally
//  the threshold value . Henceforth, it must also implement the assignment
//  operator in order to allow setting this internal parameter. Here is the
//  code that implements binary thresholding as a pixel accessor.
//
//  Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
    class ThresholdingPixelAccessor  
    {
    public:
      typedef unsigned char      InternalType;
      typedef unsigned char      ExternalType;

      ExternalType Get( const InternalType & input ) const 
        {
        return (input > m_Threshold) ? 1 : 0;
        }
      void SetThreshold( const InternalType threshold )
        {
        m_Threshold = threshold;
        }

      void operator=( const ThresholdingPixelAccessor & vpa )
        {
        m_Threshold = vpa.m_Threshold;
        }
    private:
      InternalType m_Threshold;
    };
// Software Guide : EndCodeSnippet



//  Software Guide : BeginLatex
//
//  The \code{Get()} method computes the binary thresholding of the image.
//  The assignment operator transfers the value of the threshold member
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
    return -1;
    }



//  Software Guide : BeginLatex
//
//  In order to test the image adaptor, we first instantiate an image type
//  whose pixel type is the same as the internal pixel type of the pixel
//  accessor.
//
//  Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
  typedef ThresholdingPixelAccessor::InternalType     PixelType;

  const   unsigned int   Dimension = 2;

  typedef itk::Image< PixelType,  Dimension  >   ImageType;
// Software Guide : EndCodeSnippet



//  Software Guide : BeginLatex
//
//  We instantiate the \doxygen{ImageAdaptor} using the image type as
//  first template parameter and the pixel accessor as second template
//  parameter.
//
//  Software Guide : EndLatex 



// Software Guide : BeginCodeSnippet
  typedef itk::ImageAdaptor<  ImageType, 
                              ThresholdingPixelAccessor 
                                               > ImageAdaptorType;

  ImageAdaptorType::Pointer adaptor = ImageAdaptorType::New();
// Software Guide : EndCodeSnippet



//  Software Guide : BeginLatex
//
//  In order to set the threshold value to be used, we get the value from the
//  command line, create a pixel accessor, set its threshold value and finally
//  assign the pixel accessor to the image adaptor using the
//  \code{SetPixelAccessor()} method.
//
//  Software Guide : EndLatex 



// Software Guide : BeginCodeSnippet
  ThresholdingPixelAccessor  accessor;

  accessor.SetThreshold( atoi( argv[3] ) );

  adaptor->SetPixelAccessor( accessor );
// Software Guide : EndCodeSnippet




//  Software Guide : BeginLatex
//
//  We create a reader and load the input image.
//
//  Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
  typedef itk::ImageFileReader< ImageType >   ReaderType;
  ReaderType::Pointer reader = ReaderType::New();  

  reader->SetFileName( argv[1] );

  reader->Update();
// Software Guide : EndCodeSnippet




//  Software Guide : BeginLatex
//
//  The newly read image is set as the internal image of the image adaptor.
//
//  Software Guide : EndLatex 

//  Software Guide : BeginCodeSnippet
  adaptor->SetImage( reader->GetOutput() );
//  Software Guide : EndCodeSnippet 




//  Software Guide : BeginLatex
//
//  We instantiate an \doxygen{RescaleIntensityImageFilter} and an
//  \doxygen{ImageFileWriter} to rescale the dynamic range of the pixel values
//  and send the thresholded image to a file. Note that the image type
//  used for the rescaling filter is the \code{ImageAdaptorType} itself. That
//  is, the adaptor type is used as an image type, not as a filter type.
//
//  Software Guide : EndLatex 


// Software Guide : BeginCodeSnippet
  typedef itk::RescaleIntensityImageFilter< 
                                      ImageAdaptorType, 
                                      ImageType   
                                                       > RescalerType;

  RescalerType::Pointer rescaler = RescalerType::New();

  typedef itk::ImageFileWriter< ImageType >   WriterType;
  
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
// \includegraphics[width=0.32\textwidth]{ImageAdaptorThresholdingA.eps}
// \includegraphics[width=0.32\textwidth]{ImageAdaptorThresholdingB.eps}
// \itkcaption[Image Adaptor for performing computations]{Illustration on the
// use of \doxygen{ImageAdaptor} for performing simple image computation. An
// \doxygen{ImageAdaptor} is used here for performing binary thresholding on
// the input image at left. The center image used a threshold of 180, while the
// image at right used a threshold of 220.}
// \label{fig:ImageAdaptorThresholding}
// \end{figure}
//
//  Figure~\ref{fig:ImageAdaptorThresholding} illustrates the result of
//  applying the current code for computing thresholding of typical gray scale
//  image at two different levels. Note that the same effect could have been
//  achieved by using the \doxygen{BinaryThresholdImageFilter} but at the price
//  of holding and extra copy of the image in memory.
//
//  Software Guide : EndLatex 



  return 0;

}



