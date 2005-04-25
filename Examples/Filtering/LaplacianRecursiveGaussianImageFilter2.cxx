/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    LaplacianRecursiveGaussianImageFilter2.cxx
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
//  Software Guide : BeginCommandLineArgs
//    INPUTS: {BrainProtonDensitySlice.png}
//    OUTPUTS: {LaplacianRecursiveGaussianImageFilter2output3.mha}
//    3
//    OUTPUTS: {LaplacianRecursiveGaussianImageFilter2Output3.png}
//  Software Guide : EndCommandLineArgs

//  Software Guide : BeginCommandLineArgs
//    INPUTS: {BrainProtonDensitySlice.png}
//    OUTPUTS: {LaplacianRecursiveGaussianImageFilter2output5.mha}
//    5
//    OUTPUTS: {LaplacianRecursiveGaussianImageFilter2Output5.png}
//  Software Guide : EndCommandLineArgs
  
//  Software Guide : BeginLatex
//
//  The previous exampled showed how to use the
//  \doxygen{RecursiveGaussianImageFilter} for computing the equivalent of a
//  Laplacian of an image after smoothing with a Gaussian.  The elements used
//  in this previous example have been packaged together in the
//  \doxygen{LaplacianRecursiveGaussianImageFilter} in order to simplify its
//  usage. This current example shows how to use this convenience filter for 
//  achieving the same results as the previous example.
//
//  \index{itk::LaplacianRecursiveGaussianImageFilter}
//
//  Software Guide : EndLatex 


#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

//  Software Guide : BeginLatex
//
//  The first step required to use this filter is to include its header file.
//
//  \index{itk::LaplacianRecursiveGaussianImageFilter!header}
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkLaplacianRecursiveGaussianImageFilter.h"
// Software Guide : EndCodeSnippet
#include "itkRescaleIntensityImageFilter.h"


int main( int argc, char * argv[] )
{
  if( argc < 4 ) 
    { 
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  outputImageFile  sigma [RescaledOutputImageFile] " << std::endl;
    return EXIT_FAILURE;
    }

  
  //  Software Guide : BeginLatex
  //
  //  Types should be selected on the desired input and output pixel types.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef    float    InputPixelType;
  typedef    float    OutputPixelType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The input and output image types are instantiated using the pixel types.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::Image< InputPixelType,  2 >   InputImageType;
  typedef itk::Image< OutputPixelType, 2 >   OutputImageType;
  // Software Guide : EndCodeSnippet


  typedef itk::ImageFileReader< InputImageType >  ReaderType;


  //  Software Guide : BeginLatex
  //
  //  The filter type is now instantiated using both the input image and the
  //  output image types.
  //
  //  \index{itk::RecursiveGaussianImageFilter!Instantiation}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::LaplacianRecursiveGaussianImageFilter<
                        InputImageType, OutputImageType >  FilterType;
  // Software Guide : EndCodeSnippet


  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );


  //  Software Guide : BeginLatex
  //
  //  This filter packages all the components illustrated in the previous
  //  example.  The filter is created by invoking the \code{New()} method and
  //  assigning the result to a \doxygen{SmartPointer}.
  //
  //  \index{itk::LaplacianRecursiveGaussianImageFilter!New()}
  //  \index{itk::LaplacianRecursiveGaussianImageFilter!Pointer}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  FilterType::Pointer laplacian = FilterType::New();
  // Software Guide : EndCodeSnippet



  //  Software Guide : BeginLatex
  //  
  //  The option for normalizing across scale space can also be selected in this filter.
  //
  //  \index{LaplacianRecursiveGaussianImageFilter!SetNormalizeAcrossScale()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  laplacian->SetNormalizeAcrossScale( false );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  The input image can be obtained from the output of another
  //  filter. Here, an image reader is used as the source. 
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  laplacian->SetInput( reader->GetOutput() );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  It is now time to select the $\sigma$ of the Gaussian used to smooth the
  //  data.  Note that $\sigma$ must be passed to both filters and that sigma
  //  is considered to be in millimeters. That is, at the moment of applying
  //  the smoothing process, the filter will take into account the spacing
  //  values defined in the image.
  //
  //  \index{itk::LaplacianRecursiveGaussianImageFilter!SetSigma()}
  //  \index{SetSigma()!itk::LaplacianRecursiveGaussianImageFilter}
  //
  //  Software Guide : EndLatex 

  const double sigma = atof( argv[3] );

  // Software Guide : BeginCodeSnippet
  laplacian->SetSigma( sigma );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Finally the pipeline is executed by invoking the \code{Update()} method.
  //
  //  \index{itk::LaplacianRecursiveGaussianImageFilter!Update()}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  try
    {
    laplacian->Update();
    }
  catch( itk::ExceptionObject & err ) 
    { 
    std::cout << "ExceptionObject caught !" << std::endl; 
    std::cout << err << std::endl; 
    return EXIT_FAILURE;
    } 
  // Software Guide : EndCodeSnippet



  // The image can also be saved into  a file, by using the ImageFileWriter.
  //
  typedef  float WritePixelType;
  typedef itk::Image< WritePixelType, 2 >    WriteImageType;

  typedef itk::ImageFileWriter< WriteImageType >  WriterType;

  WriterType::Pointer writer = WriterType::New();

  writer->SetInput( laplacian->GetOutput() );

  writer->SetFileName( argv[2] );

  writer->Update();



  //  Software Guide : BeginLatex
  //  
  // \begin{figure}
  // \center
  // \includegraphics[width=0.44\textwidth]{LaplacianRecursiveGaussianImageFilter2Output3.eps}
  // \includegraphics[width=0.44\textwidth]{LaplacianRecursiveGaussianImageFilter2Output5.eps}
  // \itkcaption[Output of the LaplacianRecursiveGaussianImageFilter.]{Effect of the
  // LaplacianRecursiveGaussianImageFilter on a slice from a MRI proton density image
  // of the brain.}
  // \label{fig:RecursiveGaussianImageFilter2InputOutput}
  // \end{figure}
  //
  //  Figure~\ref{fig:RecursiveGaussianImageFilter2InputOutput} illustrates the
  //  effect of this filter on a MRI proton density image of the brain using
  //  $\sigma$ values of $3$ (left) and $5$ (right).  The figure shows how the
  //  attenuation of noise can be regulated by selecting the appropriate
  //  standard deviation.  This type of scale-tunable filter is suitable for
  //  performing scale-space analysis.
  //
  //  Software Guide : EndLatex 

  
  // Rescale float outputs to png for inclusion in the Software guide
  // 
  if (argc > 4) 
    {
    typedef unsigned char    CharPixelType; 
    typedef itk::Image<CharPixelType, 2>    CharImageType;
    typedef itk::RescaleIntensityImageFilter< OutputImageType, CharImageType> 
                                                            RescaleFilterType;
    RescaleFilterType::Pointer rescale = RescaleFilterType::New();
    rescale->SetInput( laplacian->GetOutput() );
    rescale->SetOutputMinimum(   0 );
    rescale->SetOutputMaximum( 255 );
    typedef itk::ImageFileWriter< CharImageType >  CharWriterType;
    CharWriterType::Pointer charWriter = CharWriterType::New();
    charWriter->SetFileName( argv[4] );
    charWriter->SetInput( rescale->GetOutput() );
    charWriter->Update();
    }
     
  return EXIT_SUCCESS;
}

