/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    CurvatureAnisotropicDiffusionImageFilter.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

//  Software Guide : BeginLatex
//
//  The \doxygen{CurvatureAnisotropicDiffusionImageFilter} performs anisotropic
//  diffusion on an image using a modified curvature diffusion equation (MCDE).
//
//  MCDE does not exhibit the edge enhancing properties of classic anisotropic
//  diffusion, which can under certain conditions undergo a ``negative''
//  diffusion,which enhances the contrast of edges.  Equations of the form of
//  MCDE always undergo positive diffusion, with the conductance term only
//  varying the strength of that diffusion.
// 
//  Qualitatively, MCDE compares well with other non-linear diffusion
//  techniques.  It is less sensitive to contrast than classic Perona-Malik
//  style diffusion, and preserves finer detailed structures in images.  There
//  is a potential speed trade-off for using this function in place of
//  itkGradientNDAnisotropicDiffusionFunction.  Each iteration of the solution
//  takes roughly twice as long.  Fewer iterations, however, may be required to
//  reach an acceptable solution.
//  
//  The MCDE equation is given as:
// 
//  \begin{equation}
//  f_t = \mid \nabla f \mid \nabla \cdot c( \mid \nabla f \mid ) \frac{
//  \nabla f }{ \mid \nabla f \mid } 
//  \end{equation}
// 
//  where the conductance modified curvature term is
// 
//  \begin{equation}
//  \nabla \cdot \frac{\nabla f}{\mid \nabla f \mid}
//  \end{equation}
//
//  \index{itk::CurvatureAnisotropicDiffusionImageFilter|textbf}
//
//  Software Guide : EndLatex 


#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"

//  Software Guide : BeginLatex
//
//  The first step required for using this filter is to include its header file
//
//  \index{itk::CurvatureAnisotropicDiffusionImageFilter!header}
//
//  Software Guide : EndLatex 

// Software Guide : BeginCodeSnippet
#include "itkCurvatureAnisotropicDiffusionImageFilter.h"
// Software Guide : EndCodeSnippet




int main( int argc, char ** argv )
{


  if( argc < 6 ) 
    { 
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  outputImageFile ";
    std::cerr << "numberOfIterations  timeStep  conductance" << std::endl;
    return 1;
    }

  
  //  Software Guide : BeginLatex
  //
  //  Types should be choosen for the pixels of the input and output images.
  //  The image types are defined using the pixel type and the dimension.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef    float    InputPixelType;
  typedef    float    OutputPixelType;

  typedef itk::Image< InputPixelType,  2 >   InputImageType;
  typedef itk::Image< OutputPixelType, 2 >   OutputImageType;
  // Software Guide : EndCodeSnippet



  typedef itk::ImageFileReader< InputImageType >  ReaderType;




  //  Software Guide : BeginLatex
  //
  //  The filter type is now instantiated using both the input image and the
  //  output image types. The filter object is created by the \code{New()}
  //  method.
  //
  //  \index{itk::CurvatureAnisotropicDiffusionImageFilter!instantiation}
  //  \index{itk::CurvatureAnisotropicDiffusionImageFilter!New()}
  //  \index{itk::CurvatureAnisotropicDiffusionImageFilter!Pointer}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  typedef itk::CurvatureAnisotropicDiffusionImageFilter<
               InputImageType, OutputImageType >  FilterType;

  FilterType::Pointer filter = FilterType::New();
  // Software Guide : EndCodeSnippet



  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );



  //  Software Guide : BeginLatex
  //
  //  The input image can be obtained from the output of another filter. Here,
  //  an image reader is used as source.
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  filter->SetInput( reader->GetOutput() );
  // Software Guide : EndCodeSnippet


  const unsigned int numberOfIterations = atoi( argv[3] );

  const double       timeStep = atof( argv[4] );

  const double       conductance = atof( argv[5] );




  //  Software Guide : BeginLatex
  //
  //  This filter requires three parameters, the number of iterations to be
  //  performed, the time step used in the computation of the level set
  //  evolution and the value of conductance. These parameters are set using
  //  the methods \code{SetNumberOfIterations()}, \code{SetTimeStep()} and
  //  \code{SetConductance} respectively.  The filter can be executed by
  //  invoking \code{Update()}.
  //
  //  \index{itk::CurvatureAnisotropicDiffusionImageFilter!Update()}
  //  \index{itk::CurvatureAnisotropicDiffusionImageFilter!SetTimeStep()}
  //  \index{itk::CurvatureAnisotropicDiffusionImageFilter!SetNumberOfIterations()}
  //  \index{itk::CurvatureAnisotropicDiffusionImageFilter!SetConductanceParameter()}
  //  \index{SetTimeStep()!itk::CurvatureAnisotropicDiffusionImageFilter}
  //  \index{SetNumberOfIterations()!itk::CurvatureAnisotropicDiffusionImageFilter}
  //  \index{SetConductanceParameter()!itk::CurvatureAnisotropicDiffusionImageFilter}
  //
  //  Software Guide : EndLatex 

  // Software Guide : BeginCodeSnippet
  filter->SetNumberOfIterations( numberOfIterations );
  filter->SetTimeStep( timeStep );
  filter->SetConductanceParameter( conductance );
  
  filter->Update();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Typical values for the time step are 0.25 in $2D$ images and 0.125 in
  //  $3D$ images. The number of iterations can be usually around $5$, more
  //  iterations will result in further smoothing and will increase linearly
  //  the computing time. The conductance parameter is usually around $3.0$.
  //
  //  Software Guide : EndLatex 



  //
  //  If the output of this filter has been connected to other filters down the
  //  pipeline, updating any of the downstream filters would have triggered the
  //  execution of this one. For example, a writer filter could have been used
  //  after the curvatur flow filter.
  //

  typedef unsigned char WritePixelType;

  typedef itk::Image< WritePixelType, 2 > WriteImageType;

  typedef itk::RescaleIntensityImageFilter< 
               OutputImageType, WriteImageType > RescaleFilterType;

  RescaleFilterType::Pointer rescaler = RescaleFilterType::New();

  rescaler->SetOutputMinimum(   0 );
  rescaler->SetOutputMaximum( 255 );
  

  typedef itk::ImageFileWriter< WriteImageType >  WriterType;

  WriterType::Pointer writer = WriterType::New();

  writer->SetFileName( argv[2] );
 

  rescaler->SetInput( filter->GetOutput() );
  writer->SetInput( rescaler->GetOutput() );
  writer->Update();

  



  //  Software Guide : BeginLatex
  //  
  // \begin{figure} \center
  // \includegraphics[width=6cm]{BrainProtonDensitySlice.eps}
  // \includegraphics[width=6cm]{CurvatureAnisotropicDiffusionImageFilterOutput.eps}
  // \caption[CurvatureAnisotropicDiffusionImageFilter output]{Effect of the
  // CurvatureAnisotropicDiffusionImageFilter on a slice from a MRI Proton
  // Density image  of the brain.}
  // \label{fig:CurvatureAnisotropicDiffusionImageFilterInputOutput}
  // \end{figure}
  //
  //  Figure \ref{fig:CurvatureAnisotropicDiffusionImageFilterInputOutput}
  //  illustrates the effect of this filter on a MRI proton density image of
  //  the brain. In this example the filter was run with a time step of $0.25$,
  //  $5$ iterations and a conductance value of $3.0$.  The figure shows how
  //  homogeneous regions are smoothed and edges are preserved.
  //
  //
  //  Software Guide : EndLatex 


  return 0;

}

