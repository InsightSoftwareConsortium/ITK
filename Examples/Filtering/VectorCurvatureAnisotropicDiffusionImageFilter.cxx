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

//  Software Guide : BeginLatex
//
//  The \doxygen{VectorCurvatureAnisotropicDiffusionImageFilter} performs
//  anisotropic diffusion on a vector image using a modified curvature
//  diffusion equation (MCDE).  The MCDE is the same described in
//  \ref{sec:CurvatureAnisotropicDiffusionImageFilter}.
//
//  Typically in vector-valued diffusion, vector components are diffused
//  independently of one another using a conductance term that is linked across
//  the components.
//
//  This filter is designed to process images of \doxygen{Vector} type.  The
//  code relies on various typedefs and overloaded operators defined in
//  \doxygen{Vector}. It is perfectly reasonable, however, to apply this
//  filter to images of other, user-defined types as long as the appropriate
//  typedefs and operator overloads are in place.  As a general rule, follow
//  the example of the \doxygen{Vector} class in defining your data types.
//
//  \index{itk::Vector\-Curvature\-Anisotropic\-Diffusion\-Image\-Filter}
//
//  Software Guide : EndLatex


#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkGradientRecursiveGaussianImageFilter.h"
#include "itkVectorIndexSelectionCastImageFilter.h"


//  Software Guide : BeginLatex
//
//  The first step required to use this filter is to include its header file.
//
//  \index{itk::Vector\-Curvature\-Anisotropic\-Diffusion\-Image\-Filter!header}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkVectorCurvatureAnisotropicDiffusionImageFilter.h"
// Software Guide : EndCodeSnippet


int main( int argc, char * argv[] )
{
  if( argc < 6 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  outputGradientImageFile ";
    std::cerr << "outputSmoothedGradientImageFile ";
    std::cerr << "numberOfIterations  timeStep  " << std::endl;
    return EXIT_FAILURE;
    }


  //  Software Guide : BeginLatex
  //
  //  Types should be selected based on required pixel type for the input and
  //  output images.  The image types are defined using the pixel type and
  //  the dimension.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef float                            InputPixelType;
  typedef itk::CovariantVector< float, 2 > VectorPixelType;
  typedef itk::Image< InputPixelType,  2 > InputImageType;
  typedef itk::Image< VectorPixelType, 2 > VectorImageType;
  // Software Guide : EndCodeSnippet


  typedef itk::ImageFileReader< InputImageType >  ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );


  //  Software Guide : BeginLatex
  //
  //  The filter type is now instantiated using both the input image and the
  //  output image types. The filter object is created by the \code{New()}
  //  method.
  //
  //  \index{itk::Vector\-Curvature\-Anisotropic\-Diffusion\-Image\-Filter!instantiation}
  //  \index{itk::Vector\-Curvature\-Anisotropic\-Diffusion\-Image\-Filter!New()}
  //  \index{itk::Vector\-Curvature\-Anisotropic\-Diffusion\-Image\-Filter!Pointer}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::VectorCurvatureAnisotropicDiffusionImageFilter<
                       VectorImageType, VectorImageType >  FilterType;
  FilterType::Pointer filter = FilterType::New();
  // Software Guide : EndCodeSnippet


  typedef itk::GradientRecursiveGaussianImageFilter<
                       InputImageType, VectorImageType >   GradientFilterType;

  GradientFilterType::Pointer gradient = GradientFilterType::New();


  //  Software Guide : BeginLatex
  //
  //  The input image can be obtained from the output of another filter. Here,
  //  an image reader is used as source and its data is passed through a
  //  gradient filter in order to generate an image of vectors.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  gradient->SetInput( reader->GetOutput() );
  filter->SetInput( gradient->GetOutput() );
  // Software Guide : EndCodeSnippet

  const unsigned int numberOfIterations = atoi( argv[4] );
  const double       timeStep = atof( argv[5] );


  //  Software Guide : BeginLatex
  //
  //  This filter requires two parameters: the number of iterations to be
  //  performed and the time step used in the computation of the level set
  //  evolution. These parameters are set using the methods
  //  \code{SetNumberOfIterations()} and \code{SetTimeStep()} respectively.
  //  The filter can be executed by invoking \code{Update()}.
  //
  //  \index{itk::Vector\-Curvature\-Anisotropic\-Diffusion\-Image\-Filter!Update()}
  //  \index{itk::Vector\-Curvature\-Anisotropic\-Diffusion\-Image\-Filter!SetTimeStep()}
  //  \index{itk::Vector\-Curvature\-Anisotropic\-Diffusion\-Image\-Filter!SetNumberOfIterations()}
  //  \index{SetTimeStep()!itk::Vector\-Curvature\-Anisotropic\-Diffusion\-Image\-Filter}
  //  \index{SetNumberOfIterations()!itk::Vector\-Curvature\-Anisotropic\-Diffusion\-Image\-Filter}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filter->SetNumberOfIterations( numberOfIterations );
  filter->SetTimeStep( timeStep );
  filter->SetConductanceParameter(1.0);
  filter->Update();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Typical values for the time step are $0.125$ in $2D$ images and
  //  $0.0625$ in $3D$ images. The number of iterations can be usually around
  //  $5$, however more iterations will result in further smoothing and will
  //  increase the computing time linearly.
  //
  //  Software Guide : EndLatex


  //
  //  If the output of this filter has been connected to other filters down the
  //  pipeline, updating any of the downstream filters would have triggered the
  //  execution of this one. For example, a writer filter could have been used
  //  after the curvature flow filter.
  //
  typedef float                                      OutputPixelType;
  typedef itk::Image< OutputPixelType,  2 >          OutputImageType;
  typedef itk::VectorIndexSelectionCastImageFilter<
                  VectorImageType, OutputImageType > ComponentFilterType;
  ComponentFilterType::Pointer component = ComponentFilterType::New();

  // Select the component to extract.
  component->SetIndex( 0 );

  typedef unsigned char                      WritePixelType;
  typedef itk::Image< WritePixelType, 2 >    WriteImageType;
  typedef itk::RescaleIntensityImageFilter<
           OutputImageType, WriteImageType > RescaleFilterType;
  RescaleFilterType::Pointer rescaler = RescaleFilterType::New();
  rescaler->SetOutputMinimum(   0 );
  rescaler->SetOutputMaximum( 255 );

  typedef itk::ImageFileWriter< WriteImageType >  WriterType;
  WriterType::Pointer writer = WriterType::New();
  rescaler->SetInput( component->GetOutput() );
  writer->SetInput( rescaler->GetOutput() );

  // Save the component of the original gradient
  component->SetInput( gradient->GetOutput() );
  writer->SetFileName( argv[2] );
  writer->Update();


  // Save the component of the smoothed gradient
  component->SetInput( filter->GetOutput() );
  writer->SetFileName( argv[3] );
  writer->Update();


  //  Software Guide : BeginLatex
  //
  // \begin{figure} \center
  // \includegraphics[width=0.44\textwidth]{VectorCurvatureAnisotropicDiffusionImageFilterInput}
  // \includegraphics[width=0.44\textwidth]{VectorCurvatureAnisotropicDiffusionImageFilterOutput}
  // \itkcaption[VectorCurvatureAnisotropicDiffusionImageFilter output]{Effect
  // of the VectorCurvatureAnisotropicDiffusionImageFilter on the $X$ component
  // of the gradient from a MRIproton density brain image.}
  // \label{fig:VectorCurvatureAnisotropicDiffusionImageFilterInputOutput}
  // \end{figure}
  //
  //  Figure~\ref{fig:VectorCurvatureAnisotropicDiffusionImageFilterInputOutput}
  //  illustrates the effect of this filter on a MRI proton density image of
  //  the brain. The images show the $X$ component of the gradient before
  //  (left) and after (right) the application of the filter. In this example
  //  the filter was run with a time step of 0.25, and 5 iterations.
  //
  //  Software Guide : EndLatex

  return EXIT_SUCCESS;
}
