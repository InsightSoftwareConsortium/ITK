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
//    INPUTS:  {GradientRecursiveGaussianImageFilterTest.mha}
//    ARGUMENTS: {GradientVectorFlowImageFilterOutput.mha}
//    ARGUMENTS:    5 2000.0
//  Software Guide : EndCommandLineArgs

//  Software Guide : BeginLatex
//
//  The \doxygen{GradientVectorFlowImageFilter} smooths multi-components images
//  such as vector fields and color images by applying a computation of the
//  diffusion equation.  A typical use of this filter is to smooth the vector
//  field resulting from computing the gradient of an image, with the purpose
//  of using the smoothed field in order to guide a deformable model.
//
//  The input image must be a multi-components images.
//
//  \index{itk::GradientVectorFlowImageFilter}
//
//  Software Guide : EndLatex


#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"

//  Software Guide : BeginLatex
//
//  The first step required to use this filter is to include its header file.
//
//  \index{itk::GradientVectorFlowImageFilter!header}
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkGradientVectorFlowImageFilter.h"
// Software Guide : EndCodeSnippet


int main( int argc, char * argv[] )
{
  if( argc < 5 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  outputImageFile";
    std::cerr << " numberOfIterations  noiseLevel" << std::endl;
    return EXIT_FAILURE;
    }

  //  Software Guide : BeginLatex
  //
  //  Types should be selected based on the pixel types required for the input
  //  and output images. In this particular case, the input and output pixel
  //  types are multicomponents type such as itk::Vectors.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const unsigned int                                Dimension = 3;
  typedef float                                     InputValueType;
  typedef float                                     OutputValueType;
  typedef itk::Vector< InputValueType,  Dimension > InputPixelType;
  typedef itk::Vector< OutputValueType, Dimension > OutputPixelType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  With them, the input and output image types can be instantiated.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Image< InputPixelType,  Dimension >   InputImageType;
  typedef itk::Image< OutputPixelType, Dimension >   OutputImageType;
  // Software Guide : EndCodeSnippet


  typedef itk::ImageFileReader< InputImageType >  ReaderType;


  //  Software Guide : BeginLatex
  //
  //  The GradientVectorFlow filter type is now instantiated using both the
  //  input image and the output image types.
  //
  //  \index{itk::GradientVectorFlowImageFilter!instantiation}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::GradientVectorFlowImageFilter<
               InputImageType, OutputImageType >  FilterType;
  // Software Guide : EndCodeSnippet


  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );


  //  Software Guide : BeginLatex
  //
  //  A filter object is created by invoking the \code{New()} method and
  //  assigning the result to a \doxygen{SmartPointer}.
  //
  //  \index{itk::GradientVectorFlowImageFilter!New()}
  //  \index{itk::GradientVectorFlowImageFilter!Pointer}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  FilterType::Pointer filter = FilterType::New();
  // Software Guide : EndCodeSnippet


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
  const double       noiseLevel = atof( argv[4] );


  //  Software Guide : BeginLatex
  //
  //  The GradientVectorFlow filter requires two parameters, the number of
  //  iterations to be performed and the noise level of the input image. The
  //  noise level will be used to estimate the time step that should be used in
  //  the computation of the diffusion. These two parameters are set using the
  //  methods \code{SetNumberOfIterations()} and \code{SetNoiseLevel()}
  //  respectively.  Then the filter can be executed by invoking
  //  \code{Update()}.
  //
  //  \index{itk::GradientVectorFlowImageFilter!Update()}
  //  \index{itk::GradientVectorFlowImageFilter!SetNoiseLevel()}
  //  \index{itk::GradientVectorFlowImageFilter!SetNumberOfIterations()}
  //  \index{SetNoiseLevel()!itk::GradientVectorFlowImageFilter}
  //  \index{SetNumberOfIterations()!itk::GradientVectorFlowImageFilter}
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filter->SetIterationNum( numberOfIterations );
  filter->SetNoiseLevel( noiseLevel );
  filter->Update();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  When using as input the result of a gradient filter, then the typical
  //  values for the noise level will be around 2000.0.
  //
  //  Software Guide : EndLatex


  //  Software Guide : BeginLatex
  //
  //  If the output of this filter has been connected to other filters down
  //  the pipeline, updating any of the downstream filters will
  //  triggered the execution of this one. For example, a writer filter could
  //  have been used after the curvature flow filter.
  //
  //  Software Guide : EndLatex
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[2] );

  // Software Guide : BeginCodeSnippet
  writer->SetInput( filter->GetOutput() );
  writer->Update();
  // Software Guide : EndCodeSnippet


  // In order to visualize the resulting vector field you could use ParaView or
  // VV (the 4D Slicer).

  return EXIT_SUCCESS;
}
