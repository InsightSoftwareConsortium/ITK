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
// The following example illustrates the use of the
// \doxygen{NeighborhoodConnectedImageFilter}.  This filter is a close variant
// of the \doxygen{ConnectedThresholdImageFilter}. On one hand, the
// \code{ConnectedThresholdImageFilter} considers only the value of the pixel
// itself when determining whether it belongs to the region: if its value is
// within the interval [lowerThreshold,upperThreshold] it is included,
// otherwise it is excluded.  \code{NeighborhoodConnectedImageFilter},
// on the other hand, considers a user-defined neighborhood surrounding the
// pixel, requiring that the intensity of \textbf{each} neighbor be within
// the interval for it to be included.
//
// The reason for considering the neighborhood intensities instead of only the
// current pixel intensity is that small structures are less likely to be
// accepted in the region. The operation of this filter is equivalent to
// applying \code{ConnectedThresholdImageFilter} followed by mathematical
// morphology erosion using a structuring element of the same shape as
// the neighborhood provided to the \code{NeighborhoodConnectedImageFilter}.
//
// Software Guide : EndLatex


// Software Guide : BeginCodeSnippet
#include "itkNeighborhoodConnectedImageFilter.h"
// Software Guide : EndCodeSnippet


#include "itkImage.h"
#include "itkCastImageFilter.h"


//  Software Guide : BeginLatex
//
//  The \doxygen{CurvatureFlowImageFilter} is used here to smooth the image
//  while preserving edges.
//
//  Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkCurvatureFlowImageFilter.h"
// Software Guide : EndCodeSnippet

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

int main( int argc, char *argv[] )
{

  if( argc < 7 )
    {
    std::cerr << "Missing Parameters." << std::endl;
    std::cerr << "Usage: " << argv[0]
              << " inputImage outputImage"
              << " seedX seedY"
              << " lowerThreshold upperThreshold" << std::endl;
    return EXIT_FAILURE;
    }


  //  Software Guide : BeginLatex
  //
  //  We now define the image type using a particular pixel type and image
  //  dimension. In this case the \code{float} type is used for the pixels due
  //  to the requirements of the smoothing filter.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef   float           InternalPixelType;
  const     unsigned int    Dimension = 2;
  typedef itk::Image< InternalPixelType, Dimension >  InternalImageType;
  // Software Guide : EndCodeSnippet


  typedef unsigned char                            OutputPixelType;
  typedef itk::Image< OutputPixelType, Dimension > OutputImageType;

  typedef itk::CastImageFilter< InternalImageType, OutputImageType >
    CastingFilterType;
  CastingFilterType::Pointer caster = CastingFilterType::New();


  // We instantiate reader and writer types
  //
  typedef  itk::ImageFileReader< InternalImageType > ReaderType;
  typedef  itk::ImageFileWriter<  OutputImageType  > WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );


  //  Software Guide : BeginLatex
  //
  //  The smoothing filter type is instantiated using the image type as
  //  a template parameter.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::CurvatureFlowImageFilter<InternalImageType, InternalImageType>
    CurvatureFlowImageFilterType;
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Then, the filter is created by invoking the \code{New()} method and
  //  assigning the result to a \doxygen{SmartPointer}.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  CurvatureFlowImageFilterType::Pointer smoothing =
                         CurvatureFlowImageFilterType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  We now declare the type of the region growing filter. In this case it is
  //  the NeighborhoodConnectedImageFilter.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::NeighborhoodConnectedImageFilter<InternalImageType,
                                    InternalImageType > ConnectedFilterType;
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  One filter of this class is constructed using the \code{New()} method.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  ConnectedFilterType::Pointer neighborhoodConnected
                                                 = ConnectedFilterType::New();
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Now it is time to create a simple, linear data processing pipeline. A
  //  file reader is added at the beginning of the pipeline and a cast
  //  filter and writer are added at the end. The cast filter is required
  //  to convert \code{float} pixel types to integer types since only a
  //  few image file formats support \code{float} types.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  smoothing->SetInput( reader->GetOutput() );
  neighborhoodConnected->SetInput( smoothing->GetOutput() );
  caster->SetInput( neighborhoodConnected->GetOutput() );
  writer->SetInput( caster->GetOutput() );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  \code{CurvatureFlowImageFilter} requires a couple of parameters.
  //  The following are typical values for $2D$ images. However,
  //  they may have to be adjusted depending on the amount of noise present in
  //  the input image.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  smoothing->SetNumberOfIterations( 5 );
  smoothing->SetTimeStep( 0.125 );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  \code{NeighborhoodConnectedImageFilter} requires that two main parameters
  //  are specified. They are the lower and upper thresholds of the interval
  //  in which intensity values must fall to be included in the
  //  region. Setting these two values too close will not allow enough
  //  flexibility for the region to grow. Setting them too far apart will
  //  result in a region that engulfs the image.
  //
  //  \index{itk::NeighborhoodConnectedImageFilter!SetLower()}
  //  \index{itk::NeighborhoodConnectedImageFilter!SetUppder()}
  //
  //  Software Guide : EndLatex

  const InternalPixelType lowerThreshold = atof( argv[5] );
  const InternalPixelType upperThreshold = atof( argv[6] );

  // Software Guide : BeginCodeSnippet
  neighborhoodConnected->SetLower( lowerThreshold );
  neighborhoodConnected->SetUpper( upperThreshold );
  // Software Guide : EndCodeSnippet

  //  Software Guide : BeginLatex
  //
  //  Here, we add the crucial parameter that defines the neighborhood size
  //  used to determine whether a pixel lies in the region. The larger the
  //  neighborhood, the more stable this filter will be against noise in the
  //  input image, but also the longer the computing time will be.  Here we
  //  select a filter of radius $2$ along each dimension. This results in a
  //  neighborhood of $5 \times 5$ pixels.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  InternalImageType::SizeType radius;

  radius[0] = 2;   // two pixels along X
  radius[1] = 2;   // two pixels along Y

  neighborhoodConnected->SetRadius( radius );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  As in the \code{ConnectedThresholdImageFilter} example, we must
  //  provide the intensity value to be used for the output pixels accepted
  //  in the region and at least one seed point to define the starting point.
  //
  //  \index{itk::NeighborhoodConnectedImageFilter!SetSeed()}
  //  \index{itk::NeighborhoodConnectedImageFilter!SetReplaceValue()}
  //
  //  Software Guide : EndLatex

  InternalImageType::IndexType index;

  index[0] = atoi( argv[3] );
  index[1] = atoi( argv[4] );


  // Software Guide : BeginCodeSnippet
  neighborhoodConnected->SetSeed( index );
  neighborhoodConnected->SetReplaceValue( 255 );
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Invocation of the \code{Update()} method on the writer triggers the
  //  execution of the pipeline.  It is usually wise to put update calls in a
  //  \code{try/catch} block in case errors occur and exceptions are thrown.
  //
  //  Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    }
  // Software Guide : EndCodeSnippet


  //  Software Guide : BeginLatex
  //
  //  Now we'll run this example using the image
  //  \code{BrainProtonDensitySlice.png} as input available from the
  //  directory \code{Examples/Data}. We can easily segment the major
  //  anatomical structures by providing seeds in the appropriate locations
  //  and defining values for the lower and upper thresholds. For example
  //
  //  \begin{center}
  //  \begin{tabular}{|l|c|c|c|c|}
  //  \hline
  //  Structure & Seed Index & Lower & Upper & Output Image \\ \hline
  //  White matter & $(60,116)$ & 150 & 180 & Second from left in Figure \ref{fig:NeighborhoodConnectedImageFilterOutput} \\  \hline
  //  Ventricle    & $(81,112)$ & 210 & 250 & Third  from left in Figure \ref{fig:NeighborhoodConnectedImageFilterOutput} \\  \hline
  //  Gray matter  & $(107,69)$ & 180 & 210 & Fourth from left in Figure \ref{fig:NeighborhoodConnectedImageFilterOutput} \\  \hline
  //  \end{tabular}
  //  \end{center}
  //
  // \begin{figure} \center
  // \includegraphics[width=0.24\textwidth]{BrainProtonDensitySlice}
  // \includegraphics[width=0.24\textwidth]{NeighborhoodConnectedImageFilterOutput1}
  // \includegraphics[width=0.24\textwidth]{NeighborhoodConnectedImageFilterOutput2}
  // \includegraphics[width=0.24\textwidth]{NeighborhoodConnectedImageFilterOutput3}
  // \itkcaption[NeighborhoodConnected segmentation results ]{Segmentation results
  // of the NeighborhoodConnectedImageFilter for various seed points.}
  // \label{fig:NeighborhoodConnectedImageFilterOutput}
  // \end{figure}
  //
  //  As with the \code{ConnectedThresholdImageFilter} example, several seeds could
  //  be provided to the filter by repetedly calling the \code{AddSeed()} method
  //  with different indices.  Compare Figures
  //  \ref{fig:NeighborhoodConnectedImageFilterOutput} and
  //  \ref{fig:ConnectedThresholdOutput}, demonstrating the outputs of
  //  \code{NeighborhoodConnectedThresholdImageFilter} and \code{ConnectedThresholdImageFilter},
  //  respectively.  It is instructive to adjust the neighborhood radii and observe its
  //  effect on the smoothness of segmented object borders, size of the segmented region, and
  //  computing time.
  //
  //  Software Guide : EndLatex

  return EXIT_SUCCESS;
}
