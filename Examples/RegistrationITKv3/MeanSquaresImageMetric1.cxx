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
// This example illustrates how to explore the domain of an image metric.  This
// is a useful exercise to do before starting a registration process, since
// getting familiar with the characteristics of the metric is fundamental for
// the appropriate selection of the optimizer to be use for driving the
// registration process, as well as for selecting the optimizer parameters.
// This process makes possible to identify how noisy a metric may be in a given
// range of parameters, and it will also give an idea of the number of local
// minima or maxima in which an optimizer may get trapped while exploring the
// parametric space.
//
// Software Guide : EndLatex


#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"


// Software Guide : BeginLatex
//
// We start by including the headers of the basic components: Metric, Transform
// and Interpolator.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkMeanSquaresImageToImageMetric.h"
#include "itkTranslationTransform.h"
#include "itkNearestNeighborInterpolateImageFunction.h"
// Software Guide : EndCodeSnippet


int main( int argc, char * argv[] )
{
  if( argc < 3 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  fixedImage  movingImage" << std::endl;
    return EXIT_FAILURE;
    }

// Software Guide : BeginLatex
//
// We define the dimension and pixel type of the images to be used in the
// evaluation of the Metric.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  const     unsigned int   Dimension = 2;
  typedef   unsigned char  PixelType;

  typedef itk::Image< PixelType, Dimension >   ImageType;
// Software Guide : EndCodeSnippet


  typedef itk::ImageFileReader< ImageType >  ReaderType;

  ReaderType::Pointer fixedReader  = ReaderType::New();
  ReaderType::Pointer movingReader = ReaderType::New();

  fixedReader->SetFileName(  argv[ 1 ] );
  movingReader->SetFileName( argv[ 2 ] );

  try
    {
    fixedReader->Update();
    movingReader->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception catched !" << std::endl;
    std::cerr << excep << std::endl;
    }

// Software Guide : BeginLatex
//
// The type of the Metric is instantiated and one is constructed.  In this case
// we decided to use the same image type for both the fixed and the moving
// images.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::MeanSquaresImageToImageMetric<
                            ImageType, ImageType >  MetricType;

  MetricType::Pointer metric = MetricType::New();
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// We also instantiate the transform and interpolator types, and create objects
// of each class.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::TranslationTransform< double, Dimension >  TransformType;

  TransformType::Pointer transform = TransformType::New();


  typedef itk::NearestNeighborInterpolateImageFunction<
                                 ImageType, double >  InterpolatorType;

  InterpolatorType::Pointer interpolator = InterpolatorType::New();
// Software Guide : EndCodeSnippet


  transform->SetIdentity();

  ImageType::ConstPointer fixedImage  = fixedReader->GetOutput();
  ImageType::ConstPointer movingImage = movingReader->GetOutput();


// Software Guide : BeginLatex
//
// The classes required by the metric are connected to it. This includes the
// fixed and moving images, the interpolator and the  transform.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  metric->SetTransform( transform );
  metric->SetInterpolator( interpolator );

  metric->SetFixedImage(  fixedImage  );
  metric->SetMovingImage( movingImage );
// Software Guide : EndCodeSnippet

  metric->SetFixedImageRegion(  fixedImage->GetBufferedRegion()  );

  try
    {
    metric->Initialize();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception catched !" << std::endl;
    std::cerr << excep << std::endl;
    return EXIT_FAILURE;
    }


// Software Guide : BeginLatex
//
// Finally we select a region of the parametric space to explore. In this case
// we are using a translation transform in 2D, so we simply select translations
// from a negative position to a positive position, in both $x$ and $y$. For
// each one of those positions we invoke the GetValue() method of the Metric.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  MetricType::TransformParametersType displacement( Dimension );

  const int rangex = 50;
  const int rangey = 50;

  for( int dx = -rangex; dx <= rangex; dx++ )
    {
    for( int dy = -rangey; dy <= rangey; dy++ )
      {
      displacement[0] = dx;
      displacement[1] = dy;
      const double value = metric->GetValue( displacement );
      std::cout << dx << "   "  << dy << "   " << value << std::endl;
      }
    }
// Software Guide : EndCodeSnippet


// Software Guide : BeginLatex
//
// \begin{figure}
// \center
// \includegraphics[height=0.33\textwidth]{MeanSquaresMetricPlot1}
// \includegraphics[height=0.33\textwidth]{MeanSquaresMetricPlot2}
// \itkcaption[Mean Squares Metric Plots]{Plots of the Mean Squares Metric for
// an image compared to itself under multiple translations.}
// \label{fig:MeanSquaresMetricPlot}
// \end{figure}
//
// Running this code using the image BrainProtonDensitySlice.png as both the
// fixed and the moving images results in the plot shown in
// Figure~\ref{fig:MeanSquaresMetricPlot}. From this Figure, it can be seen
// that a gradient based optimizer will be appropriate for finding the extrema
// of the Metric. It is also possible to estimate a good value for the step
// length of a gradient-descent optimizer.
//
// This exercise of plotting the Metric is probably the best thing to do when a
// registration process is not converging and when it is unclear how to fine
// tune the different parameters involved in the registration. This includes
// the optimizer parameters, the metric parameters and even options such as
// preprocessing the image data with smoothing filters.
//
// The shell and Gnuplot\footnote{http://www.gnuplot.info} scripts used for
// generating the graphics in Figure~\ref{fig:MeanSquaresMetricPlot} are
// available in the directory
//
//             \code{ITKSoftwareGuide/SoftwareGuide/Art}
//
// Of course, this plotting exercise becomes more challenging when the
// transform has more than three parameters, and when those parameters have
// very different range of values. In those cases is necessary to select only a
// key subset of parameters from the transform and to study the behavior of the
// metric when those parameters are varied.
//
//
// Software Guide : EndLatex


  return EXIT_SUCCESS;
}
