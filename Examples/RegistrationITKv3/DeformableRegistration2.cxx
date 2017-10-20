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
//    INPUTS:  RatLungSlice1.mha
//    INPUTS:  RatLungSlice2.mha
//    ARGUMENTS: DeformableRegistration2Output.mha
//    ARGUMENTS: DeformableRegistration2Field.mha
//  Software Guide : EndCommandLineArgs


#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkImageRegionIterator.h"

// Software Guide : BeginLatex
//
// This example demonstrates how to use the ``demons'' algorithm to deformably
// register two images. The first step is to include the header files.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkDemonsRegistrationFilter.h"
#include "itkHistogramMatchingImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkWarpImageFilter.h"
// Software Guide : EndCodeSnippet


//  The following section of code implements a Command observer
//  that will monitor the evolution of the registration process.
//
  class CommandIterationUpdate : public itk::Command
  {
  public:
    typedef  CommandIterationUpdate                     Self;
    typedef  itk::Command                               Superclass;
    typedef  itk::SmartPointer<CommandIterationUpdate>  Pointer;
    itkNewMacro( CommandIterationUpdate );
  protected:
    CommandIterationUpdate() {};

    typedef itk::Image< float, 2 >            InternalImageType;
    typedef itk::Vector< float, 2 >           VectorPixelType;
    typedef itk::Image<  VectorPixelType, 2 > DisplacementFieldType;

    typedef itk::DemonsRegistrationFilter<
                                InternalImageType,
                                InternalImageType,
                                DisplacementFieldType>   RegistrationFilterType;

  public:

    void Execute(itk::Object *caller, const itk::EventObject & event) ITK_OVERRIDE
      {
        Execute( (const itk::Object *)caller, event);
      }

    void Execute(const itk::Object * object, const itk::EventObject & event) ITK_OVERRIDE
      {
         const RegistrationFilterType * filter = static_cast< const RegistrationFilterType * >( object );
        if( !(itk::IterationEvent().CheckEvent( &event )) )
          {
          return;
          }
        std::cout << filter->GetMetric() << std::endl;
      }
  };


int main( int argc, char *argv[] )
{
  if( argc < 4 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " fixedImageFile movingImageFile ";
    std::cerr << " outputImageFile " << std::endl;
    std::cerr << " [outputDisplacementFieldFile] " << std::endl;
    return EXIT_FAILURE;
    }

  // Software Guide : BeginLatex
  //
  // Second, we declare the types of the images.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  const unsigned int Dimension = 2;
  typedef unsigned short PixelType;

  typedef itk::Image< PixelType, Dimension >  FixedImageType;
  typedef itk::Image< PixelType, Dimension >  MovingImageType;
  // Software Guide : EndCodeSnippet

  // Set up the file readers
  typedef itk::ImageFileReader< FixedImageType  > FixedImageReaderType;
  typedef itk::ImageFileReader< MovingImageType > MovingImageReaderType;

  FixedImageReaderType::Pointer fixedImageReader   = FixedImageReaderType::New();
  MovingImageReaderType::Pointer movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName( argv[1] );
  movingImageReader->SetFileName( argv[2] );


  // Software Guide : BeginLatex
  //
  // Image file readers are set up in a similar fashion to previous examples.
  // To support the re-mapping of the moving image intensity, we declare an
  // internal image type with a floating point pixel type and cast the input
  // images to the internal image type.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef float                                      InternalPixelType;
  typedef itk::Image< InternalPixelType, Dimension > InternalImageType;
  typedef itk::CastImageFilter< FixedImageType,
                                InternalImageType >  FixedImageCasterType;
  typedef itk::CastImageFilter< MovingImageType,
                                InternalImageType >  MovingImageCasterType;

  FixedImageCasterType::Pointer fixedImageCaster = FixedImageCasterType::New();
  MovingImageCasterType::Pointer movingImageCaster
                                                = MovingImageCasterType::New();

  fixedImageCaster->SetInput( fixedImageReader->GetOutput() );
  movingImageCaster->SetInput( movingImageReader->GetOutput() );
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // The demons algorithm relies on the assumption that pixels representing the
  // same homologous point on an object have the same intensity on both the
  // fixed and moving images to be registered. In this example, we will
  // preprocess the moving image to match the intensity between the images
  // using the \doxygen{HistogramMatchingImageFilter}.
  //
  // \index{itk::HistogramMatchingImageFilter}
  //
  // The basic idea is to match the histograms of the two images at a
  // user-specified number of quantile values. For robustness, the histograms
  // are matched so that the background pixels are excluded from both
  // histograms.  For MR images, a simple procedure is to exclude all gray
  // values that are smaller than the mean gray value of the image.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::HistogramMatchingImageFilter<
                                    InternalImageType,
                                    InternalImageType >   MatchingFilterType;
  MatchingFilterType::Pointer matcher = MatchingFilterType::New();
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // For this example, we set the moving image as the source or input image and
  // the fixed image as the reference image.
  //
  // \index{itk::HistogramMatchingImageFilter!SetInput()}
  // \index{itk::HistogramMatchingImageFilter!SetSourceImage()}
  // \index{itk::HistogramMatchingImageFilter!SetReferenceImage()}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  matcher->SetInput( movingImageCaster->GetOutput() );
  matcher->SetReferenceImage( fixedImageCaster->GetOutput() );
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // We then select the number of bins to represent the histograms and the
  // number of points or quantile values where the histogram is to be
  // matched.
  //
  // \index{itk::HistogramMatchingImageFilter!SetNumberOfHistogramLevels()}
  // \index{itk::HistogramMatchingImageFilter!SetNumberOfMatchPoints()}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  matcher->SetNumberOfHistogramLevels( 1024 );
  matcher->SetNumberOfMatchPoints( 7 );
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // Simple background extraction is done by thresholding at the mean
  // intensity.
  //
  // \index{itk::HistogramMatchingImageFilter!ThresholdAtMeanIntensityOn()}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  matcher->ThresholdAtMeanIntensityOn();
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // In the \doxygen{DemonsRegistrationFilter}, the deformation field is
  // represented as an image whose pixels are floating point vectors.
  //
  // \index{itk::DemonsRegistrationFilter}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::Vector< float, Dimension >           VectorPixelType;
  typedef itk::Image<  VectorPixelType, Dimension > DisplacementFieldType;
  typedef itk::DemonsRegistrationFilter<
                                InternalImageType,
                                InternalImageType,
                                DisplacementFieldType> RegistrationFilterType;
  RegistrationFilterType::Pointer filter = RegistrationFilterType::New();
  // Software Guide : EndCodeSnippet


  // Create the Command observer and register it with the registration filter.
  //
  CommandIterationUpdate::Pointer observer = CommandIterationUpdate::New();
  filter->AddObserver( itk::IterationEvent(), observer );


  // Software Guide : BeginLatex
  //
  // The input fixed image is simply the output of the fixed image casting
  // filter.  The input moving image is the output of the histogram matching
  // filter.
  //
  // \index{itk::DemonsRegistrationFilter!SetFixedImage()}
  // \index{itk::DemonsRegistrationFilter!SetMovingImage()}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filter->SetFixedImage( fixedImageCaster->GetOutput() );
  filter->SetMovingImage( matcher->GetOutput() );
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // The demons registration filter has two parameters: the number of
  // iterations to be performed and the standard deviation of the Gaussian
  // smoothing kernel to be applied to the deformation field after each
  // iteration.
  // \index{itk::DemonsRegistrationFilter!SetNumberOfIterations()}
  // \index{itk::DemonsRegistrationFilter!SetStandardDeviations()}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filter->SetNumberOfIterations( 50 );
  filter->SetStandardDeviations( 1.0 );
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // The registration algorithm is triggered by updating the filter. The
  // filter output is the computed deformation field.
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  filter->Update();
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // The \doxygen{WarpImageFilter} can be used to warp the moving image with
  // the output deformation field. Like the \doxygen{ResampleImageFilter},
  // the WarpImageFilter requires the specification of the input image to be
  // resampled, an input image interpolator, and the output image spacing and
  // origin.
  //
  // \index{itk::WarpImageFilter}
  // \index{itk::WarpImageFilter!SetInput()}
  // \index{itk::WarpImageFilter!SetInterpolator()}
  // \index{itk::WarpImageFilter!SetOutputSpacing()}
  // \index{itk::WarpImageFilter!SetOutputOrigin()}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  typedef itk::WarpImageFilter<
                          MovingImageType,
                          MovingImageType,
                          DisplacementFieldType  >     WarperType;
  typedef itk::LinearInterpolateImageFunction<
                                   MovingImageType,
                                   double          >  InterpolatorType;
  WarperType::Pointer warper = WarperType::New();
  InterpolatorType::Pointer interpolator = InterpolatorType::New();
  FixedImageType::Pointer fixedImage = fixedImageReader->GetOutput();

  warper->SetInput( movingImageReader->GetOutput() );
  warper->SetInterpolator( interpolator );
  warper->SetOutputSpacing( fixedImage->GetSpacing() );
  warper->SetOutputOrigin( fixedImage->GetOrigin() );
  warper->SetOutputDirection( fixedImage->GetDirection() );
  // Software Guide : EndCodeSnippet


  // Software Guide : BeginLatex
  //
  // Unlike the ResampleImageFilter, the WarpImageFilter
  // warps or transform the input image with respect to the deformation field
  // represented by an image of vectors.  The resulting warped or resampled
  // image is written to file as per previous examples.
  //
  // \index{itk::WarpImageFilter!SetDisplacementField()}
  //
  // Software Guide : EndLatex

  // Software Guide : BeginCodeSnippet
  warper->SetDisplacementField( filter->GetOutput() );
  // Software Guide : EndCodeSnippet


  // Write warped image out to file
  typedef  unsigned char                           OutputPixelType;
  typedef itk::Image< OutputPixelType, Dimension > OutputImageType;
  typedef itk::CastImageFilter<
                        MovingImageType,
                        OutputImageType >          CastFilterType;
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;

  WriterType::Pointer      writer =  WriterType::New();
  CastFilterType::Pointer  caster =  CastFilterType::New();

  writer->SetFileName( argv[3] );

  caster->SetInput( warper->GetOutput() );
  writer->SetInput( caster->GetOutput()   );
  writer->Update();


  // Software Guide : BeginLatex
  //
  // Let's execute this example using the rat lung data from the previous example.
  // The associated data files can be found in \code{Examples/Data}:
  //
  // \begin{itemize}
  // \item \code{RatLungSlice1.mha}
  // \item \code{RatLungSlice2.mha}
  // \end{itemize}
  //
  // \begin{figure} \center
  // \includegraphics[width=0.44\textwidth]{DeformableRegistration2CheckerboardBefore}
  // \includegraphics[width=0.44\textwidth]{DeformableRegistration2CheckerboardAfter}
  // \itkcaption[Demon's deformable registration output]{Checkerboard comparisons
  // before and after demons-based deformable registration.}
  // \label{fig:DeformableRegistration2Output}
  // \end{figure}
  //
  // The result of the demons-based deformable registration is presented in
  // Figure \ref{fig:DeformableRegistration2Output}. The checkerboard
  // comparison shows that the algorithm was able to recover the misalignment
  // due to expiration.
  //
  // Software Guide : EndLatex


  // Software Guide : BeginLatex
  //
  // It may be also desirable to write the deformation field as an image of
  // vectors.  This can be done with the following code.
  //
  // Software Guide : EndLatex

  if( argc > 4 ) // if a fourth line argument has been provided...
    {

  // Software Guide : BeginCodeSnippet
  typedef itk::ImageFileWriter< DisplacementFieldType > FieldWriterType;
  FieldWriterType::Pointer fieldWriter = FieldWriterType::New();
  fieldWriter->SetFileName( argv[4] );
  fieldWriter->SetInput( filter->GetOutput() );

  fieldWriter->Update();
  // Software Guide : EndCodeSnippet

  // Software Guide : BeginLatex
  //
  // Note that the file format used for writing the deformation field must be
  // capable of representing multiple components per pixel. This is the case
  // for the MetaImage and VTK file formats for example.
  //
  // Software Guide : EndLatex

    }


  if( argc > 5 ) // if a fifth line argument has been provided...
    {

  typedef DisplacementFieldType            VectorImage2DType;
  typedef DisplacementFieldType::PixelType Vector2DType;

  VectorImage2DType::ConstPointer vectorImage2D = filter->GetOutput();

  VectorImage2DType::RegionType  region2D = vectorImage2D->GetBufferedRegion();
  VectorImage2DType::IndexType   index2D  = region2D.GetIndex();
  VectorImage2DType::SizeType    size2D   = region2D.GetSize();


  typedef itk::Vector< float,       3 >  Vector3DType;
  typedef itk::Image< Vector3DType, 3 >  VectorImage3DType;

  typedef itk::ImageFileWriter< VectorImage3DType > VectorImage3DWriterType;

  VectorImage3DWriterType::Pointer writer3D = VectorImage3DWriterType::New();

  VectorImage3DType::Pointer vectorImage3D = VectorImage3DType::New();

  VectorImage3DType::RegionType  region3D;
  VectorImage3DType::IndexType   index3D;
  VectorImage3DType::SizeType    size3D;

  index3D[0] = index2D[0];
  index3D[1] = index2D[1];
  index3D[2] = 0;

  size3D[0]  = size2D[0];
  size3D[1]  = size2D[1];
  size3D[2]  = 1;

  region3D.SetSize( size3D );
  region3D.SetIndex( index3D );

  vectorImage3D->SetRegions( region3D );
  vectorImage3D->Allocate();

  typedef itk::ImageRegionConstIterator< VectorImage2DType > Iterator2DType;

  typedef itk::ImageRegionIterator< VectorImage3DType > Iterator3DType;

  Iterator2DType  it2( vectorImage2D, region2D );
  Iterator3DType  it3( vectorImage3D, region3D );

  it2.GoToBegin();
  it3.GoToBegin();

  Vector2DType vector2D;
  Vector3DType vector3D;

  vector3D[2] = 0; // set Z component to zero.

  while( !it2.IsAtEnd() )
    {
    vector2D = it2.Get();
    vector3D[0] = vector2D[0];
    vector3D[1] = vector2D[1];
    it3.Set( vector3D );
    ++it2;
    ++it3;
    }


  writer3D->SetInput( vectorImage3D );

  writer3D->SetFileName( argv[5] );

  try
    {
    writer3D->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }


  }

  return EXIT_SUCCESS;
}
