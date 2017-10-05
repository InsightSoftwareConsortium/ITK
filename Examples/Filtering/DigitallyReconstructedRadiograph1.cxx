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
// This example illustrates the use of the
// \code{RayCastInterpolateImageFunction} class to generate digitally
// reconstructed radiographs (DRRs) from a 3D image volume such as CT
// or MR.
//
// Software Guide : EndLatex


#include "itkImage.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkResampleImageFilter.h"
#include "itkCenteredEuler3DTransform.h"
#include "itkNearestNeighborInterpolateImageFunction.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkRescaleIntensityImageFilter.h"

// Software Guide : BeginLatex
//
// The \code{RayCastInterpolateImageFunction} class definition for
// this example is contained in the following header file.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
#include "itkRayCastInterpolateImageFunction.h"
// Software Guide : EndCodeSnippet

//#define WRITE_CUBE_IMAGE_TO_FILE


void usage()
{
  std::cerr << "\n";
  std::cerr << "Usage: DRR <options> [input]\n";
  std::cerr << "  calculates the Digitally Reconstructed Radiograph from a volume. \n\n";
  std::cerr << " where <options> is one or more of the following:\n\n";
  std::cerr << "  <-h>                    Display (this) usage information\n";
  std::cerr << "  <-v>                    Verbose output [default: no]\n";
  std::cerr << "  <-res float float>      Pixel spacing of the output image [default: 1x1mm]  \n";
  std::cerr << "  <-size int int>         Dimension of the output image [default: 501x501]  \n";
  std::cerr << "  <-sid float>            Distance of ray source (focal point) [default: 400mm]\n";
  std::cerr << "  <-t float float float>  Translation parameter of the camera \n";
  std::cerr << "  <-rx float>             Rotation around x,y,z axis in degrees \n";
  std::cerr << "  <-ry float>\n";
  std::cerr << "  <-rz float>\n";
  std::cerr << "  <-normal float float>   The 2D projection normal position [default: 0x0mm]\n";
  std::cerr << "  <-cor float float float> The centre of rotation relative to centre of volume\n";
  std::cerr << "  <-threshold float>      Threshold [default: 0]\n";
  std::cerr << "  <-o file>               Output image filename\n\n";
  std::cerr << "                          by  thomas@hartkens.de\n";
  std::cerr << "                          and john.hipwell@kcl.ac.uk (CISG London)\n\n";
  exit(1);
}

int main( int argc, char *argv[] )
{
  char *input_name = ITK_NULLPTR;
  char *output_name = ITK_NULLPTR;

  bool ok;
  bool verbose = false;

  float rx = 0.;
  float ry = 0.;
  float rz = 0.;

  float tx = 0.;
  float ty = 0.;
  float tz = 0.;

  float cx = 0.;
  float cy = 0.;
  float cz = 0.;

  float sid = 400.;

  float sx = 1.;
  float sy = 1.;

  int dx = 501;
  int dy = 501;

  float o2Dx = 0;
  float o2Dy = 0;

  double threshold=0;


  // Parse command line parameters

  while (argc > 1)
    {
    ok = false;

    if ((ok == false) && (strcmp(argv[1], "-h") == 0))
      {
      argc--; argv++;
      ok = true;
      usage();
      }

    if ((ok == false) && (strcmp(argv[1], "-v") == 0))
      {
      argc--; argv++;
      ok = true;
      verbose = true;
      }

    if ((ok == false) && (strcmp(argv[1], "-rx") == 0))
      {
      argc--; argv++;
      ok = true;
      rx=atof(argv[1]);
      argc--; argv++;
      }

    if ((ok == false) && (strcmp(argv[1], "-ry") == 0))
      {
      argc--; argv++;
      ok = true;
      ry=atof(argv[1]);
      argc--; argv++;
      }

    if ((ok == false) && (strcmp(argv[1], "-rz") == 0))
      {
      argc--; argv++;
      ok = true;
      rz=atof(argv[1]);
      argc--; argv++;
      }

    if ((ok == false) && (strcmp(argv[1], "-threshold") == 0))
      {
      argc--; argv++;
      ok = true;
      threshold=atof(argv[1]);
      argc--; argv++;
      }

    if ((ok == false) && (strcmp(argv[1], "-t") == 0))
      {
      argc--; argv++;
      ok = true;
      tx=atof(argv[1]);
      argc--; argv++;
      ty=atof(argv[1]);
      argc--; argv++;
      tz=atof(argv[1]);
      argc--; argv++;
      }

    if ((ok == false) && (strcmp(argv[1], "-cor") == 0))
      {
      argc--; argv++;
      ok = true;
      cx=atof(argv[1]);
      argc--; argv++;
      cy=atof(argv[1]);
      argc--; argv++;
      cz=atof(argv[1]);
      argc--; argv++;
      }

    if ((ok == false) && (strcmp(argv[1], "-res") == 0))
      {
      argc--; argv++;
      ok = true;
      sx=atof(argv[1]);
      argc--; argv++;
      sy=atof(argv[1]);
      argc--; argv++;
      }

    if ((ok == false) && (strcmp(argv[1], "-size") == 0))
      {
      argc--; argv++;
      ok = true;
      dx=atoi(argv[1]);
      argc--; argv++;
      dy=atoi(argv[1]);
      argc--; argv++;
      }

    if ((ok == false) && (strcmp(argv[1], "-sid") == 0))
      {
      argc--; argv++;
      ok = true;
      sid=atof(argv[1]);
      argc--; argv++;
      }

    if ((ok == false) && (strcmp(argv[1], "-normal") == 0))
      {
      argc--; argv++;
      ok = true;
      o2Dx=atof(argv[1]);
      argc--; argv++;
      o2Dy=atof(argv[1]);
      argc--; argv++;
      }

    if ((ok == false) && (strcmp(argv[1], "-o") == 0))
      {
      argc--; argv++;
      ok = true;
      output_name = argv[1];
      argc--; argv++;
      }

    if (ok == false)
      {

      if (input_name == ITK_NULLPTR)
        {
        input_name = argv[1];
        argc--;
        argv++;
        }

      else
        {
        std::cerr << "ERROR: Can not parse argument " << argv[1] << std::endl;
        usage();
        }
      }
    }

  if (verbose)
    {
    if (input_name)  std::cout << "Input image: "  << input_name  << std::endl;
    if (output_name) std::cout << "Output image: " << output_name << std::endl;
    }

// Software Guide : BeginLatex
//
// Although we generate a 2D projection of the 3D volume for the
// purposes of the interpolator both images must be three dimensional.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  const     unsigned int   Dimension = 3;
  typedef   short         InputPixelType;
  typedef   unsigned char OutputPixelType;

  typedef itk::Image< InputPixelType,  Dimension >   InputImageType;
  typedef itk::Image< OutputPixelType, Dimension >   OutputImageType;

  InputImageType::Pointer image;
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// For the purposes of this example we assume the input volume has
// been loaded into an \code{itk::Image image}.
//
// Software Guide : EndLatex

  if (input_name)
    {

    typedef itk::ImageFileReader< InputImageType >  ReaderType;
    ReaderType::Pointer reader = ReaderType::New();
    reader->SetFileName( input_name );


    try
      {
      reader->Update();
      }
    catch( itk::ExceptionObject & err )
      {
      std::cerr << "ERROR: ExceptionObject caught !" << std::endl;
      std::cerr << err << std::endl;
      return EXIT_FAILURE;
      }

    image = reader->GetOutput();

    }
  else
    {   // No input image specified so create a cube

    image = InputImageType::New();

    InputImageType::SpacingType spacing;
    spacing[0] = 3.;
    spacing[1] = 3.;
    spacing[2] = 3.;
    image->SetSpacing( spacing );

    InputImageType::PointType origin;
    origin[0] = 0.;
    origin[1] = 0.;
    origin[2] = 0.;
    image->SetOrigin( origin );

    InputImageType::IndexType start;

    start[0] =   0;  // first index on X
    start[1] =   0;  // first index on Y
    start[2] =   0;  // first index on Z

    InputImageType::SizeType  size;

    size[0]  = 61;  // size along X
    size[1]  = 61;  // size along Y
    size[2]  = 61;  // size along Z

    InputImageType::RegionType region;

    region.SetSize( size );
    region.SetIndex( start );

    image->SetRegions( region );
    image->Allocate(true); // initialize to zero.

    image->Update();

    typedef itk::ImageRegionIteratorWithIndex< InputImageType > IteratorType;

    IteratorType iterate( image, image->GetLargestPossibleRegion() );

    while ( ! iterate.IsAtEnd() )
      {

      InputImageType::IndexType idx = iterate.GetIndex();

      if (   (idx[0] >= 6) && (idx[0] <= 54)
          && (idx[1] >= 6) && (idx[1] <= 54)
          && (idx[2] >= 6) && (idx[2] <= 54)

          && (   (   ((idx[0] <= 11) || (idx[0] >= 49))
                  && ((idx[1] <= 11) || (idx[1] >= 49)))

              || (   ((idx[0] <= 11) || (idx[0] >= 49))
                  && ((idx[2] <= 11) || (idx[2] >= 49)))

              || (   ((idx[1] <= 11) || (idx[1] >= 49))
                  && ((idx[2] <= 11) || (idx[2] >= 49))) ))
        {
        iterate.Set(10);
        }

      else if (   (idx[0] >= 18) && (idx[0] <= 42)
               && (idx[1] >= 18) && (idx[1] <= 42)
               && (idx[2] >= 18) && (idx[2] <= 42)

                  && (   (   ((idx[0] <= 23) || (idx[0] >= 37))
                          && ((idx[1] <= 23) || (idx[1] >= 37)))

                      || (   ((idx[0] <= 23) || (idx[0] >= 37))
                          && ((idx[2] <= 23) || (idx[2] >= 37)))

                      || (   ((idx[1] <= 23) || (idx[1] >= 37))
                          && ((idx[2] <= 23) || (idx[2] >= 37))) ))
        {
        iterate.Set(60);
        }

      else if ((idx[0] == 30) && (idx[1] == 30) && (idx[2] == 30))
        {
        iterate.Set(100);
        }

      ++iterate;
      }


#ifdef WRITE_CUBE_IMAGE_TO_FILE
    const char *filename = "cube.gipl";
    typedef itk::ImageFileWriter< InputImageType >  WriterType;
    WriterType::Pointer writer = WriterType::New();

    writer->SetFileName( filename );
    writer->SetInput( image );

    try
      {
      std::cout << "Writing image: " << filename << std::endl;
      writer->Update();
      }
    catch( itk::ExceptionObject & err )
      {

      std::cerr << "ERROR: ExceptionObject caught !" << std::endl;
      std::cerr << err << std::endl;
      return EXIT_FAILURE;
      }
#endif
  }


  // Print out the details of the input volume

  if (verbose)
    {
    unsigned int i;
    const InputImageType::SpacingType spacing = image->GetSpacing();
    std::cout << std::endl << "Input ";

    InputImageType::RegionType region = image->GetBufferedRegion();
    region.Print(std::cout);

    std::cout << "  Resolution: [";
    for (i=0; i<Dimension; i++)
      {
      std::cout << spacing[i];
      if (i < Dimension-1) std::cout << ", ";
      }
    std::cout << "]" << std::endl;

    const InputImageType::PointType origin = image->GetOrigin();
    std::cout << "  Origin: [";
    for (i=0; i<Dimension; i++)
      {
      std::cout << origin[i];
      if (i < Dimension-1) std::cout << ", ";
      }
    std::cout << "]" << std::endl<< std::endl;
    }

// Software Guide : BeginLatex
//
// Creation of a \code{ResampleImageFilter} enables coordinates for
// each of the pixels in the DRR image to be generated. These
// coordinates are used by the \code{RayCastInterpolateImageFunction}
// to determine the equation of each corresponding ray which is cast
// through the input volume.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::ResampleImageFilter<InputImageType, InputImageType > FilterType;

  FilterType::Pointer filter = FilterType::New();

  filter->SetInput( image );
  filter->SetDefaultPixelValue( 0 );
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// An Euler transformation is defined to position the input volume.
// The \code{ResampleImageFilter} uses this transform to position the
// output DRR image for the desired view.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::CenteredEuler3DTransform< double >  TransformType;

  TransformType::Pointer transform = TransformType::New();

  transform->SetComputeZYX(true);

  TransformType::OutputVectorType translation;

  translation[0] = tx;
  translation[1] = ty;
  translation[2] = tz;

  // constant for converting degrees into radians
  const double dtr = ( std::atan(1.0) * 4.0 ) / 180.0;

  transform->SetTranslation( translation );
  transform->SetRotation( dtr*rx, dtr*ry, dtr*rz );

  InputImageType::PointType   imOrigin = image->GetOrigin();
  InputImageType::SpacingType imRes    = image->GetSpacing();

  typedef InputImageType::RegionType     InputImageRegionType;
  typedef InputImageRegionType::SizeType InputImageSizeType;

  InputImageRegionType imRegion = image->GetBufferedRegion();
  InputImageSizeType   imSize   = imRegion.GetSize();

  imOrigin[0] += imRes[0] * static_cast<double>( imSize[0] ) / 2.0;
  imOrigin[1] += imRes[1] * static_cast<double>( imSize[1] ) / 2.0;
  imOrigin[2] += imRes[2] * static_cast<double>( imSize[2] ) / 2.0;

  TransformType::InputPointType center;
  center[0] = cx + imOrigin[0];
  center[1] = cy + imOrigin[1];
  center[2] = cz + imOrigin[2];

  transform->SetCenter(center);

  if (verbose)
    {
    std::cout << "Image size: "
       << imSize[0] << ", " << imSize[1] << ", " << imSize[2]
       << std::endl << "   resolution: "
       << imRes[0] << ", " << imRes[1] << ", " << imRes[2]
       << std::endl << "   origin: "
       << imOrigin[0] << ", " << imOrigin[1] << ", " << imOrigin[2]
       << std::endl << "   center: "
       << center[0] << ", " << center[1] << ", " << center[2]
       << std::endl << "Transform: " << transform << std::endl;
    }
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// The \code{RayCastInterpolateImageFunction} is instantiated and passed the transform
// object. The \code{RayCastInterpolateImageFunction} uses this
// transform to reposition the x-ray source such that the DRR image
// and x-ray source move as one around the input volume. This coupling
// mimics the rigid geometry of the x-ray gantry.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  typedef itk::RayCastInterpolateImageFunction<InputImageType,double>
                                                             InterpolatorType;
  InterpolatorType::Pointer interpolator = InterpolatorType::New();
  interpolator->SetTransform(transform);
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// We can then specify a threshold above which the volume's
// intensities will be integrated.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  interpolator->SetThreshold(threshold);
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// The ray-cast interpolator needs to know the initial position of the
// ray source or focal point. In this example we place the input
// volume at the origin and halfway between the ray source and the
// screen. The distance between the ray source and the screen
// is the "source to image distance" \code{sid} and is specified by
// the user.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  InterpolatorType::InputPointType focalpoint;

  focalpoint[0]= imOrigin[0];
  focalpoint[1]= imOrigin[1];
  focalpoint[2]= imOrigin[2] - sid/2.;

  interpolator->SetFocalPoint(focalpoint);
 // Software Guide : EndCodeSnippet

  if (verbose)
    {
    std::cout << "Focal Point: "
              << focalpoint[0] << ", "
              << focalpoint[1] << ", "
              << focalpoint[2] << std::endl;
    }

// Software Guide : BeginLatex
//
// Having initialised the interpolator we pass the object to the
// resample filter.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
  interpolator->Print (std::cout);

  filter->SetInterpolator( interpolator );
  filter->SetTransform( transform );
// Software Guide : EndCodeSnippet

// Software Guide : BeginLatex
//
// The size and resolution of the output DRR image is specified via the
// resample filter.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet

  // setup the scene
  InputImageType::SizeType   size;

  size[0] = dx;  // number of pixels along X of the 2D DRR image
  size[1] = dy;  // number of pixels along Y of the 2D DRR image
  size[2] = 1;   // only one slice

  filter->SetSize( size );

  InputImageType::SpacingType spacing;

  spacing[0] = sx;  // pixel spacing along X of the 2D DRR image [mm]
  spacing[1] = sy;  // pixel spacing along Y of the 2D DRR image [mm]
  spacing[2] = 1.0; // slice thickness of the 2D DRR image [mm]

  filter->SetOutputSpacing( spacing );

// Software Guide : EndCodeSnippet

  if (verbose)
    {
    std::cout << "Output image size: "
              << size[0] << ", "
              << size[1] << ", "
              << size[2] << std::endl;

    std::cout << "Output image spacing: "
              << spacing[0] << ", "
              << spacing[1] << ", "
              << spacing[2] << std::endl;
    }

// Software Guide : BeginLatex
//
// In addition the position of the DRR is specified. The default
// position of the input volume, prior to its transformation is
// half-way between the ray source and screen and unless specified
// otherwise the normal from the "screen" to the ray source passes
// directly through the centre of the DRR.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet

  double origin[ Dimension ];

  origin[0] = imOrigin[0] + o2Dx - sx*((double) dx - 1.)/2.;
  origin[1] = imOrigin[1] + o2Dy - sy*((double) dy - 1.)/2.;
  origin[2] = imOrigin[2] + sid/2.;

  filter->SetOutputOrigin( origin );
// Software Guide : EndCodeSnippet

  if (verbose)
    {
    std::cout << "Output image origin: "
              << origin[0] << ", "
              << origin[1] << ", "
              << origin[2] << std::endl;
    }

  // create writer

  if (output_name)
    {

// Software Guide : BeginLatex
//
// The output of the resample filter can then be passed to a writer to
// save the DRR image to a file.
//
// Software Guide : EndLatex

// Software Guide : BeginCodeSnippet
    typedef itk::RescaleIntensityImageFilter<
      InputImageType, OutputImageType > RescaleFilterType;
    RescaleFilterType::Pointer rescaler = RescaleFilterType::New();
    rescaler->SetOutputMinimum(   0 );
    rescaler->SetOutputMaximum( 255 );
    rescaler->SetInput( filter->GetOutput() );

    typedef itk::ImageFileWriter< OutputImageType >  WriterType;
    WriterType::Pointer writer = WriterType::New();

    writer->SetFileName( output_name );
    writer->SetInput( rescaler->GetOutput() );

    try
      {
      std::cout << "Writing image: " << output_name << std::endl;
      writer->Update();
      }
    catch( itk::ExceptionObject & err )
      {
      std::cerr << "ERROR: ExceptionObject caught !" << std::endl;
      std::cerr << err << std::endl;
      }
// Software Guide : EndCodeSnippet

    }
  else
    {
    filter->Update();
    }

  return EXIT_SUCCESS;
}
