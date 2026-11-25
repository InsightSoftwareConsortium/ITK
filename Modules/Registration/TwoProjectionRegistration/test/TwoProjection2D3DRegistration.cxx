/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

/*=========================================================================

 This program implements an intensity based 2D-3D registration algorithm using the
 SiddonJacobsRayCastInterpolateImageFunction and NormalizedCorrelationTwoImageToOneImageMetric
 similarity measure

 PowellOptimizer is used as the optimization method to avoid gradient calculation.
 Euler3DTransform instead of CenteredEuler3DTransform is used to avoid the shift of the center.

 When generating DRRs, the program attempts to simulate a 2D imaging system attached to a linac
 for radiation therapy. The program registers two 2D portal images with their corresponding DRR
 images generated from the 3D dataset. The portal images may be acquired at any arbitrary projection
 angles. The similarity measure is the summation of the measures calculated each projection.
 Multiresolution strategy was not implemented.

 This program was modified from the ITK application--IntensityBased2D3DRegistration.cxx

=========================================================================*/
#include "itkTwoProjectionImageRegistrationMethod.h"

// The transformation used is a rigid 3D Euler transform with the
// provision of a center of rotation which defaults to the center of
// the 3D volume. In practice the center of the particular feature of
// interest in the 3D volume should be used.

#include "itkEuler3DTransform.h"

// We have chosen to implement the registration using the normalized coorelation
// similarity measure.

// #include "itkGradientDifferenceTwoImageToOneImageMetric.h"
#include "itkNormalizedCorrelationTwoImageToOneImageMetric.h"

// This is an intensity based registration algorithm so ray casting is
// used to project the 3D volume onto pixels in the target 2D image.
#include "itkSiddonJacobsRayCastInterpolateImageFunction.h"

// Finally the Powell optimizer is used to avoid requiring gradient information.
#include "itkPowellOptimizer.h"

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkResampleImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkFlipImageFilter.h"

#include "itkCommand.h"
#include "itkTimeProbesCollectorBase.h"


// First we define the command class to allow us to monitor the registration.

class CommandIterationUpdate : public itk::Command
{
public:
  using Self = CommandIterationUpdate;
  using Superclass = itk::Command;
  using Pointer = itk::SmartPointer<Self>;
  itkNewMacro(Self);

protected:
  CommandIterationUpdate() = default;

public:
  using OptimizerType = itk::PowellOptimizer;
  using OptimizerPointer = const OptimizerType *;

  void
  Execute(itk::Object * caller, const itk::EventObject & event) override
  {
    Execute((const itk::Object *)caller, event);
  }

  void
  Execute(const itk::Object * object, const itk::EventObject & event) override
  {
    auto optimizer = dynamic_cast<OptimizerPointer>(object);
    if (typeid(event) != typeid(itk::IterationEvent))
    {
      return;
    }
    //    std::cout << "Iteration: " << optimizer->GetCurrentIteration() << std::endl;
    std::cout << "Similarity: " << optimizer->GetValue() << std::endl;
    std::cout << "Position: " << optimizer->GetCurrentPosition() << std::endl;
  }
};


void
exe_usage()
{
  std::cerr << "\n";
  std::cerr << "Usage: TwoProjection2D3DRegistration <options> Image2D1 ProjAngle1 Image2D2 ProjAngle2 Volume3D\n";
  std::cerr << "       Registers two 2D images to a 3D volume. \n\n";
  std::cerr << "   where <options> is one or more of the following:\n\n";
  std::cerr << "       <-h>                     Display (this) usage information\n";
  std::cerr << "       <-v>                     Verbose output [default: no]\n";
  std::cerr << "       <-dbg>                   Debugging output [default: no]\n";
  std::cerr << "       <-scd float>             Source to isocenter distance [default: 1000mm]\n";
  std::cerr << "       <-t float float float>   CT volume translation in x, y, and z direction in mm \n";
  std::cerr << "       <-rx float>              CT volume rotation about x axis in degrees \n";
  std::cerr << "       <-ry float>              CT volume rotation about y axis in degrees \n";
  std::cerr << "       <-rz float>              CT volume rotation about z axis in degrees \n";
  std::cerr
    << "       <-2dcx float float float float>    Central axis positions of the 2D images in continuous indices \n";
  std::cerr << "       <-res float float float float>     Pixel spacing of projection images in the isocenter plane "
               "[default: 1x1 mm]  \n";
  std::cerr << "       <-iso float float float> Isocenter location in voxel in indices (center of rotation and "
               "projection center)\n";
  std::cerr << "       <-threshold float>       Intensity threshold below which are ignore [default: 0]\n";
  std::cerr << "       <-o file>                Output image filename\n\n";
  std::cerr << "                                by  Jian Wu\n";
  std::cerr << "                                eewujian@hotmail.com\n";
  std::cerr << "                                (Univeristy of Florida)\n\n";
  exit(EXIT_FAILURE);
}


int
TwoProjection2D3DRegistration(int argc, char * argv[])
{
  char * fileImage2D1 = nullptr;
  double projAngle1 = -999;
  char * fileImage2D2 = nullptr;
  double projAngle2 = -999;
  char * fileVolume3D = nullptr;
  // Default output file names
  const char * fileOutput1 = "Image2D1_Registered.tif";
  const char * fileOutput2 = "Image2D2_Registered.tif";

  bool ok;
  bool verbose = false;
  bool debug = false;
  bool customized_iso = false;
  bool customized_2DCX = false;  // Flag for customized 2D image central axis positions
  bool customized_2DRES = false; // Flag for customized 2D image pixel spacing

  double rx = 0.;
  double ry = 0.;
  double rz = 0.;

  double tx = 0.;
  double ty = 0.;
  double tz = 0.;

  double cx = 0.;
  double cy = 0.;
  double cz = 0.;

  double scd = 1000.; // Source to isocenter distance

  double image1centerX = 0.0;
  double image1centerY = 0.0;
  double image2centerX = 0.0;
  double image2centerY = 0.0;

  double image1resX = 1.0;
  double image1resY = 1.0;
  double image2resX = 1.0;
  double image2resY = 1.0;

  double threshold = 0.0;

  // Parse command line parameters

  if (argc <= 5)
    exe_usage();

  while (argc > 1)
  {
    ok = false;

    if ((ok == false) && (strcmp(argv[1], "-h") == 0))
    {
      argc--;
      argv++;
      ok = true;
      exe_usage();
    }

    if ((ok == false) && (strcmp(argv[1], "-v") == 0))
    {
      argc--;
      argv++;
      ok = true;
      verbose = true;
    }

    if ((ok == false) && (strcmp(argv[1], "-dbg") == 0))
    {
      argc--;
      argv++;
      ok = true;
      debug = true;
    }

    if ((ok == false) && (strcmp(argv[1], "-scd") == 0))
    {
      argc--;
      argv++;
      ok = true;
      scd = atof(argv[1]);
      argc--;
      argv++;
    }

    if ((ok == false) && (strcmp(argv[1], "-t") == 0))
    {
      argc--;
      argv++;
      ok = true;
      tx = atof(argv[1]);
      argc--;
      argv++;
      ty = atof(argv[1]);
      argc--;
      argv++;
      tz = atof(argv[1]);
      argc--;
      argv++;
    }

    if ((ok == false) && (strcmp(argv[1], "-rx") == 0))
    {
      argc--;
      argv++;
      ok = true;
      rx = atof(argv[1]);
      argc--;
      argv++;
    }

    if ((ok == false) && (strcmp(argv[1], "-ry") == 0))
    {
      argc--;
      argv++;
      ok = true;
      ry = atof(argv[1]);
      argc--;
      argv++;
    }

    if ((ok == false) && (strcmp(argv[1], "-rz") == 0))
    {
      argc--;
      argv++;
      ok = true;
      rz = atof(argv[1]);
      argc--;
      argv++;
    }

    if ((ok == false) && (strcmp(argv[1], "-2dcx") == 0))
    {
      argc--;
      argv++;
      ok = true;
      image1centerX = atof(argv[1]);
      argc--;
      argv++;
      image1centerY = atof(argv[1]);
      argc--;
      argv++;
      image2centerX = atof(argv[1]);
      argc--;
      argv++;
      image2centerY = atof(argv[1]);
      argc--;
      argv++;
      customized_2DCX = true;
    }

    if ((ok == false) && (strcmp(argv[1], "-res") == 0))
    {
      argc--;
      argv++;
      ok = true;
      image1resX = atof(argv[1]);
      argc--;
      argv++;
      image1resY = atof(argv[1]);
      argc--;
      argv++;
      image2resX = atof(argv[1]);
      argc--;
      argv++;
      image2resY = atof(argv[1]);
      argc--;
      argv++;
      customized_2DRES = true;
    }

    if ((ok == false) && (strcmp(argv[1], "-iso") == 0))
    {
      argc--;
      argv++;
      ok = true;
      cx = atof(argv[1]);
      argc--;
      argv++;
      cy = atof(argv[1]);
      argc--;
      argv++;
      cz = atof(argv[1]);
      argc--;
      argv++;
      customized_iso = true;
    }

    if ((ok == false) && (strcmp(argv[1], "-threshold") == 0))
    {
      argc--;
      argv++;
      ok = true;
      threshold = atof(argv[1]);
      argc--;
      argv++;
    }

    if ((ok == false) && (strcmp(argv[1], "-o") == 0))
    {
      argc--;
      argv++;
      ok = true;
      fileOutput1 = argv[1];
      argc--;
      argv++;
      fileOutput2 = argv[1];
      argc--;
      argv++;
    }


    if (ok == false)
    {

      if (fileImage2D1 == nullptr)
      {
        fileImage2D1 = argv[1];
        argc--;
        argv++;
      }

      if (projAngle1 == -999)
      {
        projAngle1 = atof(argv[1]);
        argc--;
        argv++;
      }

      if (fileImage2D2 == nullptr)
      {
        fileImage2D2 = argv[1];
        argc--;
        argv++;
      }

      if (projAngle2 == -999)
      {
        projAngle2 = atof(argv[1]);
        argc--;
        argv++;
      }

      else if (fileVolume3D == nullptr)
      {
        fileVolume3D = argv[1];
        argc--;
        argv++;
      }

      else
      {
        std::cerr << "ERROR: Cannot parse argument " << argv[1] << std::endl;
        exe_usage();
      }
    }
  }

  if (verbose)
  {
    if (fileImage2D1)
      std::cout << "Input 2D image 1: " << fileImage2D1 << std::endl;
    if (fileImage2D1)
      std::cout << "Projection Angle 1: " << projAngle1 << std::endl;
    if (fileImage2D2)
      std::cout << "Input 2D image 2: " << fileImage2D2 << std::endl;
    if (fileImage2D2)
      std::cout << "Projection Angle 2: " << projAngle2 << std::endl;
    if (fileVolume3D)
      std::cout << "Input 3D image: " << fileVolume3D << std::endl;
    if (fileOutput1)
      std::cout << "Output image 1: " << fileOutput1 << std::endl;
    if (fileOutput2)
      std::cout << "Output image 2: " << fileOutput2 << std::endl;
  }


  // We begin the program proper by defining the 2D and 3D images. The
  // {TwoProjectionImageRegistrationMethod} requires that both
  // images have the same dimension so the 2D image is given
  // dimension 3 and the size of the {z} dimension is set to unity.

  constexpr unsigned int Dimension = 3;
  using InternalPixelType = float;
  using PixelType3D = short;

  using ImageType3D = itk::Image<PixelType3D, Dimension>;

  using OutputPixelType = unsigned char;
  using OutputImageType = itk::Image<OutputPixelType, Dimension>;

  // The following lines define each of the components used in the
  // registration: The transform, optimizer, metric, interpolator and
  // the registration method itself.

  using InternalImageType = itk::Image<InternalPixelType, Dimension>;

  using TransformType = itk::Euler3DTransform<double>;

  using OptimizerType = itk::PowellOptimizer;

  // using MetricType = itk::GradientDifferenceTwoImageToOneImageMetric<
  using MetricType = itk::NormalizedCorrelationTwoImageToOneImageMetric<InternalImageType, InternalImageType>;

  using InterpolatorType = itk::SiddonJacobsRayCastInterpolateImageFunction<InternalImageType, double>;


  using RegistrationType = itk::TwoProjectionImageRegistrationMethod<InternalImageType, InternalImageType>;


  // Each of the registration components are instantiated in the
  // usual way...

  MetricType::Pointer       metric = MetricType::New();
  TransformType::Pointer    transform = TransformType::New();
  OptimizerType::Pointer    optimizer = OptimizerType::New();
  InterpolatorType::Pointer interpolator1 = InterpolatorType::New();
  InterpolatorType::Pointer interpolator2 = InterpolatorType::New();
  RegistrationType::Pointer registration = RegistrationType::New();

  metric->ComputeGradientOff();
  metric->SetSubtractMean(true);

  // and passed to the registration method:

  registration->SetMetric(metric);
  registration->SetOptimizer(optimizer);
  registration->SetTransform(transform);
  registration->SetInterpolator1(interpolator1);
  registration->SetInterpolator2(interpolator2);

  if (debug)
  {
    metric->DebugOn();
    // transform->DebugOn();
    // optimizer->DebugOn();
    interpolator1->DebugOn();
    interpolator2->DebugOn();
    // registration->DebugOn();
  }


  //  The 2- and 3-D images are read from files,

  using ImageReaderType2D = itk::ImageFileReader<InternalImageType>;
  using ImageReaderType3D = itk::ImageFileReader<ImageType3D>;

  ImageReaderType2D::Pointer imageReader2D1 = ImageReaderType2D::New();
  ImageReaderType2D::Pointer imageReader2D2 = ImageReaderType2D::New();
  ImageReaderType3D::Pointer imageReader3D = ImageReaderType3D::New();

  imageReader2D1->SetFileName(fileImage2D1);
  imageReader2D2->SetFileName(fileImage2D2);
  imageReader3D->SetFileName(fileVolume3D);
  imageReader3D->Update();

  ImageType3D::Pointer image3DIn = imageReader3D->GetOutput();

  // To simply Siddon-Jacob's fast ray-tracing algorithm, we force the origin of the CT image
  // to be (0,0,0). Because we align the CT isocenter with the central axis, the projection
  // geometry is fully defined. The origin of the CT image becomes irrelavent.
  ImageType3D::PointType image3DOrigin;
  image3DOrigin[0] = 0.0;
  image3DOrigin[1] = 0.0;
  image3DOrigin[2] = 0.0;
  image3DIn->SetOrigin(image3DOrigin);

  InternalImageType::Pointer image_tmp1 = imageReader2D1->GetOutput();
  InternalImageType::Pointer image_tmp2 = imageReader2D2->GetOutput();

  imageReader2D1->Update();
  imageReader2D2->Update();

  if (customized_2DRES)
  {
    InternalImageType::SpacingType spacing;
    spacing[0] = image1resX;
    spacing[1] = image1resY;
    spacing[2] = 1.0;
    image_tmp1->SetSpacing(spacing);

    spacing[0] = image2resX;
    spacing[1] = image2resY;
    image_tmp2->SetSpacing(spacing);
  }
  // The input 2D images were loaded as 3D images. They were considered
  // as a single slice from a 3D volume. By default, images stored on the
  // disk are treated as if they have RAI orientation. After view point
  // transformation, the order of 2D image pixel reading is equivalent to
  // from inferior to superior. This is contradictory to the traditional
  // 2D x-ray image storage, in which a typical 2D image reader reads and
  // writes images from superior to inferior. Thus the loaded 2D DICOM
  // images should be flipped in y-direction. This was done by using a.
  // FilpImageFilter.
  using FlipFilterType = itk::FlipImageFilter<InternalImageType>;
  FlipFilterType::Pointer flipFilter1 = FlipFilterType::New();
  FlipFilterType::Pointer flipFilter2 = FlipFilterType::New();

  using FlipAxesArrayType = FlipFilterType::FlipAxesArrayType;
  FlipAxesArrayType flipArray;
  flipArray[0] = false;
  flipArray[1] = true;
  flipArray[2] = false;

  flipFilter1->SetFlipAxes(flipArray);
  flipFilter2->SetFlipAxes(flipArray);

  flipFilter1->SetInput(imageReader2D1->GetOutput());
  flipFilter2->SetInput(imageReader2D2->GetOutput());

  // The input 2D images may have 16 bits. We rescale the pixel value to between 0-255.
  using Input2DRescaleFilterType = itk::RescaleIntensityImageFilter<InternalImageType, InternalImageType>;

  Input2DRescaleFilterType::Pointer rescaler2D1 = Input2DRescaleFilterType::New();
  rescaler2D1->SetOutputMinimum(0);
  rescaler2D1->SetOutputMaximum(255);
  rescaler2D1->SetInput(flipFilter1->GetOutput());

  Input2DRescaleFilterType::Pointer rescaler2D2 = Input2DRescaleFilterType::New();
  rescaler2D2->SetOutputMinimum(0);
  rescaler2D2->SetOutputMaximum(255);
  rescaler2D2->SetInput(flipFilter2->GetOutput());


  //  The 3D CT dataset is casted to the internal image type using
  //  {CastImageFilters}.

  using CastFilterType3D = itk::CastImageFilter<ImageType3D, InternalImageType>;

  CastFilterType3D::Pointer caster3D = CastFilterType3D::New();
  caster3D->SetInput(image3DIn);

  rescaler2D1->Update();
  rescaler2D2->Update();
  caster3D->Update();


  registration->SetFixedImage1(rescaler2D1->GetOutput());
  registration->SetFixedImage2(rescaler2D2->GetOutput());
  registration->SetMovingImage(caster3D->GetOutput());

  // Initialise the transform
  // ~~~~~~~~~~~~~~~~~~~~~~~~

  // Set the order of the computation. Default ZXY
  transform->SetComputeZYX(true);


  // The transform is initialised with the translation [tx,ty,tz] and
  // rotation [rx,ry,rz] specified on the command line

  TransformType::OutputVectorType translation;

  translation[0] = tx;
  translation[1] = ty;
  translation[2] = tz;

  transform->SetTranslation(translation);

  // constant for converting degrees to radians
  const double dtr = (atan(1.0) * 4.0) / 180.0;
  transform->SetRotation(dtr * rx, dtr * ry, dtr * rz);

  // The centre of rotation is set by default to the centre of the 3D
  // volume but can be offset from this position using a command
  // line specified translation [cx,cy,cz]

  ImageType3D::PointType       origin3D = image3DIn->GetOrigin();
  const itk::Vector<double, 3> resolution3D = image3DIn->GetSpacing();

  using ImageRegionType3D = ImageType3D::RegionType;
  using SizeType3D = ImageRegionType3D::SizeType;

  ImageRegionType3D region3D = caster3D->GetOutput()->GetBufferedRegion();
  SizeType3D        size3D = region3D.GetSize();

  TransformType::InputPointType isocenter;
  if (customized_iso)
  {
    // Isocenter location given by the user.
    isocenter[0] = origin3D[0] + resolution3D[0] * cx;
    isocenter[1] = origin3D[1] + resolution3D[1] * cy;
    isocenter[2] = origin3D[2] + resolution3D[2] * cz;
  }
  else
  {
    // Set the center of the image as the isocenter.
    isocenter[0] = origin3D[0] + resolution3D[0] * static_cast<double>(size3D[0]) / 2.0;
    isocenter[1] = origin3D[1] + resolution3D[1] * static_cast<double>(size3D[1]) / 2.0;
    isocenter[2] = origin3D[2] + resolution3D[2] * static_cast<double>(size3D[2]) / 2.0;
  }

  transform->SetCenter(isocenter);


  if (verbose)
  {
    std::cout << "3D image size: " << size3D[0] << ", " << size3D[1] << ", " << size3D[2] << std::endl
              << "   resolution: " << resolution3D[0] << ", " << resolution3D[1] << ", " << resolution3D[2] << std::endl
              << "Transform: " << transform << std::endl;
  }


  // Set the origin of the 2D image
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  // For correct (perspective) projection of the 3D volume, the 2D
  // image needs to be placed at a certain distance (the source-to-
  // isocenter distance {scd} ) from the focal point, and the normal
  // from the imaging plane to the focal point needs to be specified.
  //
  // By default, the imaging plane normal is set by default to the
  // center of the 2D image but may be modified from this using the
  // command line parameters [image1centerX, image1centerY,
  // image2centerX, image2centerY].

  double origin2D1[Dimension];
  double origin2D2[Dimension];

  // Note: Two 2D images may have different image sizes and pixel dimensions, although
  // scd are the same.

  const itk::Vector<double, 3> resolution2D1 = imageReader2D1->GetOutput()->GetSpacing();
  const itk::Vector<double, 3> resolution2D2 = imageReader2D2->GetOutput()->GetSpacing();

  using ImageRegionType2D = InternalImageType::RegionType;
  using SizeType2D = ImageRegionType2D::SizeType;

  ImageRegionType2D region2D1 = rescaler2D1->GetOutput()->GetBufferedRegion();
  ImageRegionType2D region2D2 = rescaler2D2->GetOutput()->GetBufferedRegion();
  SizeType2D        size2D1 = region2D1.GetSize();
  SizeType2D        size2D2 = region2D2.GetSize();

  if (!customized_2DCX)
  { // Central axis positions are not given by the user. Use the image centers
    // as the central axis position.
    image1centerX = ((double)size2D1[0] - 1.) / 2.;
    image1centerY = ((double)size2D1[1] - 1.) / 2.;
    image2centerX = ((double)size2D2[0] - 1.) / 2.;
    image2centerY = ((double)size2D2[1] - 1.) / 2.;
  }

  // 2D Image 1
  origin2D1[0] = -resolution2D1[0] * image1centerX;
  origin2D1[1] = -resolution2D1[1] * image1centerY;
  origin2D1[2] = -scd;

  imageReader2D1->GetOutput()->SetOrigin(origin2D1);
  rescaler2D1->GetOutput()->SetOrigin(origin2D1);

  // 2D Image 2
  origin2D2[0] = -resolution2D2[0] * image2centerX;
  origin2D2[1] = -resolution2D2[1] * image2centerY;
  origin2D2[2] = -scd;

  imageReader2D2->GetOutput()->SetOrigin(origin2D2);
  rescaler2D2->GetOutput()->SetOrigin(origin2D2);

  registration->SetFixedImageRegion1(rescaler2D1->GetOutput()->GetBufferedRegion());
  registration->SetFixedImageRegion2(rescaler2D2->GetOutput()->GetBufferedRegion());

  if (verbose)
  {
    std::cout << "2D image 1 size: " << size2D1[0] << ", " << size2D1[1] << ", " << size2D1[2] << std::endl
              << "   resolution: " << resolution2D1[0] << ", " << resolution2D1[1] << ", " << resolution2D1[2]
              << std::endl
              << "   and position: " << origin2D1[0] << ", " << origin2D1[1] << ", " << origin2D1[2] << std::endl
              << "2D image 2 size: " << size2D2[0] << ", " << size2D2[1] << ", " << size2D2[2] << std::endl
              << "   resolution: " << resolution2D2[0] << ", " << resolution2D2[1] << ", " << resolution2D2[2]
              << std::endl
              << "   and position: " << origin2D2[0] << ", " << origin2D2[1] << ", " << origin2D2[2] << std::endl;
  }

  // Initialize the ray cast interpolator
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  // The ray cast interpolator is used to project the 3D volume. It
  // does this by casting rays from the (transformed) focal point to
  // each (transformed) pixel coordinate in the 2D image.
  //
  // In addition a threshold may be specified to ensure that only
  // intensities greater than a given value contribute to the
  // projected volume. This can be used, for instance, to remove soft
  // tissue from projections of CT data and force the registration
  // to find a match which aligns bony structures in the images.

  // 2D Image 1
  interpolator1->SetProjectionAngle(dtr * projAngle1);
  interpolator1->SetFocalPointToIsocenterDistance(scd);
  interpolator1->SetThreshold(threshold);
  interpolator1->SetTransform(transform);

  interpolator1->Initialize();

  // 2D Image 2
  interpolator2->SetProjectionAngle(dtr * projAngle2);
  interpolator2->SetFocalPointToIsocenterDistance(scd);
  interpolator2->SetThreshold(threshold);
  interpolator2->SetTransform(transform);

  interpolator2->Initialize();


  // Set up the transform and start position
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  // The registration start position is intialised using the
  // transformation parameters.

  registration->SetInitialTransformParameters(transform->GetParameters());

  // We wish to minimize the negative normalized correlation similarity measure.

  // optimizer->SetMaximize( true );  // for GradientDifferenceTwoImageToOneImageMetric
  optimizer->SetMaximize(false); // for NCC

  optimizer->SetMaximumIteration(10);
  optimizer->SetMaximumLineIteration(4); // for Powell's method
  optimizer->SetStepLength(4.0);
  optimizer->SetStepTolerance(0.02);
  optimizer->SetValueTolerance(0.001);

  // The optimizer weightings are set such that one degree equates to
  // one millimeter.

  itk::Optimizer::ScalesType weightings(transform->GetNumberOfParameters());

  weightings[0] = 1. / dtr;
  weightings[1] = 1. / dtr;
  weightings[2] = 1. / dtr;
  weightings[3] = 1.;
  weightings[4] = 1.;
  weightings[5] = 1.;

  optimizer->SetScales(weightings);

  if (verbose)
  {
    optimizer->Print(std::cout);
  }


  // Create the observers
  // ~~~~~~~~~~~~~~~~~~~~

  CommandIterationUpdate::Pointer observer = CommandIterationUpdate::New();

  optimizer->AddObserver(itk::IterationEvent(), observer);


  // Start the registration
  // ~~~~~~~~~~~~~~~~~~~~~~

  // Create a timer to record calculation time.
  itk::TimeProbesCollectorBase timer;

  if (verbose)
  {
    std::cout << "Starting the registration now..." << std::endl;
  }

  try
  {
    timer.Start("Registration");
    // Start the registration.
    registration->StartRegistration();
    timer.Stop("Registration");
  }
  catch (itk::ExceptionObject & err)
  {
    std::cout << "ExceptionObject caught !" << std::endl;
    std::cout << err << std::endl;
    return -1;
  }

  using ParametersType = RegistrationType::ParametersType;
  ParametersType finalParameters = registration->GetLastTransformParameters();

  const double RotationAlongX = finalParameters[0] / dtr; // Convert radian to degree
  const double RotationAlongY = finalParameters[1] / dtr;
  const double RotationAlongZ = finalParameters[2] / dtr;
  const double TranslationAlongX = finalParameters[3];
  const double TranslationAlongY = finalParameters[4];
  const double TranslationAlongZ = finalParameters[5];

  const int numberOfIterations = optimizer->GetCurrentIteration();

  const double bestValue = optimizer->GetValue();

  std::cout << "Result = " << std::endl;
  std::cout << " Rotation Along X = " << RotationAlongX << " deg" << std::endl;
  std::cout << " Rotation Along Y = " << RotationAlongY << " deg" << std::endl;
  std::cout << " Rotation Along Z = " << RotationAlongZ << " deg" << std::endl;
  std::cout << " Translation X = " << TranslationAlongX << " mm" << std::endl;
  std::cout << " Translation Y = " << TranslationAlongY << " mm" << std::endl;
  std::cout << " Translation Z = " << TranslationAlongZ << " mm" << std::endl;
  std::cout << " Number Of Iterations = " << numberOfIterations << std::endl;
  std::cout << " Metric value  = " << bestValue << std::endl;


  // Write out the projection images at the registration position
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  TransformType::Pointer finalTransform = TransformType::New();
  // The transform is determined by the parameters and the rotation center.
  finalTransform->SetParameters(finalParameters);
  finalTransform->SetCenter(isocenter);

  using ResampleFilterType = itk::ResampleImageFilter<InternalImageType, InternalImageType>;

  // The ResampleImageFilter is the driving force for the projection image generation.
  ResampleFilterType::Pointer resampleFilter1 = ResampleFilterType::New();

  resampleFilter1->SetInput(caster3D->GetOutput()); // Link the 3D volume.
  resampleFilter1->SetDefaultPixelValue(0);

  // The parameters of interpolator1, such as ProjectionAngle and FocalPointToIsocenterDistance
  // have been set before registration. Here we only need to replace the initial
  // transform with the final transform.
  interpolator1->SetTransform(finalTransform);
  interpolator1->Initialize();
  resampleFilter1->SetInterpolator(interpolator1);

  // The output 2D projection image has the same image size, origin, and the pixel spacing as
  // those of the input 2D image.
  resampleFilter1->SetSize(rescaler2D1->GetOutput()->GetLargestPossibleRegion().GetSize());
  resampleFilter1->SetOutputOrigin(rescaler2D1->GetOutput()->GetOrigin());
  resampleFilter1->SetOutputSpacing(rescaler2D1->GetOutput()->GetSpacing());

  // Do the same thing for the output image 2.
  ResampleFilterType::Pointer resampleFilter2 = ResampleFilterType::New();
  resampleFilter2->SetInput(caster3D->GetOutput());
  resampleFilter2->SetDefaultPixelValue(0);

  // The parameters of interpolator2, such as ProjectionAngle and FocalPointToIsocenterDistance
  // have been set before registration. Here we only need to replace the initial
  // transform with the final transform.
  interpolator2->SetTransform(finalTransform);
  interpolator2->Initialize();
  resampleFilter2->SetInterpolator(interpolator2);

  resampleFilter2->SetSize(rescaler2D2->GetOutput()->GetLargestPossibleRegion().GetSize());
  resampleFilter2->SetOutputOrigin(rescaler2D2->GetOutput()->GetOrigin());
  resampleFilter2->SetOutputSpacing(rescaler2D2->GetOutput()->GetSpacing());

  /////////////////////////////---DEGUG--START----////////////////////////////////////
  if (debug)
  {
    InternalImageType::PointType outputorigin2D1 = rescaler2D1->GetOutput()->GetOrigin();
    std::cout << "Output image 1 origin: " << outputorigin2D1[0] << ", " << outputorigin2D1[1] << ", "
              << outputorigin2D1[2] << std::endl;
    InternalImageType::PointType outputorigin2D2 = rescaler2D2->GetOutput()->GetOrigin();
    std::cout << "Output image 2 origin: " << outputorigin2D2[0] << ", " << outputorigin2D2[1] << ", "
              << outputorigin2D2[2] << std::endl;
  }
  /////////////////////////////---DEGUG--END----//////////////////////////////////////


  // As explained before, the computed projection is upsided-down.
  // Here we use a FilpImageFilter to flip the images in y-direction.
  flipFilter1->SetInput(resampleFilter1->GetOutput());
  flipFilter2->SetInput(resampleFilter2->GetOutput());

  // Rescale the intensity of the projection images to 0-255 for output.
  using RescaleFilterType = itk::RescaleIntensityImageFilter<InternalImageType, OutputImageType>;

  RescaleFilterType::Pointer rescaler1 = RescaleFilterType::New();
  rescaler1->SetOutputMinimum(0);
  rescaler1->SetOutputMaximum(255);
  rescaler1->SetInput(flipFilter1->GetOutput());

  RescaleFilterType::Pointer rescaler2 = RescaleFilterType::New();
  rescaler2->SetOutputMinimum(0);
  rescaler2->SetOutputMaximum(255);
  rescaler2->SetInput(flipFilter2->GetOutput());


  using WriterType = itk::ImageFileWriter<OutputImageType>;
  WriterType::Pointer writer1 = WriterType::New();
  WriterType::Pointer writer2 = WriterType::New();

  writer1->SetFileName(fileOutput1);
  writer1->SetInput(rescaler1->GetOutput());

  try
  {
    std::cout << "Writing image: " << fileOutput1 << std::endl;
    writer1->Update();
  }
  catch (itk::ExceptionObject & err)
  {
    std::cerr << "ERROR: ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
  }

  writer2->SetFileName(fileOutput2);
  writer2->SetInput(rescaler2->GetOutput());

  try
  {
    std::cout << "Writing image: " << fileOutput2 << std::endl;
    writer2->Update();
  }
  catch (itk::ExceptionObject & err)
  {
    std::cerr << "ERROR: ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
  }
  timer.Report();

  return EXIT_SUCCESS;
}
