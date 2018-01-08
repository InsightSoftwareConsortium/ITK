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

/**
 * Test program for JointHistogramMutualInformationImageToImageMetricv4 and
 * GradientDescentOptimizerv4 classes.
 *
 * Perform a registration using user-supplied images.
 * No numerical verification is performed. Test passes as long
 * as no exception occurs.
 */
#include "itkJointHistogramMutualInformationImageToImageMetricv4.h"
#include "itkGradientDescentOptimizerv4.h"
#include "itkRegistrationParameterScalesFromPhysicalShift.h"

#include "itkGaussianSmoothingOnUpdateDisplacementFieldTransform.h"

#include "itkCastImageFilter.h"

#include "itkCommand.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include <iomanip>

namespace{

template < typename TOptimizer, typename TMIMetric >
/** \class JointPDFStatus
 * \brief Save the JointPDF from the metric to an image file and check to make sure
 * they are normalized properly. */
class JointPDFStatus: public itk::Command
{
public:
  typedef JointPDFStatus            Self;
  typedef itk::Command              Superclass;
  typedef itk::SmartPointer< Self > Pointer;

  itkSimpleNewMacro( Self ); //Clone, and CreateAnother are not needed here.

  typedef TOptimizer OptimizerType;
  typedef TMIMetric  MIMetricType;

  void SetMIMetric( const MIMetricType * metric )
    {
    this->m_MIMetric = metric;
    }

  void SetOutputFileNameBase( const char * filename )
    {
    this->m_OutputFileNameBase = filename;
    }

  void Execute(itk::Object *caller, const itk::EventObject & event) override
    {
    Execute( (const itk::Object *)caller, event);
    }

  void Execute(const itk::Object * object, const itk::EventObject & event) override
    {
    const OptimizerType * optimizer =
      dynamic_cast< const OptimizerType * >( object );
    if(optimizer == nullptr)
      {
      return;
      }
    if( !(itk::IterationEvent().CheckEvent( &event )) )
      {
      return;
      }
    std::cout << "Current optimizer iteration: " << optimizer->GetCurrentIteration() << "\n";
    std::cout << "Current optimizer value:     " << optimizer->GetCurrentMetricValue() << "\n";

    std::string  ext = itksys::SystemTools::GetFilenameExtension( this->m_OutputFileNameBase );
    std::string name = itksys::SystemTools::GetFilenameWithoutExtension( this->m_OutputFileNameBase );
    std::string path = itksys::SystemTools::GetFilenamePath( this->m_OutputFileNameBase );
    std::ostringstream ostrm;
    ostrm << name << "_jointpdf_" << this->m_Count << ext;
    std::cout << "Writing joint pdf to:        " << ostrm.str() << std::endl;
    ostrm.str( "" );
    ostrm << path << "/" << name << "_jointpdf_" << this->m_Count << ext;
    this->m_Writer->SetFileName( ostrm.str() );

    typedef typename MIMetricType::JointPDFType JointPDFType;
    const JointPDFType * jointPDF = this->m_MIMetric->GetJointPDF();
    this->m_Writer->SetInput( jointPDF );
    this->m_Writer->Update();

    // Check for correct normalization.
    typedef itk::ImageRegionConstIterator< JointPDFType > IteratorType;
    IteratorType it( jointPDF, jointPDF->GetBufferedRegion() );
    double sum = 0.0;
    for( it.GoToBegin(); ! it.IsAtEnd(); ++it )
      {
      sum += it.Get();
      }
    std::cout << "The PDF sum is               " << std::setprecision( 20 ) << sum << std::endl;

    ++this->m_Count;
    }

protected:
  JointPDFStatus() :
    m_MIMetric( nullptr ),
    m_Count( 0 )
    {
    this->m_Writer = WriterType::New();
    }

private:
  const MIMetricType * m_MIMetric;

  unsigned int m_Count;
  std::string  m_OutputFileNameBase;

  typedef typename itk::ImageFileWriter< typename MIMetricType::JointPDFType > WriterType;
  typename WriterType::Pointer m_Writer;
};
}

int itkJointHistogramMutualInformationImageToImageRegistrationTest(int argc, char *argv[])
{

  if( argc < 4 )
    {
    std::cerr << "Missing Parameters " << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << " fixedImageFile movingImageFile ";
    std::cerr << " outputImageFile ";
    std::cerr << " [numberOfIterations numberOfDisplacementIterations] ";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << argc << std::endl;
  unsigned int numberOfIterations = 10;
  unsigned int numberOfDisplacementIterations = 10;
  if( argc >= 5 )
    {
    numberOfIterations = atoi( argv[4] );
    }
  if( argc >= 6 )
    {
    numberOfDisplacementIterations = atoi( argv[5] );
    }
  std::cout << " iterations "<< numberOfIterations
    << " displacementIterations " << numberOfDisplacementIterations << std::endl;

  const unsigned int Dimension = 2;
  typedef double PixelType; //I assume png is unsigned short

  typedef itk::Image< PixelType, Dimension >  FixedImageType;
  typedef itk::Image< PixelType, Dimension >  MovingImageType;

  typedef itk::ImageFileReader< FixedImageType  > FixedImageReaderType;
  typedef itk::ImageFileReader< MovingImageType > MovingImageReaderType;

  FixedImageReaderType::Pointer fixedImageReader   = FixedImageReaderType::New();
  MovingImageReaderType::Pointer movingImageReader = MovingImageReaderType::New();

  fixedImageReader->SetFileName( argv[1] );
  movingImageReader->SetFileName( argv[2] );

  //get the images
  fixedImageReader->Update();
  FixedImageType::Pointer  fixedImage = fixedImageReader->GetOutput();
  movingImageReader->Update();
  MovingImageType::Pointer movingImage = movingImageReader->GetOutput();

  /** define a resample filter that will ultimately be used to deform the image */
  typedef itk::ResampleImageFilter<
                            MovingImageType,
                            FixedImageType >    ResampleFilterType;
  ResampleFilterType::Pointer resample = ResampleFilterType::New();


  /** create a composite transform holder for other transforms  */
  typedef itk::CompositeTransform<double, Dimension>    CompositeType;

  CompositeType::Pointer compositeTransform = CompositeType::New();

  //create an affine transform
  typedef itk::AffineTransform<double, Dimension>
                                                    AffineTransformType;
  AffineTransformType::Pointer affineTransform = AffineTransformType::New();
  affineTransform->SetIdentity();
  std::cout <<" affineTransform params prior to optimization " << affineTransform->GetParameters() << std::endl;

  typedef itk::GaussianSmoothingOnUpdateDisplacementFieldTransform< double, Dimension> DisplacementTransformType;
  DisplacementTransformType::Pointer displacementTransform = DisplacementTransformType::New();

  typedef DisplacementTransformType::DisplacementFieldType DisplacementFieldType;
  DisplacementFieldType::Pointer field = DisplacementFieldType::New();

  // set the field to be the same as the fixed image region, which will
  // act by default as the virtual domain in this example.
  field->SetRegions( fixedImage->GetLargestPossibleRegion() );
  //make sure the field has the same spatial information as the image
  field->CopyInformation( fixedImage );
  std::cout << "fixedImage->GetLargestPossibleRegion(): "
            << fixedImage->GetLargestPossibleRegion() << std::endl;
  field->Allocate();
  // Fill it with 0's
  DisplacementTransformType::OutputVectorType zeroVector;
  zeroVector.Fill( 0 );
  field->FillBuffer( zeroVector );
  // Assign to transform
  displacementTransform->SetDisplacementField( field );
  displacementTransform->SetGaussianSmoothingVarianceForTheUpdateField( 5 );
  displacementTransform->SetGaussianSmoothingVarianceForTheTotalField( 6 );

  //identity transform for fixed image
  typedef itk::IdentityTransform<double, Dimension> IdentityTransformType;
  IdentityTransformType::Pointer identityTransform = IdentityTransformType::New();
  identityTransform->SetIdentity();

  // The metric
  typedef itk::JointHistogramMutualInformationImageToImageMetricv4 < FixedImageType, MovingImageType >  MetricType;
  typedef MetricType::FixedSampledPointSetType                                                              PointSetType;
  MetricType::Pointer metric = MetricType::New();
  metric->SetNumberOfHistogramBins(20);

  typedef PointSetType::PointType     PointType;
  PointSetType::Pointer               pset(PointSetType::New());
  unsigned long ind=0,ct=0;
  itk::ImageRegionIteratorWithIndex<FixedImageType> It(fixedImage, fixedImage->GetLargestPossibleRegion() );
  for( It.GoToBegin(); !It.IsAtEnd(); ++It )
    {
    // take every N^th point
    if ( ct % 20 == 0  ) // about a factor of 5 speed-up over dense
      {
        PointType pt;
        fixedImage->TransformIndexToPhysicalPoint( It.GetIndex(), pt);
        pset->SetPoint(ind, pt);
        ind++;
      }
      ct++;
    }
    // brief profiling notes on mutual information affine registration macbook air , mi using every 20th point for sparse
    //  1 thread dense = 10 sec
    //  2 thread dense = 7.5  sec
    //  1 thread sparse = 2.2 sec
    //  2 thread sparse = 1.8 sec
    // this uses only 1500 points so it's probably not a great multi-thread test for the sparse case
  std::cout << "Setting point set with " << ind << " points of " << fixedImage->GetLargestPossibleRegion().GetNumberOfPixels() << " total " << std::endl;
  metric->SetFixedSampledPointSet( pset );
  metric->SetUseFixedSampledPointSet( true );
  std::cout << "Testing metric with point set..." << std::endl;

  // Assign images and transforms.
  // By not setting a virtual domain image or virtual domain settings,
  // the metric will use the fixed image for the virtual domain.
  metric->SetFixedImage( fixedImage );
  metric->SetMovingImage( movingImage );
  metric->SetFixedTransform( identityTransform );
  metric->SetMovingTransform( affineTransform );
  const bool gaussian = false;
  metric->SetUseMovingImageGradientFilter( gaussian );
  metric->SetUseFixedImageGradientFilter( gaussian );
  metric->Initialize();

  typedef itk::RegistrationParameterScalesFromPhysicalShift< MetricType > RegistrationParameterScalesFromShiftType;
  RegistrationParameterScalesFromShiftType::Pointer shiftScaleEstimator = RegistrationParameterScalesFromShiftType::New();
  shiftScaleEstimator->SetMetric(metric);

  std::cout << "First do an affine registration " << std::endl;
  typedef itk::GradientDescentOptimizerv4  OptimizerType;
  OptimizerType::Pointer  optimizer = OptimizerType::New();
  typedef JointPDFStatus< OptimizerType, MetricType > JointPDFStatusType;
  JointPDFStatusType::Pointer jointPDFStatus = JointPDFStatusType::New();
  jointPDFStatus->SetOutputFileNameBase( argv[3] );
  jointPDFStatus->SetMIMetric( metric );
  //optimizer->AddObserver( itk::IterationEvent(), jointPDFStatus );
  optimizer->SetMetric( metric );
  optimizer->SetNumberOfIterations( numberOfIterations );
  optimizer->SetScalesEstimator( shiftScaleEstimator );
  optimizer->StartOptimization();

  std::cout << "Number of threads: metric: " << metric->GetNumberOfThreadsUsed() << " optimizer: " << optimizer->GetNumberOfThreads() << std::endl;
  std::cout << "GetNumberOfSkippedFixedSampledPoints: " << metric->GetNumberOfSkippedFixedSampledPoints() << std::endl;

  std::cout << "Follow affine with deformable registration " << std::endl;
  // now add the displacement field to the composite transform
  compositeTransform->AddTransform( affineTransform );
  compositeTransform->AddTransform( displacementTransform );
  compositeTransform->SetAllTransformsToOptimizeOn(); //Set back to optimize all.
  compositeTransform->SetOnlyMostRecentTransformToOptimizeOn(); //set to optimize the displacement field
  metric->SetMovingTransform( compositeTransform );
  metric->SetUseFixedSampledPointSet( false );
  metric->Initialize();

  // Optimizer
  RegistrationParameterScalesFromShiftType::ScalesType
    displacementScales( displacementTransform->GetNumberOfLocalParameters() );
  displacementScales.Fill(1);
  if( 0 )
    {
    optimizer->SetScales( displacementScales );
    }
  else
    {
    optimizer->SetScalesEstimator( shiftScaleEstimator );
    }
  optimizer->SetMetric( metric );
  optimizer->SetNumberOfIterations( numberOfDisplacementIterations );
  try
    {
    optimizer->StartOptimization();
    }
  catch( itk::ExceptionObject & e )
    {
    std::cout << "Exception thrown ! " << std::endl;
    std::cout << "An error occurred during deformation Optimization:" << std::endl;
    std::cout << e.GetLocation() << std::endl;
    std::cout << e.GetDescription() << std::endl;
    std::cout << e.what()    << std::endl;
    std::cout << "Test FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << "...finished. " << std::endl;

  std::cout << "GetNumberOfSkippedFixedSampledPoints: " << metric->GetNumberOfSkippedFixedSampledPoints() << std::endl;


  //warp the image with the displacement field
  resample->SetTransform( compositeTransform );
  resample->SetInput( movingImageReader->GetOutput() );
  resample->SetSize(    fixedImage->GetLargestPossibleRegion().GetSize() );
  resample->SetOutputOrigin(  fixedImage->GetOrigin() );
  resample->SetOutputSpacing( fixedImage->GetSpacing() );
  resample->SetOutputDirection( fixedImage->GetDirection() );
  resample->SetDefaultPixelValue( 0 );
  resample->Update();

  //write out the displacement field
  typedef itk::ImageFileWriter< DisplacementFieldType >  DisplacementWriterType;
  DisplacementWriterType::Pointer      displacementwriter =  DisplacementWriterType::New();
  std::string outfilename( argv[3] );
  std::string  ext = itksys::SystemTools::GetFilenameExtension( outfilename );
  std::string name = itksys::SystemTools::GetFilenameWithoutExtension( outfilename );
  std::string path = itksys::SystemTools::GetFilenamePath( outfilename );
  std::string defout = path + std::string( "/" ) + name + std::string("_def") + ext;
  displacementwriter->SetFileName( defout.c_str() );
  displacementwriter->SetInput( displacementTransform->GetDisplacementField() );
  displacementwriter->Update();

  //write the warped image into a file
  typedef double                                    OutputPixelType;
  typedef itk::Image< OutputPixelType, Dimension >  OutputImageType;
  typedef itk::CastImageFilter<
                        MovingImageType,
                        OutputImageType >           CastFilterType;
  typedef itk::ImageFileWriter< OutputImageType >   WriterType;
  WriterType::Pointer      writer =  WriterType::New();
  CastFilterType::Pointer  caster =  CastFilterType::New();
  writer->SetFileName( argv[3] );
  caster->SetInput( resample->GetOutput() );
  writer->SetInput( caster->GetOutput() );
  writer->Update();

  std::cout << "After optimization affine params are: " <<  affineTransform->GetParameters() << std::endl;
  std::cout << "Test PASSED." << std::endl;
  return EXIT_SUCCESS;

}
