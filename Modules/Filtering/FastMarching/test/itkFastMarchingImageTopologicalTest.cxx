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

#include "itkFastMarchingImageFilterBase.h"
#include "itkFastMarchingThresholdStoppingCriterion.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkLabelContourImageFilter.h"


template <unsigned int VDimension>
int FastMarchingImageFilter( unsigned int argc, char *argv[] )
{
  typedef float InternalPixelType;

  typedef itk::Image< InternalPixelType, VDimension > InternalImageType;

  typedef unsigned char OutputPixelType;

  typedef itk::Image<OutputPixelType, VDimension> OutputImageType;

  typedef itk::FastMarchingThresholdStoppingCriterion< InternalImageType, InternalImageType >
      CriterionType;

  typedef typename CriterionType::Pointer CriterionPointer;

  InternalPixelType stoppingValue = atof( argv[5] );

  CriterionPointer criterion = CriterionType::New();
  criterion->SetThreshold( stoppingValue );

  typedef itk::ImageFileReader< InternalImageType>  ReaderType;
  typedef typename ReaderType::Pointer              ReaderPointer;

  ReaderPointer reader = ReaderType::New();
  reader->SetFileName( argv[2] );
  try
    {
    reader->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::FastMarchingImageFilterBase< InternalImageType, InternalImageType > FastMarchingType;
  typedef typename FastMarchingType::Pointer FastMarchingPointer;

  FastMarchingPointer fastMarching = FastMarchingType::New();
  fastMarching->SetInput( reader->GetOutput() );
  fastMarching->SetStoppingCriterion( criterion );

  typedef typename FastMarchingType::LabelImageType LabelImageType;
  typedef typename LabelImageType::PixelType        LabelType;

  typedef itk::ImageFileReader<LabelImageType>    LabelImageReaderType;
  typedef typename LabelImageReaderType::Pointer  LabelImageReaderPointer;
  LabelImageReaderPointer labelImageReader = LabelImageReaderType::New();
  labelImageReader->SetFileName( argv[4] );

  try
    {
    labelImageReader->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    return EXIT_FAILURE;
    }

  LabelType label_zero = itk::NumericTraits<LabelType>::ZeroValue();

  typedef itk::LabelContourImageFilter<LabelImageType,
      LabelImageType> ContourFilterType;
  typename ContourFilterType::Pointer contour = ContourFilterType::New();
  contour->SetInput( labelImageReader->GetOutput() );
  contour->FullyConnectedOff();
  contour->SetBackgroundValue( label_zero );

  try
    {
    contour->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    return EXIT_FAILURE;
    }

  typedef typename FastMarchingType::NodePairType           NodePairType;
  typedef typename FastMarchingType::NodePairContainerType  NodePairContainerType;


  typename NodePairContainerType::Pointer AlivePoints = NodePairContainerType::New();
  typename NodePairContainerType::Pointer TrialPoints = NodePairContainerType::New();

  itk::ImageRegionIteratorWithIndex<LabelImageType> ItL(
        labelImageReader->GetOutput(),
        labelImageReader->GetOutput()->GetLargestPossibleRegion() );

  itk::ImageRegionIteratorWithIndex<LabelImageType> ItC(
        contour->GetOutput(),
        contour->GetOutput()->GetLargestPossibleRegion() );

  ItL.GoToBegin();
  ItC.GoToBegin();

  while( !ItL.IsAtEnd() )
    {
    if( ItC.Get() != label_zero )
      {
      TrialPoints->push_back( NodePairType( ItC.GetIndex(), 0. ) );
      }
    else
      {
      if( ItL.Get() != label_zero )
        {
        AlivePoints->push_back( NodePairType( ItL.GetIndex(), 0. ) );
        }
      }
    ++ItL;
    ++ItC;
    }

  fastMarching->SetTrialPoints(  TrialPoints  );
  fastMarching->SetAlivePoints(  AlivePoints  );

  fastMarching->SetTopologyCheck( FastMarchingType::Nothing );

  if( argc > 6 && atoi( argv[6] ) == 1 )
    {
    std::cout << "Strict." << std::endl;
    fastMarching->SetTopologyCheck( FastMarchingType::Strict );
    }
  if( argc > 6 && atoi( argv[6] ) == 2 )
    {
    std::cout << "No handles." << std::endl;
    fastMarching->SetTopologyCheck( FastMarchingType::NoHandles );
    }

  try
    {
    fastMarching->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::BinaryThresholdImageFilter
    <InternalImageType, OutputImageType> ThresholdingFilterType;
  typename ThresholdingFilterType::Pointer thresholder
    = ThresholdingFilterType::New();

  thresholder->SetLowerThreshold( 0.0 );
  thresholder->SetUpperThreshold( stoppingValue );
  thresholder->SetOutsideValue( 0 );
  thresholder->SetInsideValue( 1 );
  thresholder->SetInput( fastMarching->GetOutput() );

  try
    {
    thresholder->Update();
    }
   catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    return EXIT_FAILURE;
    }


  typedef itk::ImageFileWriter<OutputImageType> WriterType;
  typename WriterType::Pointer writer = WriterType::New();
  writer->SetInput( thresholder->GetOutput() );
  writer->SetFileName( argv[3] );

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    return EXIT_FAILURE;
    }

  if( argc > 7 )
    {
      {
      std::string filename = std::string( argv[7] ) +
        std::string( "LevelSet.nii.gz" );
      typedef itk::ImageFileWriter<InternalImageType> InternalWriterType;
      typename InternalWriterType::Pointer internal_writer = InternalWriterType::New();
      internal_writer->SetInput( fastMarching->GetOutput() );
      internal_writer->SetFileName( filename.c_str() );

      try
        {
        internal_writer->Update();
        }
      catch( itk::ExceptionObject & excep )
        {
        std::cerr << "Exception caught !" << std::endl;
        std::cerr << excep << std::endl;
        return EXIT_FAILURE;
        }
      }

      {
      std::string filename = std::string( argv[7] ) +
        std::string( "LabelMap.nii.gz" );
      typedef itk::ImageFileWriter< LabelImageType > LabelImageWriterType;
      typename LabelImageWriterType::Pointer mapWriter = LabelImageWriterType::New();
      mapWriter->SetInput( fastMarching->GetLabelImage() );
      mapWriter->SetFileName( filename.c_str() );

      try
        {
        mapWriter->Update();
        }
      catch( itk::ExceptionObject & excep )
        {
        std::cerr << "Exception caught !" << std::endl;
        std::cerr << excep << std::endl;
        return EXIT_FAILURE;
        }
      }
    }

  return EXIT_SUCCESS;
}

int itkFastMarchingImageTopologicalTest( int argc, char *argv[] )
{
  if( argc < 6 )
    {
    std::cerr << "Usage: " << argv[0] << " imageDimension";
    std::cerr << " speedImage outputImage seedImage ";
    std::cerr << " stoppingValue [checkTopology] [otherFilePrefix]"
      << std::endl;
    return EXIT_FAILURE;
    }

  switch( atoi( argv[1] ) )
   {
   case 2:
     return FastMarchingImageFilter<2>( argc, argv );
   case 3:
     return FastMarchingImageFilter<3>( argc, argv );
   default:
     std::cerr << "Unsupported dimension" << std::endl;
     return EXIT_FAILURE;
   }
}
