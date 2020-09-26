
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkAdaptiveNonLocalMeansDenoisingImageFilter.h"

// template <typename TFilter>
// class CommandProgressUpdate : public itk::Command
// {
// public:
//   using Self = CommandProgressUpdate<TFilter>;
//   using Superclass = itk::Command;
//   using Pointer = itk::SmartPointer<CommandProgressUpdate<TFilter> >;
//   itkNewMacro( CommandProgressUpdate );
// protected:

//   CommandProgressUpdate()  = default;

//   using FilterType = TFilter;

//   unsigned int m_CurrentProgress{ 0 };

// public:

//   void Execute(itk::Object *caller, const itk::EventObject & event) override
//     {
//     auto *po = dynamic_cast<itk::ProcessObject *>( caller );
//     if (! po) return;
//     if( typeid( event ) == typeid ( itk::ProgressEvent )  )
//       {
//       if( this->m_CurrentProgress < 99 )
//         {
//         this->m_CurrentProgress++;
//         if( this->m_CurrentProgress % 10 == 0 )
//           {
//           std::cout << this->m_CurrentProgress << std::flush;
//           }
//         else
//           {
//           std::cout << "*" << std::flush;
//           }
//         }
//       }
//     }

//   void Execute(const itk::Object * object, const itk::EventObject & event) override
//     {
//     auto *po = dynamic_cast<itk::ProcessObject *>(
//       const_cast<itk::Object *>( object ) );
//     if (! po) return;

//     if( typeid( event ) == typeid ( itk::ProgressEvent )  )
//       {
//       if( this->m_CurrentProgress < 99 )
//         {
//         this->m_CurrentProgress++;
//         if( this->m_CurrentProgress % 10 == 0 )
//           {
//           std::cout << this->m_CurrentProgress << std::flush;
//           }
//         else
//           {
//           std::cout << "*" << std::flush;
//           }
//         }
//       }
//     }
// };

int
itkAdaptiveNonLocalMeansDenoisingImageFilterTest(int argc, char * argv[])
{
  if (argc < 2)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << argv[0];
    std::cerr << "  inputImage";
    std::cerr << "  outputImage";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;
  using PixelType = float;
  using ImageType = itk::Image<PixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<ImageType>;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(argv[1]);

  using DenoiserType = itk::AdaptiveNonLocalMeansDenoisingImageFilter<ImageType, ImageType>;
  DenoiserType::Pointer filter = DenoiserType::New();

  // ITK_EXERCISE_BASIC_OBJECT_METHODS( filter, AdaptiveNonLocalMeansDenoisingImageFilter, ImageToImageFilter );

  filter->SetInput(reader->GetOutput());
  filter->SetUseRicianNoiseModel(false);
  // ITK_TEST_SET_GET_VALUE( false, filter->GetUseRicianNoiseModel() );

  DenoiserType::NeighborhoodRadiusType neighborhoodPatchRadius;
  DenoiserType::NeighborhoodRadiusType neighborhoodSearchRadius;

  neighborhoodPatchRadius.Fill(1);
  neighborhoodSearchRadius.Fill(2);

  filter->SetNeighborhoodSearchRadius(neighborhoodSearchRadius);
  filter->SetNeighborhoodPatchRadius(neighborhoodPatchRadius);

  DenoiserType::NeighborhoodRadiusType neighborhoodRadiusForLocalMeanAndVariance;
  neighborhoodRadiusForLocalMeanAndVariance.Fill(1);

  filter->SetNeighborhoodRadiusForLocalMeanAndVariance(neighborhoodRadiusForLocalMeanAndVariance);

  filter->SetEpsilon(0.00001);
  // ITK_TEST_SET_GET_VALUE( 0.00001, filter->GetEpsilon() );

  filter->SetMeanThreshold(0.95);
  // ITK_TEST_SET_GET_VALUE( 0.95, filter->GetMeanThreshold() );

  filter->SetVarianceThreshold(0.5);
  // ITK_TEST_SET_GET_VALUE( 0.5, filter->GetVarianceThreshold() );

  filter->SetSmoothingFactor(1.0);
  // ITK_TEST_SET_GET_VALUE( 1.0, filter->GetSmoothingFactor() );

  filter->SetSmoothingVariance(2.0);
  // ITK_TEST_SET_GET_VALUE( 2.0, filter->GetSmoothingVariance() );

  // using CommandType = CommandProgressUpdate<DenoiserType>;
  // CommandType::Pointer observer = CommandType::New();
  // filter->AddObserver( itk::ProgressEvent(), observer );

  // ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());

  using WriterType = itk::ImageFileWriter<ImageType>;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName(argv[2]);
  writer->SetInput(filter->GetOutput());

  // ITK_TRY_EXPECT_NO_EXCEPTION( writer->Update() );

  return EXIT_SUCCESS;
}
