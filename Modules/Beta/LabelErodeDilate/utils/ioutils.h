#ifndef __ioutils_h_
#define __ioutils_h_

#include <itkImageFileWriter.h>
#include <itkImageFileReader.h>
#include <itkShiftScaleImageFilter.h>
#include <itkStatisticsImageFilter.h>
#include <itkNumericTraits.h>
#include <itkOrientImageFilter.h>
#include <itkSpatialOrientation.h>

int
readImageInfo(std::string filename, itk::ImageIOBase::IOComponentType * ComponentType, int * dim)
{
  itk::ImageIOBase::Pointer imageIO =
    itk::ImageIOFactory::CreateImageIO(filename.c_str(), itk::ImageIOFactory::ReadMode);
  if (imageIO.IsNull())
    return 0;


  imageIO->SetFileName(filename.c_str());
  imageIO->ReadImageInformation();

  *ComponentType = imageIO->GetComponentType();
  *dim = imageIO->GetNumberOfDimensions();
  return (1);
}


template <class TImage>
void
writeIm(typename TImage::Pointer Im, std::string filename)
{
  using WriterType = typename itk::ImageFileWriter<TImage>;
  typename WriterType::Pointer writer = WriterType::New();
  writer->SetInput(Im);
  writer->SetFileName(filename.c_str());
  writer->Update();
}

template <class TImage, class PixType>
void
writeImScale(typename TImage::Pointer Im, std::string filename)
{
  const int dim = TImage::ImageDimension;
  using OutType = typename itk::Image<PixType, dim>;
  using WriterType = typename itk::ImageFileWriter<OutType>;
  using StatsType = typename itk::StatisticsImageFilter<TImage>;
  using FitType = typename itk::ShiftScaleImageFilter<TImage, OutType>;

  typename StatsType::Pointer stats = StatsType::New();
  typename FitType::Pointer   fitter = FitType::New();

  stats->SetInput(Im);
  stats->Update();
  float range = itk::NumericTraits<PixType>::max() - itk::NumericTraits<PixType>::NonpositiveMin() + 1;
  float mx = stats->GetMaximum();
  float mn = stats->GetMinimum();
  float scale = range / (mx - mn);
  float shift = -mn;

  fitter->SetInput(stats->GetOutput());
  fitter->SetScale(scale);
  fitter->SetShift(shift);

  typename WriterType::Pointer writer = WriterType::New();
  writer->SetInput(fitter->GetOutput());
  writer->SetFileName(filename.c_str());
  writer->Update();
}

template <class TImage>
typename TImage::Pointer
readIm(std::string filename)
{
  using ReaderType = typename itk::ImageFileReader<TImage>;
  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(filename.c_str());
  typename TImage::Pointer result = reader->GetOutput();
  try
  {
    result->Update();
  }
  catch (itk::ExceptionObject & ex)
  {
    std::cout << ex << std::endl;
    std::cout << filename << std::endl;
    return 0;
  }
  result->DisconnectPipeline();
  return (result);
}

template <class TImage>
typename TImage::Pointer
readImOriented(std::string filename, itk::SpatialOrientation::ValidCoordinateOrientationFlags direction)
{
  using ReaderType = typename itk::ImageFileReader<TImage>;
  typename ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(filename.c_str());
  using ReorientType = typename itk::OrientImageFilter<TImage, TImage>;
  typename ReorientType::Pointer reorient = ReorientType::New();
  reorient->SetInput(reader->GetOutput());
  reorient->UseImageDirectionOn();
  reorient->SetDesiredCoordinateOrientation(direction);
  typename TImage::Pointer result = reorient->GetOutput();

  try
  {
    result->Update();
  }
  catch (itk::ExceptionObject & ex)
  {
    std::cout << ex << std::endl;
    std::cout << filename << std::endl;
    return 0;
  }
  result->DisconnectPipeline();
  return (result);
}


#endif
