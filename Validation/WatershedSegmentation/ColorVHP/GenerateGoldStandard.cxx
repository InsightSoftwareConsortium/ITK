#include "itkRawImageIO.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionIterator.h"
#include "param.h"
#include <list>
#include <string>
#include "math.h"
inline void die (char *c) { std::cerr << c << std::endl; ::exit(1); }

int main(int argc, char *argv[])
{
  typedef unsigned char ScalarType;
  typedef itk::Image<ScalarType, 3> ImageType;
  typedef itk::Image<int, 3> IntImageType;
  typedef itk::ImageRegionIterator<ImageType> IteratorType;
  typedef itk::ImageRegionIterator<IntImageType> IntIteratorType;
  typedef std::list<std::string> FileListType;
  typedef itk::ImageFileReader<ImageType> ReaderType;
  typedef itk::ImageFileWriter<ImageType> WriterType;
  typedef itk::RawImageIO<ScalarType, 3> ImageIOType;  
  
  FileListType file_list;
  std::string output_filename;
  int x, y, z;
  float threshold;
  int i_threshold;

  std::string s;

  // Read parameters
  VPF::ParameterFile pf(argv[1]);
  unsigned N = 0;
  while (VPF::set(s, pf["FILENAMES"][N]) != VPF::INVALID)
    {
      file_list.push_back(s);
      N++;
    }
  if (VPF::set(x, pf["DIMENSIONS"][0]) == VPF::INVALID)
    die("Cannot find all the DIMENSIONS parameters");
  if (VPF::set(y, pf["DIMENSIONS"][1]) == VPF::INVALID)
    die("Cannot find all the DIMENSIONS parameters");
  if (VPF::set(z, pf["DIMENSIONS"][2]) == VPF::INVALID)
    die("Cannot find all the DIMENSIONS parameters");
  if (VPF::set(output_filename, pf["OUTPUT_FILENAME"][0]) == VPF::INVALID)
    die("Cannot find the OUTPUT_FILENAME parameter");
  if (VPF::set(threshold, pf["THRESHOLD"][0]) == VPF::INVALID)
    die("Cannot find the THRESHOLD parameter");

  // Set up reader
  itk::RawImageIO<ScalarType, 3>::Pointer inputIo
    = itk::RawImageIO<ScalarType, 3>::New();
  itk::ImageFileReader<ImageType>::Pointer reader
    = itk::ImageFileReader<ImageType>::New();

  inputIo->SetFileDimensionality(3);  ///  <------IMPORTANT!!!!!!!
  inputIo->SetFileTypeToBinary();
  inputIo->SetByteOrderToBigEndian();
  inputIo->SetDimensions(0, x);
  inputIo->SetDimensions(1, y);
  inputIo->SetDimensions(2, z);
  
  reader->SetImageIO(inputIo);

  WriterType::Pointer writer = WriterType::New();
  ImageIOType::Pointer outputIo = ImageIOType::New();
  outputIo->SetFileDimensionality(3);
  outputIo->SetFileTypeToBinary();
  outputIo->SetByteOrderToLittleEndian();

  writer->SetImageIO(outputIo);

  // Set up counting image
  IntImageType::Pointer ev = IntImageType::New();
  ImageType::RegionType reg;
  ImageType::RegionType::IndexType idx;
  ImageType::RegionType::SizeType sz;
  idx[0] = idx[1] = idx[2] = 0;
  sz[0] = x;  sz[1] = y;  sz[2] = z;
  reg.SetIndex(idx);
  reg.SetSize(sz);

  ev->SetRegions(reg);
  ev->Allocate();
  for (IntIteratorType it(ev, ev->GetRequestedRegion()); !it.IsAtEnd(); ++it)
    {      it.Set(0);    }

  ImageType::Pointer out = ImageType::New();
  out->SetRegions(reg);
  out->Allocate();

  try { 
  // Counting step
  for (FileListType::iterator fit = file_list.begin();
       fit != file_list.end();
       ++fit)
    {
      IteratorType it1;
      IntIteratorType it2;
      reader->SetFileName((*fit).c_str());
      std::cout << "Counting " << (*fit).c_str() << std::endl;

      reader->Update();

      it1 = IteratorType(reader->GetOutput(), reader->GetOutput()->GetRequestedRegion());
      it2 = IntIteratorType(ev, ev->GetRequestedRegion());

      for (it1 = it1.Begin(), it2 = it2.Begin();
           !it1.IsAtEnd(); ++it1, ++it2)
        {
          if (it1.Get() != 0)
            { it2.Set(it2.Get()+1); }
        }
    }
  
  // Thresholding step
  i_threshold = (int) (::floor(threshold * (float) N));

  IntIteratorType it(ev, ev->GetRequestedRegion());
  IteratorType it_out(out, out->GetRequestedRegion());
  for (; !it.IsAtEnd(); ++it, ++it_out)
    {
      if (it.Get() >= i_threshold)
        {
          it_out.Set(1);
        }
      else
        {
          it_out.Set(0);
        }
    }


  writer->SetInput(out);
  writer->SetFileName(output_filename.c_str());
  writer->Write();
  }
  catch (itk::ExceptionObject &e)
    {
      std::cerr << e << std::endl;
      exit(1);
    }

  return 0;
}
