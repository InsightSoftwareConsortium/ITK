#include "itkRawImageIO.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionIterator.h"
#include "param.h"
#include <list>
#include <string>
#include <vector>
#include "math.h"
inline void die (char *c) { std::cerr << c << std::endl; ::exit(1); }

int main(int argc, char *argv[])
{
  typedef unsigned char ScalarType;
  typedef itk::Image<ScalarType, 3> ImageType;
  typedef itk::ImageRegionIterator<ImageType> IteratorType;
  typedef std::list<std::string> FileListType;
  typedef itk::ImageFileReader<ImageType> ReaderType;
  typedef itk::RawImageIO<ScalarType, 3> ImageIOType;  
  
  FileListType file_list;
  std::string gold_filename;
  int x, y, z;
  std::vector<float> accuracy_vector;
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
  if (VPF::set(gold_filename, pf["GOLD_FILENAME"][0]) == VPF::INVALID)
    die("Cannot find the GOLD_FILENAME parameter");

  // Set up reader
  itk::RawImageIO<ScalarType, 3>::Pointer inputIo
    = itk::RawImageIO<ScalarType, 3>::New();
  itk::ImageFileReader<ImageType>::Pointer reader
    = itk::ImageFileReader<ImageType>::New();
    itk::ImageFileReader<ImageType>::Pointer gs_reader
    = itk::ImageFileReader<ImageType>::New();

  inputIo->SetFileDimensionality(3);  ///  <------IMPORTANT!!!!!!!
  inputIo->SetFileTypeToBinary();
  inputIo->SetByteOrderToLittleEndian();
  inputIo->SetDimensions(0, x);
  inputIo->SetDimensions(1, y);
  inputIo->SetDimensions(2, z);

  gs_reader->SetImageIO(inputIo);
  reader->SetImageIO(inputIo);
  
  try { 
    gs_reader->SetFileName(gold_filename.c_str());
    gs_reader->Update();
    float accuracy;
    for (FileListType::iterator fit = file_list.begin();
         fit != file_list.end();
         ++fit)
      {
        IteratorType it1;
        IteratorType it2;
        reader->SetFileName((*fit).c_str());
        std::cout << "Analyzing " << (*fit).c_str() << std::endl;
        
        reader->Update();
        
        it1 = IteratorType(reader->GetOutput(), reader->GetOutput()->GetRequestedRegion());
        it2 = IteratorType(gs_reader->GetOutput(), reader->GetOutput()->GetRequestedRegion());
        unsigned correct = 0;
        unsigned total = 0;
        for (it1 = it1.Begin(), it2 = it2.Begin();
             !it1.IsAtEnd(); ++it1, ++it2)
          {
            if ( ((it1.Get()==0) && (it2.Get()==0))
                 || ((it1.Get()!=0) && (it2.Get()!=0)) )
              { correct++; total++; }
            else
              { total++; }
          }
        accuracy = (float) ( (float)correct/(float)total );
        std::cout << accuracy << std::endl;
        std::cout << std::endl;
        accuracy_vector.push_back(accuracy);
      }

    // expected value
    float expected_value = 0;
    for (std::vector<float>::iterator vit = accuracy_vector.begin();
         vit!=accuracy_vector.end(); ++vit)
      {
        std::cout << *vit << " ";
        expected_value += *vit;
      }
    expected_value = expected_value / (float) accuracy_vector.size();
    std::cout << std::endl;
    std::cout << "expected_value = " << expected_value <<std::endl;

    // variance
    float variance, std_deviation;
    variance = 0;
    std_deviation = 0;
    for (std::vector<float>::iterator vit = accuracy_vector.begin();
         vit!=accuracy_vector.end(); ++vit)
      {
        std::cout << ::vnl_math_sqr(*vit - expected_value) << " ";
        variance += ::vnl_math_sqr(*vit - expected_value);
      }
    std::cout << std::endl;
    variance = variance / (float) accuracy_vector.size();
    std::cout << "variance = " << variance << std::endl;
    std_deviation = ::vnl_math_sqrt(variance);
    std::cout << "std_deviation = " << std_deviation << std::endl;
  }
  catch (itk::ExceptionObject &e)
    {
      std::cerr << e << std::endl;
      exit(1);
    }

  return 0;
}
