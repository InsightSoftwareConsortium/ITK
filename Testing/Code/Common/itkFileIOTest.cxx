#include <itkImage.h>
#include <itkFileIOMetaImage.h>
#include <itkWriteMetaImage.h>
#include <itkFilterFileIOToImage.h>

using namespace itk;
using namespace std;

int main ()
{
  typedef Image<unsigned short, 3> myImageType;
  typedef WriteMetaImage<myImageType> myWriterType;
  typedef FilterFileIOToImage<myImageType> myFilterType;

  ObjectFactoryBase::RegisterFactory(new FileIOMetaImageFactory);
  myWriterType::Pointer writer = myWriterType::New();
  myFilterType::Pointer filter = myFilterType::New();

	filter->SetFileToLoad("D:/research/Angio002Series005.mhd");
  filter->Update();
// Why does Print() cause an infinite loop?
//  filter->Print(std::cout);
  writer->SetFileName("D:/research/TestOutput.mhd");
  writer->SetInput(filter->GetOutput());
//  writer->Print(std::cout);
  writer->Write();

  return 0;
}
