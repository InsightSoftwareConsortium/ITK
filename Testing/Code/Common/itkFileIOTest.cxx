#include <itkImage.h>
#include "itkFileIOMetaImage.h"
#include "itkWriteMetaImage.h"
#include "itkFilterFileIOToImage.h"

using namespace itk;

void main ()
{
	typedef Image<unsigned short, 3> myImageType;
	typedef WriteMetaImage<myImageType> myWriterType;
	typedef FilterFileIOToImage<myImageType> myFilterType;

	myWriterType::Pointer writer = myWriterType::New();
	myFilterType::Pointer filter = new myFilterType("D:/research/Angio002Series005.mhd");

	filter->Update();
	filter->Print(std::cout);
	writer->SetFileName("D:/research/TestOutput.mhd");
	writer->SetInput(filter->GetOutput());
	writer->Print(std::cout);
	writer->Write();
}