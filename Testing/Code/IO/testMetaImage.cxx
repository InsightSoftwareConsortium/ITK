#include <stdio.h>
#include <ctype.h>
#include <metaImage.h>

int testMetaImage(int , char * [])
  {

  MetaImage tIm(8, 8, 1, 2, MET_CHAR);
  MetaImage tImCopy(tIm);

  int i;
  for(i=0; i<64; i++)
    tIm.ElementData(i, i);

  for(i=0; i<64; i++)
    {
    if(i != tIm.ElementData(i))
      {
      std::cout << "Assigned Element Values Maintained: FAIL" << std::endl;
      return 1;
      }
    }

  tIm.Write("test.mha");
  tIm.PrintInfo();

  MetaImage tIm2("test.mha");

  std::cout << "Header size = " << tIm2.HeaderSize() << std::endl;
  tIm2.Modality(MET_MOD_CT);
  std::cout << "Modality = " << tIm2.Modality() << std::endl;
  std::cout << "DimSize = " << tIm2.DimSize() << std::endl;
  std::cout << "Quantity = " << tIm2.Quantity() << std::endl;
  std::cout << "SubQuantity = " << tIm2.SubQuantity() << std::endl;
  std::cout << "SubQuantity(0) = " << tIm2.SubQuantity(0) << std::endl;
  std::cout << "SequenceID = " << tIm2.SequenceID() << std::endl;
  std::cout << "ElementSizeValid = " << tIm2.ElementSizeValid() << std::endl;
  std::cout << "ElementSize = " << tIm2.ElementSize() << std::endl;
  std::cout << "ElementSize(0) = " << tIm2.ElementSize(0) << std::endl;
  std::cout << "ElementType = " << tIm2.ElementType() << std::endl;
  std::cout << "ElementNumberOfChannels = " << tIm2.ElementNumberOfChannels() << std::endl;
  std::cout << "ElementMinMaxValid = " << tIm2.ElementMinMaxValid() << std::endl;
  
  tIm2.ElementMinMaxRecalc();
 
  std::cout << "ElementMin = " << tIm2.ElementMin() << std::endl;
  std::cout << "ElementMax = " << tIm2.ElementMax() << std::endl;

  std::cout << "AutoFreeElementData = " << tIm2.AutoFreeElementData() << std::endl;
  std::cout << "ElementDataFileName = " << tIm2.ElementDataFileName() << std::endl;


  


  tIm2.PrintInfo();
  for(i=0; i<64; i++)
    {
    if(i != tIm.ElementData(i))
      {
      std::cout << "Read Element Values: FAIL" << std::endl;
      return 1;
      }
    }

  std::cout << "[DONE]" << std::endl;
  return 0;
  }
