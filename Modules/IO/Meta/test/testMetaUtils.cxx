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

#include <fstream>
#include <cstdlib>


#include <metaUtils.h>
#include "itksys/SystemTools.hxx"
#include "itkMacro.h"

int testMetaUtils(int argc, char * argv[])
{
  if (argc > 1)
    {
    itksys::SystemTools::ChangeDirectory(argv[1]);
    }

  if(MET_SystemByteOrderMSB())
    {
    std::cout << "MET_SYSTEM_BYTE_ORDER_MSB = TRUE" << std::endl;
    }
  else
    {
    std::cout << "MET_SYSTEM_BYTE_ORDER_MSB = FALSE" << std::endl;
    }
  unsigned short x = 256;
  std::cout << "MET_ByteSwapShort: ";
  if(MET_ByteOrderSwapShort(x) != 1)
    std::cout << "FAILED: 256->" << MET_ByteOrderSwapShort(x) << std::endl;
  else
    std::cout << "PASSED" << std::endl;
  x = 1;
  std::cout << "MET_ByteSwapShort: ";
  if(MET_ByteOrderSwapShort(x) != 256)
    std::cout << "FAILED: 1->" << MET_ByteOrderSwapShort(x) << std::endl;
  else
    std::cout << "PASSED" << std::endl;

  MET_ValueEnumType metType = MET_USHORT;
  MET_ValueEnumType tmpMetType = MET_USHORT;
  char tmpString[80];
  sprintf(tmpString, "MET_USHORT");
  std::cout << "MET_StringToType: ";
  MET_StringToType(tmpString, &tmpMetType);
  if(tmpMetType != metType)
    std::cout << "FAILED" << std::endl;
  else
    std::cout << "PASSED" << std::endl;

  std::cout << "MET_TypeToString: ";
  MET_TypeToString(MET_USHORT, tmpString);
  if(strcmp(tmpString, "MET_USHORT"))
    std::cout << "FAILED" << std::endl;
  else
    std::cout << "PASSED" << std::endl;

  int n;
  std::cout << "MET_SizeOfType: ";
  MET_SizeOfType(MET_USHORT, &n);
  if(2 != n)
    std::cout << "FAILED" << std::endl;
  else
    std::cout << "PASSED" << std::endl;

  char **wordArray;
  MET_StringToWordArray("This is a test", &n, &wordArray);
  std::cout << "MET_StringToWordArray: N: ";
  if(n != 4)
    std::cout << "FAILED" << std::endl;
  else
    std::cout << "PASSED" << std::endl;
  std::cout << "MET_StringToWordArray: 1: ";
  if(strcmp(wordArray[0], "This"))
    std::cout << "FAILED" << std::endl;
  else
    std::cout << "PASSED" << std::endl;
  std::cout << "MET_StringToWordArray: 2: ";
  if(strcmp(wordArray[1], "is"))
    std::cout << "FAILED" << std::endl;
  else
    std::cout << "PASSED" << std::endl;
  std::cout << "MET_StringToWordArray: 3: ";
  if(strcmp(wordArray[2], "a"))
    std::cout << "FAILED" << std::endl;
  else
    std::cout << "PASSED" << std::endl;
  std::cout << "MET_StringToWordArray: 4: ";
  if(strcmp(wordArray[3], "test"))
    std::cout << "FAILED" << std::endl;
  else
    std::cout << "PASSED" << std::endl;

  int i;
  for(i=0;i<n;i++)
    {
      delete[] wordArray[i];
    }
  delete[] wordArray;

  char fName[80];
  sprintf(fName, "this/is/a/test.com");

  std::cout << "MET_GetFilePathTest: ";
  MET_GetFilePath(fName, tmpString);
  if(strcmp(tmpString, "this/is/a/"))
    std::cout << "FAILED" << std::endl;
  else
    std::cout << "PASSED" << std::endl;

  int tmpI;
  std::cout << "MET_GetFileSuffixPtr: ";
  MET_GetFileSuffixPtr(fName, &tmpI);
  if(fName[tmpI] != 'c')
    {
    std::cout << "FAILED" << std::endl;
    std::cout << &(fName[tmpI]) << std::endl;
    }
  else
    std::cout << "PASSED" << std::endl;

  std::cout << "MET_SetFileSuffix: ";
  MET_SetFileSuffix(fName, ".net");
  if(strcmp(fName, "this/is/a/test.net"))
    std::cout << "FAILED" << std::endl;
  else
    std::cout << "PASSED" << std::endl;

  std::ofstream fout("testMetaFileUtils.txt", std::ios::out);

  MET_FieldRecordType * mF;
  std::vector<MET_FieldRecordType *> mFields;

  mF = new MET_FieldRecordType;
  MET_InitWriteField(mF, "NDims", MET_UCHAR, 2);
  mFields.push_back(mF);

  float vTmp[10];
  vTmp[0] = 0.5;
  vTmp[1] = 0.75;
  mF = new MET_FieldRecordType;
  MET_InitWriteField(mF, "ElementSize", MET_FLOAT_ARRAY, 2, vTmp);
  mFields.push_back(mF);

  char s[80];
  strcpy(s, "X-AXIS Y-AXIS");
  mF = new MET_FieldRecordType;
  MET_InitWriteField(mF, "DirNames", MET_STRING, strlen(s), s);
  mFields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitWriteField(mF, "END", MET_NONE);
  mF->terminateRead = true;
  mFields.push_back(mF);

  MET_Write(fout, &mFields);

  MET_WriteFieldToFile(fout, "Beyond", MET_STRING, 4, "True");
  MET_WriteFieldToFile(fout, "Extra", MET_USHORT, 1);

  fout.flush();

  std::vector<MET_FieldRecordType *>::iterator fieldIter;
  for(fieldIter=mFields.begin(); fieldIter != mFields.end(); ++fieldIter)
    {
    delete *fieldIter;
    }
  mFields.clear();

  //
  //
  //
  std::ifstream fin;
  fin.open("testMetaFileUtils.txt", std::ios::in);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "NDims", MET_INT);
  mFields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "ElementSize", MET_FLOAT_ARRAY, true, 0);
  mFields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "DirNames", MET_STRING);
  mFields.push_back(mF);

  mF = new MET_FieldRecordType;
  MET_InitReadField(mF, "END", MET_NONE);
  mF->terminateRead = true;
  mFields.push_back(mF);

  std::cout << "MET_Read: ";
  if(!MET_Read(fin, &mFields))
    std::cout << "FAILED" << std::endl;
  else
    std::cout << "PASSED" << std::endl;

  fieldIter = mFields.begin();
  int nDims;
  if((*fieldIter)->defined)
    {
    nDims = (int)((*fieldIter)->value[0]);
    if(nDims != 2)
      std::cout << "nDims not equal to 2" << std::endl;
    else
      std::cout << "nDims: Passed" << std::endl;
    }
  else
    std::cout << "nDims not defined" << std::endl;

  double eSize[2];
  ++fieldIter;
  if((*fieldIter)->defined)
    {
    eSize[0] = (*fieldIter)->value[0];
    eSize[1] = (*fieldIter)->value[1];
    if(eSize[0] != 0.5 || eSize[1] != 0.75)
      std::cout << "ElementSizes are wrong: " << eSize[0] << ", " << eSize[1] << std::endl;
    else
      std::cout << "ElementSizes: Passed" << std::endl;
    }
  else
    std::cout << "ElementSize not defined" << std::endl;

  int nNames=0;
  char **names=ITK_NULLPTR;
  ++fieldIter;
  if((*fieldIter)->defined)
    {
    MET_StringToWordArray((char *)((*fieldIter)->value), &nNames, &names);
    if(nNames != 2)
      std::cout << "nNames wrong : " << nNames << std::endl;
    else
      if(strcmp(names[0], "X-AXIS") || strcmp(names[1], "Y-AXIS"))
        std::cout << "names wrong : _" << names[0] << "_, _" << names[1] << "_" << std::endl;
      else
        std::cout << "Names: Passed" << std::endl;
    }
  else
    std::cout << "DirNames not defined" << std::endl;

  for(i=0;i<nNames;i++)
    {
      delete[] names[i];
    }
  delete[] names;

  for(fieldIter=mFields.begin(); fieldIter != mFields.end(); ++fieldIter)
    {
    delete *fieldIter;
    }

  std::cout << "[DONE]" << std::endl;
  return EXIT_SUCCESS;
}
