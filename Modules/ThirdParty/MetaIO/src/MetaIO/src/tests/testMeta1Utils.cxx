#include <iostream>

#include <metaUtils.h>

int
main(int, char *[])
{
  int exitCode = EXIT_SUCCESS;
  if (MET_SystemByteOrderMSB())
  {
    std::cout << "MET_SYSTEM_BYTE_ORDER_MSB = TRUE" << std::endl;
  }
  else
  {
    std::cout << "MET_SYSTEM_BYTE_ORDER_MSB = FALSE" << std::endl;
  }
  unsigned short x = 256;
  std::cout << "MET_ByteSwapShort: ";
  if (MET_ByteOrderSwapShort(x) != 1)
  {
    std::cout << "FAILED: 256->" << MET_ByteOrderSwapShort(x) << std::endl;
    exitCode = EXIT_FAILURE;
  }
  else {
    std::cout << "PASSED" << std::endl;
}
  x = 1;
  std::cout << "MET_ByteSwapShort: ";
  if (MET_ByteOrderSwapShort(x) != 256)
  {
    std::cout << "FAILED: 1->" << MET_ByteOrderSwapShort(x) << std::endl;
    exitCode = EXIT_FAILURE;
  }
  else {
    std::cout << "PASSED" << std::endl;
}

  MET_ValueEnumType metType = MET_USHORT;
  MET_ValueEnumType tmpMetType = MET_USHORT;
  char              buffer[METAIO_MAX_WORD_SIZE];
  snprintf(buffer, sizeof(buffer), "MET_USHORT");
  std::cout << "MET_StringToType: ";
  MET_StringToType(buffer, &tmpMetType);
  if (tmpMetType != metType)
  {
    std::cout << "FAILED" << std::endl;
    exitCode = EXIT_FAILURE;
  }
  else {
    std::cout << "PASSED" << std::endl;
}

  std::cout << "MET_TypeToString: ";
  MET_TypeToString(MET_USHORT, buffer);
  if (strcmp(buffer, "MET_USHORT") != 0)
  {
    std::cout << "FAILED" << std::endl;
    exitCode = EXIT_FAILURE;
  }
  else {
    std::cout << "PASSED" << std::endl;
}

  int n;
  std::cout << "MET_SizeOfType: ";
  MET_SizeOfType(MET_USHORT, &n);
  if (2 != n)
  {
    std::cout << "FAILED" << std::endl;
    exitCode = EXIT_FAILURE;
  }
  else {
    std::cout << "PASSED" << std::endl;
}

  char ** wordArray;
  MET_StringToWordArray("This is a test", &n, &wordArray);
  std::cout << "MET_StringToWordArray: N: ";
  if (n != 4)
  {
    std::cout << "FAILED" << std::endl;
    exitCode = EXIT_FAILURE;
  }
  else {
    std::cout << "PASSED" << std::endl;
}
  std::cout << "MET_StringToWordArray: 1: ";
  if (strcmp(wordArray[0], "This") != 0)
  {
    std::cout << "FAILED" << std::endl;
    exitCode = EXIT_FAILURE;
  }
  else {
    std::cout << "PASSED" << std::endl;
}
  std::cout << "MET_StringToWordArray: 2: ";
  if (strcmp(wordArray[1], "is") != 0)
  {
    std::cout << "FAILED" << std::endl;
    exitCode = EXIT_FAILURE;
  }
  else {
    std::cout << "PASSED" << std::endl;
}
  std::cout << "MET_StringToWordArray: 3: ";
  if (strcmp(wordArray[2], "a") != 0)
  {
    std::cout << "FAILED" << std::endl;
    exitCode = EXIT_FAILURE;
  }
  else {
    std::cout << "PASSED" << std::endl;
}
  std::cout << "MET_StringToWordArray: 4: ";
  if (strcmp(wordArray[3], "test") != 0)
  {
    std::cout << "FAILED" << std::endl;
    exitCode = EXIT_FAILURE;
  }
  else {
    std::cout << "PASSED" << std::endl;
}

  for (int i = 0; i < n; i++)
  {
    delete[] wordArray[i];
  }
  delete[] wordArray;

  std::string fName = "this/is/a/test.com";
  std::string tmpString;
  std::cout << "MET_GetFilePathTest: ";
  MET_GetFilePath(fName, tmpString);
  if (tmpString != "this/is/a/")
  {
    std::cout << "FAILED" << std::endl;
    exitCode = EXIT_FAILURE;
  }
  else {
    std::cout << "PASSED" << std::endl;
}

  int tmpI;
  std::cout << "MET_GetFileSuffixPtr: ";
  MET_GetFileSuffixPtr(fName, &tmpI);
  if (fName[tmpI] != 'c')
  {
    std::cout << "FAILED" << std::endl;
    std::cout << &(fName[tmpI]) << std::endl;
    exitCode = EXIT_FAILURE;
  }
  else {
    std::cout << "PASSED" << std::endl;
}

  std::cout << "MET_SetFileSuffix: 1:";
  MET_SetFileSuffix(fName, ".net");
  if (fName != "this/is/a/test.net")
  {
    std::cout << "FAILED" << std::endl;
    exitCode = EXIT_FAILURE;
  }
  else {
    std::cout << "PASSED" << std::endl;
}

  fName = "this/is/a/test.com"; // Only necessary if previous test fails
  std::cout << "MET_SetFileSuffix: 2:";
  MET_SetFileSuffix(fName, "net");
  if (fName != "this/is/a/test.net")
  {
    std::cout << "FAILED" << std::endl;
    exitCode = EXIT_FAILURE;
  }
  else {
    std::cout << "PASSED" << std::endl;
}

  fName = "this/is/a/test";
  std::cout << "MET_SetFileSuffix: 3:";
  MET_SetFileSuffix(fName, "net");
  if (fName != "this/is/a/test.net")
  {
    std::cout << "FAILED" << std::endl;
    exitCode = EXIT_FAILURE;
  }
  else {
    std::cout << "PASSED" << std::endl;
}

  fName = "this/is/a/test"; // Only necessary if previous test fails
  std::cout << "MET_SetFileSuffix: 4:";
  MET_SetFileSuffix(fName, ".net");
  if (fName != "this/is/a/test.net")
  {
    std::cout << "FAILED" << std::endl;
    exitCode = EXIT_FAILURE;
  }
  else {
    std::cout << "PASSED" << std::endl;
}

  std::ofstream fout;
  fout.open("testMetaFileUtils.txt", std::ios::out);

  MET_FieldRecordType *              mF;
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

  char s[METAIO_MAX_WORD_SIZE];
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
  for (fieldIter = mFields.begin(); fieldIter != mFields.end(); ++fieldIter) {
    delete *fieldIter;
}
  mFields.clear();


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
  if (!MET_Read(fin, &mFields))
  {
    std::cout << "FAILED" << std::endl;
    exitCode = EXIT_FAILURE;
  }
  else {
    std::cout << "PASSED" << std::endl;
}

  fieldIter = mFields.begin();

  if ((*fieldIter)->defined)
  {
    int nDims = static_cast<int>((*fieldIter)->value[0]);
    if (nDims != 2)
    {
      std::cout << "nDims not equal to 2" << std::endl;
      exitCode = EXIT_FAILURE;
    }
    else {
      std::cout << "nDims: Passed" << std::endl;
}
  }
  else
  {
    std::cout << "nDims not defined" << std::endl;
    exitCode = EXIT_FAILURE;
  }

  double eSize[2];
  ++fieldIter;
  if ((*fieldIter)->defined)
  {
    eSize[0] = (*fieldIter)->value[0];
    eSize[1] = (*fieldIter)->value[1];
    if (eSize[0] != 0.5 || eSize[1] != 0.75)
    {
      std::cout << "ElementSizes are wrong: " << eSize[0] << ", " << eSize[1] << std::endl;
      exitCode = EXIT_FAILURE;
    }
    else {
      std::cout << "ElementSizes: Passed" << std::endl;
}
  }
  else
  {
    std::cout << "ElementSize not defined" << std::endl;
    exitCode = EXIT_FAILURE;
  }

  int     nNames = 0;
  char ** names = nullptr;
  ++fieldIter;
  if ((*fieldIter)->defined)
  {
    MET_StringToWordArray(reinterpret_cast<char *>((*fieldIter)->value), &nNames, &names);
    if (nNames != 2) {
      std::cout << "nNames wrong : " << nNames << std::endl;
    } else if (strcmp(names[0], "X-AXIS") != 0 || strcmp(names[1], "Y-AXIS") != 0)
    {
      std::cout << "names wrong : _" << names[0] << "_, _" << names[1] << "_" << std::endl;
      exitCode = EXIT_FAILURE;
    }
    else {
      std::cout << "Names: Passed" << std::endl;
}
  }
  else
  {
    std::cout << "DirNames not defined" << std::endl;
    exitCode = EXIT_FAILURE;
  }

  for (int i = 0; i < nNames; i++)
  {
    delete[] names[i];
  }
  delete[] names;

  for (fieldIter = mFields.begin(); fieldIter != mFields.end(); ++fieldIter)
  {
    delete *fieldIter;
  }

  return exitCode;
}
