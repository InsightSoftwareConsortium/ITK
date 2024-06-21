#include <iostream>

#include <metaObject.h>

int
main(int, char *[])
{
  MetaObject tObj;

  tObj.InitializeEssential(3);
  tObj.FileName("testObject.txt");
  tObj.Comment("TestObject");
  tObj.ObjectTypeName("Object");
  tObj.ObjectSubTypeName("MinorObject");
  tObj.Position(0, 1);
  tObj.Position(1, 2);
  tObj.Position(2, 3);
  double orient[9];
  int    i;
  for (i = 0; i < 9; i++)
  {
    orient[i] = 0;
  }
  orient[0] = 1;
  orient[5] = 1;
  orient[7] = 1;
  tObj.Orientation(orient);
  tObj.ElementSpacing(0, 1);
  tObj.ElementSpacing(1, 2);
  tObj.ElementSpacing(2, 1);

  // Add user's defined fields
  int myarray[3];
  myarray[0] = 1;
  myarray[1] = 2;
  myarray[2] = 3;
  tObj.AddUserField("MyName", MET_STRING, static_cast<int>(strlen("Julien")), "Julien");
  tObj.AddUserField("MyArray", MET_INT_ARRAY, 3, myarray);

  float myMatrix[4];
  for (i = 0; i < 4; i++)
  {
    myMatrix[i] = static_cast<float>(i);
  }
  tObj.AddUserField("MyMatrix", MET_FLOAT_MATRIX, 2, myMatrix);

  std::cout << "*** Writing this info..." << std::endl;
  tObj.PrintInfo();
  tObj.Write();

  tObj.Clear();
  tObj.ClearUserFields();

  std::cout << "*** Adding user fields..." << std::endl;
  tObj.AddUserField("MyName", MET_STRING);
  tObj.AddUserField("MyArray", MET_INT_ARRAY, 3);
  tObj.AddUserField("MyMatrix", MET_FLOAT_MATRIX, 2);

  std::cout << "*** Pre-reading..." << std::endl;
  tObj.PrintInfo();
  std::cout << "*** Reading..." << std::endl;
  tObj.Read();
  std::cout << "*** Reading results..." << std::endl;
  tObj.PrintInfo();

  char * name = static_cast<char *>(tObj.GetUserField("MyName"));
  if (name == nullptr || strcmp(name, "Julien") != 0)
  {
    std::cout << "MyName: FAIL" << '\n';
    return EXIT_FAILURE;
  }

  delete[] name;

  int * array = static_cast<int *>(tObj.GetUserField("MyArray"));
  for (i = 0; i < 3; i++)
  {
    if (array == nullptr || array[i] != i + 1)
    {
      std::cout << "MyArray: FAIL" << '\n';
      delete[] array;
      return EXIT_FAILURE;
    }
  }

  delete[] array;

  auto * matrix = static_cast<float *>(tObj.GetUserField("MyMatrix"));
  for (i = 0; i < 4; i++)
  {
    if (matrix == nullptr || matrix[i] != i)
    {
      std::cout << "MyMatrix: FAIL" << '\n';
      delete[] matrix;
      return EXIT_FAILURE;
    }
  }

  delete[] matrix;

  std::cout << "PASSED!" << '\n';

  tObj.Clear();
  tObj.ClearUserFields();

  tObj.FileName("testObject2.txt");
  tObj.InitializeEssential(2);
  tObj.Position(0, 4);
  tObj.ElementSpacing(0, 2);
  tObj.PrintInfo();
  tObj.Write();
  tObj.Clear();

  tObj.Read();
  tObj.PrintInfo();
  if (tObj.NDims() != 2)
  {
    std::cout << "NDims: FAIL" << '\n';
    return EXIT_FAILURE;
  }
  else
  {
    std::cout << "NDims: PASS" << '\n';
  }

  int zero = 0;
  if (tObj.Position(zero) != 4)
  {
    std::cout << "Position: FAIL :" << tObj.Position(zero) << '\n';
    return EXIT_FAILURE;
  }
  else
  {
    std::cout << "Position: PASS" << '\n';
  }

  if (tObj.ElementSpacing(zero) != 2)
  {
    std::cout << "ElementSpacing: FAIL: " << tObj.ElementSpacing(zero) << '\n';
    return EXIT_FAILURE;
  }
  else
  {
    std::cout << "ElementSpacing: PASS" << '\n';
  }

  auto * inDataChar = new char[1];
  inDataChar[0] = 1;
  auto * outDataChar = new char[1];
  if (!MET_ValueToValueN(MET_CHAR_ARRAY, inDataChar, 0, MET_CHAR_ARRAY, outDataChar, 1))
  {
    std::cout << "MET_ValueToValueN: FAIL" << '\n';
    return EXIT_FAILURE;
  }
  else
  {
    std::cout << "outDataChar = " << static_cast<int>(outDataChar[0]) << '\n';
  }

  delete[] inDataChar;
  delete[] outDataChar;

  auto * inDataUChar = new unsigned char[1];
  inDataUChar[0] = 1;
  auto * outDataUChar = new unsigned char[1];
  if (!MET_ValueToValueN(MET_UCHAR_ARRAY, inDataUChar, 0, MET_UCHAR_ARRAY, outDataUChar, 1))
  {
    std::cout << "MET_ValueToValueN: FAIL" << '\n';
    return EXIT_FAILURE;
  }
  else
  {
    std::cout << "outDataUChar = " << static_cast<int>(outDataUChar[0]) << '\n';
  }

  delete[] inDataUChar;
  delete[] outDataUChar;


  std::cout << "[DONE]" << '\n';
  return EXIT_SUCCESS;
}
