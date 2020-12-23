#include <iostream>

#include <metaForm.h>

int
main(int, char *[])
{
  MetaForm tObj;

  MetaForm::InitializeEssential();
  tObj.FileName("testForm.txt");
  tObj.Comment("TestForm");
  tObj.FormTypeName("Form");

  // Add user's defined fields
  int myarray[3];
  myarray[0] = 1;
  myarray[1] = 2;
  myarray[2] = 3;
  tObj.AddUserField("MyName", MET_STRING, static_cast<int>(strlen("Julien")), "Julien");
  tObj.AddUserField("MyArray", MET_INT_ARRAY, 3, myarray);

  int   i;
  float myMatrix[4];
  for (i = 0; i < 4; i++)
  {
    myMatrix[i] = static_cast<float>(i);
  }
  tObj.AddUserField("MyMatrix", MET_FLOAT_MATRIX, 2, myMatrix);

  tObj.PrintInfo();
  tObj.Write();

  tObj.Clear();
  tObj.ClearUserFields();

  tObj.AddUserField("MyName", MET_STRING, static_cast<int>(strlen("default")), "default");
  tObj.AddUserField("MyArray", MET_INT_ARRAY, 3, myarray);
  tObj.AddUserField("MyMatrix", MET_FLOAT_MATRIX, 2, myMatrix);

  std::cout << "Read: " << std::endl;
  tObj.Read();
  std::cout << "PrintInfo: " << std::endl;
  tObj.PrintInfo();

  std::cout << "Check fields: " << std::endl;
  char * name = static_cast<char *>(tObj.GetUserField("MyName"));
  if (!strcmp(name, "Julien"))
  {
    std::cout << "MyName: FAIL" << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "MyName: " << name << std::endl;
  delete[] name;

  int * array = static_cast<int *>(tObj.GetUserField("MyArray"));

  for (i = 0; i < 3; i++)
  {
    if (array[i] != i + 1)
    {
      std::cout << "MyArray: FAIL" << std::endl;
      return EXIT_FAILURE;
    }
  }
  std::cout << "MyArray: PASS" << std::endl;
  delete[] array;

  auto * matrix = static_cast<float *>(tObj.GetUserField("MyMatrix"));
  for (i = 0; i < 4; i++)
  {
    if (matrix[i] != i)
    {
      std::cout << "MyMatrix: FAIL" << std::endl;
      return EXIT_FAILURE;
    }
  }
  std::cout << "MyMatrix: PASS" << std::endl;
  delete[] matrix;

  tObj.Clear();
  tObj.ClearUserFields();

  tObj.FileName("testObject2.txt");
  MetaForm::InitializeEssential();
  tObj.PrintInfo();
  tObj.Write();
  tObj.Clear();

  tObj.Read();
  tObj.PrintInfo();

  std::cout << "PASSED!" << std::endl;
  return EXIT_SUCCESS;
}
