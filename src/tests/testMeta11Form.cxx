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

  std::cout << "Read: " << '\n';
  tObj.Read();
  std::cout << "PrintInfo: " << '\n';
  tObj.PrintInfo();

  std::cout << "Check fields: " << '\n';
  char * name = static_cast<char *>(tObj.GetUserField("MyName"));
  if (!strcmp(name, "Julien"))
  {
    std::cout << "MyName: FAIL" << '\n';
    return EXIT_FAILURE;
  }
  std::cout << "MyName: " << name << '\n';
  delete[] name;

  int * array = static_cast<int *>(tObj.GetUserField("MyArray"));

  for (i = 0; i < 3; i++)
  {
    if (array[i] != i + 1)
    {
      std::cout << "MyArray: FAIL" << '\n';
      return EXIT_FAILURE;
    }
  }
  std::cout << "MyArray: PASS" << '\n';
  delete[] array;

  auto * matrix = static_cast<float *>(tObj.GetUserField("MyMatrix"));
  for (i = 0; i < 4; i++)
  {
    if (matrix[i] != i)
    {
      std::cout << "MyMatrix: FAIL" << '\n';
      return EXIT_FAILURE;
    }
  }
  std::cout << "MyMatrix: PASS" << '\n';
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

  std::cout << "PASSED!" << '\n';
  return EXIT_SUCCESS;
}
