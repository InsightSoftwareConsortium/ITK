#include "itkMesh.h"
#include "itkVectorContainer.h"

template itk::Mesh<int>;
template itk::Mesh<int>::Cell;
template itk::VectorContainer<int, float>;

int main(void)
{
  return 0;
}
