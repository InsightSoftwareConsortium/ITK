#include "itkMesh.h"
#include "itkVectorContainer.h"

template class itk::Mesh<int>;
template class itk::VectorContainer<unsigned int, float>;

int main(void)
{
  return 0;
}
