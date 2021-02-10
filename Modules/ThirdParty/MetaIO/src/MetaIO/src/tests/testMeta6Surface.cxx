#include <iostream>

#include <metaSurface.h>

int
main(int, char *[])
{
  std::cout << "Creating test file ...";
  auto * surface = new MetaSurface(3);
  surface->ID(0);
  SurfacePnt * pnt;

  unsigned int i;
  for (i = 0; i < 10; i++)
  {
    pnt = new SurfacePnt(3);
    pnt->m_X[0] = static_cast<float>(0.2);
    pnt->m_X[1] = i;
    pnt->m_X[2] = i;
    pnt->m_V[0] = static_cast<float>(0.8);
    pnt->m_V[1] = i;
    pnt->m_V[2] = i;
    surface->GetPoints().push_back(pnt);
  }


  std::cout << "Writing ASCII test file ...";

  surface->Write("mySurface.meta");

  std::cout << "done" << std::endl;
  std::cout << "Reading ASCII test file ...";

  surface->Clear();
  surface->Read("mySurface.meta");
  surface->PrintInfo();

  MetaSurface::PointListType                 list = surface->GetPoints();
  MetaSurface::PointListType::const_iterator it = list.begin();

  while (it != list.end())
  {
    for (unsigned int d = 0; d < 3; d++)
    {
      std::cout << (*it)->m_X[d] << " ";
    }
    std::cout << std::endl;
    for (unsigned int d = 0; d < 3; d++)
    {
      std::cout << (*it)->m_V[d] << " ";
    }

    std::cout << std::endl;
    for (unsigned int d = 0; d < 4; d++)
    {
      std::cout << (*it)->m_Color[d] << " ";
    }
    std::cout << std::endl;
    ++it;
  }

  std::cout << "Writing Binary test file ...";
  surface->BinaryData(true);
  surface->ElementType(MET_FLOAT);
  surface->Write("mySurface.meta");

  std::cout << "done" << std::endl;
  std::cout << "Reading Binary test file ...";

  surface->Clear();
  surface->Read("mySurface.meta");
  surface->PrintInfo();

  MetaSurface::PointListType list2 = surface->GetPoints();
  it = list2.begin();

  while (it != list2.end())
  {
    for (unsigned int d = 0; d < 3; d++)
    {
      std::cout << (*it)->m_X[d] << " ";
    }
    std::cout << std::endl;
    for (unsigned int d = 0; d < 3; d++)
    {
      std::cout << (*it)->m_V[d] << " ";
    }
    std::cout << std::endl;
    ++it;
  }

  delete surface;
  std::cout << "done" << std::endl;
  return EXIT_SUCCESS;
}
