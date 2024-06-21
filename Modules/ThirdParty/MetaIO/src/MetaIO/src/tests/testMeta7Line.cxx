#include <iostream>

#include <metaLine.h>

int
main(int, char *[])
{
  std::cout << "Creating test file ...";
  auto * Line = new MetaLine(3);
  Line->ID(0);
  LinePnt * pnt;

  unsigned int i;
  for (i = 0; i < 10; i++)
  {
    pnt = new LinePnt(3);
    pnt->m_X[0] = static_cast<float>(0.2);
    float i_f = static_cast<float>(i);
    pnt->m_X[1] = i_f;
    pnt->m_X[2] = i_f;
    pnt->m_V[0][0] = static_cast<float>(0.3);
    pnt->m_V[0][1] = i_f;
    pnt->m_V[0][2] = i_f;
    pnt->m_V[1][0] = static_cast<float>(0.4);
    pnt->m_V[1][1] = i_f + 1;
    pnt->m_V[1][2] = i_f + 1;
    Line->GetPoints().push_back(pnt);
  }

  std::cout << "Writing test file ...";

  Line->BinaryData(true);

  Line->Write("myLine.meta");

  std::cout << "done" << '\n';
  std::cout << "Reading test file ...";

  Line->Clear();
  Line->Read("myLine.meta");

  MetaLine LineRead("myLine.meta");
  MetaLine LineCopy(&LineRead);

  std::cout << "PointDim = " << LineCopy.PointDim() << '\n';
  std::cout << "NPoints = " << LineCopy.NPoints() << '\n';
  std::cout << "ElementType = " << LineCopy.ElementType() << '\n';

  Line->PrintInfo();

  MetaLine::PointListType                 list = Line->GetPoints();
  MetaLine::PointListType::const_iterator it = list.begin();

  i = 0;
  while (it != list.end())
  {
    std::cout << "Point #" << i++ << ":" << '\n';
    std::cout << "position = ";

    for (unsigned int d = 0; d < 3; d++)
    {
      std::cout << (*it)->m_X[d] << " ";
    }
    std::cout << '\n';
    std::cout << "First normal = ";
    for (unsigned int d = 0; d < 3; d++)
    {
      std::cout << (*it)->m_V[0][d] << " ";
    }
    std::cout << '\n';
    std::cout << "Second normal = ";
    for (unsigned int d = 0; d < 3; d++)
    {
      std::cout << (*it)->m_V[1][d] << " ";
    }
    std::cout << '\n';
    ++it;
  }

  delete Line;
  std::cout << "done" << '\n';
  return EXIT_SUCCESS;
}
