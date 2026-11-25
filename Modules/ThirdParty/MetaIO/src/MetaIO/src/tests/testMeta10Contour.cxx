#include <iostream>

#include <metaContour.h>

int
main(int, char *[])
{
  std::cout << "Creating test file ..." << '\n';
  MetaContour Contour(3);
  Contour.ID(0);
  Contour.Name("First Contour");
  ContourControlPnt * pnt;

  std::cout << "Allocating points..." << '\n';
  unsigned int i;
  for (i = 0; i < 10; i++)
  {
    pnt = new ContourControlPnt(3);
    pnt->m_Id = i;
    pnt->m_XPicked[0] = 0;
    pnt->m_XPicked[1] = 1;
    pnt->m_XPicked[2] = 2;
    pnt->m_X[0] = static_cast<float>(0.2);
    pnt->m_X[1] = static_cast<float>(i);
    pnt->m_X[2] = static_cast<float>(i);
    Contour.GetControlPoints().push_back(pnt);
  }

  Contour.Interpolation(MET_EXPLICIT_INTERPOLATION);

  ContourInterpolatedPnt * pntI;
  for (i = 0; i < 5; i++)
  {
    pntI = new ContourInterpolatedPnt(3);
    pntI->m_Id = i;
    pntI->m_X[0] = static_cast<float>(0.2);
    pntI->m_X[1] = static_cast<float>(i);
    pntI->m_X[2] = static_cast<float>(i);
    Contour.GetInterpolatedPoints().push_back(pntI);
  }

  std::cout << "Writing test file ..." << '\n';
  Contour.BinaryData(true);
  Contour.Write("Contours.meta");

  std::cout << "  done" << '\n';

  std::cout << "Reading test file ..." << '\n';
  Contour.Read("Contours.meta");

  std::cout << "  done" << '\n';

  Contour.PrintInfo();

  std::cout << "Accessing pointlist..." << '\n';

  MetaContour::ControlPointListType                 plist = Contour.GetControlPoints();
  MetaContour::ControlPointListType::const_iterator it = plist.begin();

  while (it != plist.end())
  {
    std::cout << (*it)->m_Id << " ";
    unsigned int d;
    for (d = 0; d < 3; d++)
    {
      std::cout << (*it)->m_X[d] << " ";
    }
    for (d = 0; d < 3; d++)
    {
      std::cout << (*it)->m_XPicked[d] << " ";
    }
    for (d = 0; d < 3; d++)
    {
      std::cout << (*it)->m_V[d] << " ";
    }
    std::cout << '\n';
    ++it;
  }


  MetaContour::InterpolatedPointListType                 ilist = Contour.GetInterpolatedPoints();
  MetaContour::InterpolatedPointListType::const_iterator iti = ilist.begin();

  while (iti != ilist.end())
  {
    std::cout << (*iti)->m_Id << " ";
    for (unsigned int d = 0; d < 3; d++)
    {
      std::cout << (*iti)->m_X[d] << " ";
    }

    std::cout << '\n';
    ++iti;
  }

  std::cout << "done" << '\n';
  return 0;
}
