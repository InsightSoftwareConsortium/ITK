#include "itkPoint.h"
#include "itkVectorContainer.h"
#include "itkBoundingBox.h"

int itkModifiedTimeTest( int, char* [] )
{

  typedef itk::Point< double, 3 > Point;
  typedef itk::VectorContainer< unsigned long int, Point > PointsContainer;
  typedef itk::BoundingBox< unsigned long int, 3, double, PointsContainer > BoundingBox;

  Point p,q,r;
  
  p.Fill(0);
  q.Fill(0);
  r.Fill(0);

  BoundingBox::Pointer bb = BoundingBox::New();
  
  PointsContainer::Pointer pc = PointsContainer::New();

  pc->InsertElement(0,p);
  pc->InsertElement(1,q);
  pc->Modified();

  bb->SetPoints(pc);

  const unsigned long bbBeforeTime = bb->GetMTime();
  const unsigned long pcBeforeTime = pc->GetMTime();


  std::cout<<"BB time before modification: "<< bbBeforeTime <<std::endl;
  std::cout<<"PC time before modification: "<< pcBeforeTime <<std::endl;

  pc->InsertElement(2,r);
  pc->Modified(); // call the Modified function to update the modified time of the container
  
  const unsigned long bbAfterTime = bb->GetMTime();
  const unsigned long pcAfterTime = pc->GetMTime();

  std::cout<<"BB time after modification: "<< bbAfterTime <<std::endl;
  std::cout<<"PC time after modification: "<< pcAfterTime <<std::endl;


  if( pcAfterTime == pcBeforeTime )
    {
    std::cout << "Points Container Modified Time is not being " << std::endl;
    std::cout << "updated by call to Modified()" << std::endl;
    return EXIT_FAILURE;
    }

  if( bbAfterTime == bbBeforeTime )
    {
    std::cout << "Bounding Box Modified Time is not being " << std::endl;
    std::cout << "updated by changes in the points" << std::endl;
    return EXIT_FAILURE;
    }


  if( bbAfterTime < pcAfterTime )
    {
    std::cout << "Bounding Box Modified Time is not as recent " << std::endl;
    std::cout << "as the modifiction in the points" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test PASSED !" << std::endl;

  return EXIT_SUCCESS;

}
