#include "itkNeighborhood.h"
#include "itkBinaryBallStructuringElement.h"

namespace neighborhood
{

}

namespace structuringElement 
{
  typedef itk::BinaryBallStructuringElement<float, 2 >::Self             F2;
  typedef itk::BinaryBallStructuringElement<float, 3 >::Self             F3;
  typedef itk::BinaryBallStructuringElement<unsigned char, 2 >::Self     UC2;
  typedef itk::BinaryBallStructuringElement<unsigned char, 3 >::Self     UC3;
  typedef itk::BinaryBallStructuringElement<unsigned short, 2 >::Self    US2;
  typedef itk::BinaryBallStructuringElement<unsigned short, 3 >::Self    US3;
}


