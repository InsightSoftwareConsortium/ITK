#include "itkImage.h"

#ifdef CABLE_CONFIGURATION
#include "wrap_ITKCommon.h"

namespace _cable_
{
  const char* const group = ITK_WRAP_GROUP(itkImage);
  namespace wrappers
  {
    namespace itk
    {
      typedef ::itk::ImageBase<2> ImageBase2;
      typedef ::itk::ImageBase<3> ImageBase3;
      typedef ::itk::Image<float, 2> ImageF2;
      typedef ::itk::Image<float, 3> ImageF3;
      typedef ::itk::Image<unsigned short, 2> ImageUS2;
      typedef ::itk::Image<unsigned short, 3> ImageUS3;
      typedef ImageF2::Pointer ImageF2_Pointer;
      typedef ImageF3::Pointer ImageF3_Pointer;
      typedef ImageUS2::Pointer ImageUS2_Pointer;
      typedef ImageUS3::Pointer ImageUS3_Pointer;
    }
  }
}

void force_instantiate()
{
  using namespace _cable_::wrappers::itk;
  sizeof(ImageBase2);
  sizeof(ImageBase3);
  sizeof(ImageF2);
  sizeof(ImageF3);
  sizeof(ImageUS2);
  sizeof(ImageUS3);
  sizeof(ImageF2_Pointer);
  sizeof(ImageF3_Pointer);
  sizeof(ImageUS2_Pointer);
  sizeof(ImageUS3_Pointer);
}

#endif
