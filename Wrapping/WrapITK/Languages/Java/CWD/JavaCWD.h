#ifndef __JavaCWD_h
#define __JavaCWD_h
namespace itk
/** \class TODO */
class JavaCWD
{
public:
  static void SetCWD(const char* dir);
  static const char* GetCWD();
  static int Load(const char* lib);
};
}
#endif
