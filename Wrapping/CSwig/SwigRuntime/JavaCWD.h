class JavaCWD
{
public:
  static void SetCWD(const char* dir);
  static const char* GetCWD();
  static int Load(const char* lib);
};
