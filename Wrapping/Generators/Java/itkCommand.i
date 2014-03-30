// This is needed for a reference in SwigExtras.i
// The actuall itkCommand wrapping is done in wrap_ITKCommonBase.cxx by
// Swig
class itkCommand
{
public:
  virtual ~itkCommand() {}
};
