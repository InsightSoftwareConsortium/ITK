// tool to report the location and values of non zero voxels so we can
// check differences between versions

#include "itkinstance.h"

#include "tclap/CmdLine.h"
#include "ioutils.h"

#include "itkImageRegionIterator.h"

typedef class CmdLineType
{
public:
  std::string InputIm;
} CmdLineType;

void
ParseCmdLine(int argc, char * argv[], CmdLineType & CmdLineObj)
{
  using namespace TCLAP;
  try
  {
    // Define the command line object.
    CmdLine cmd("varSize ", ' ', "0.9");

    ValueArg<std::string> inArg("i", "input", "input image (label mask)", true, "result", "string");
    cmd.add(inArg);

    // Parse the args.
    cmd.parse(argc, argv);

    CmdLineObj.InputIm = inArg.getValue();
  }
  catch (ArgException & e) // catch any exceptions
  {
    std::cerr << "error: " << e.error() << " for arg " << e.argId() << std::endl;
  }
}

template <class PixType, int dim>
void
doSearch(const CmdLineType & CmdLineObj)
{
  using MaskImType = typename itk::Image<PixType, dim>;

  // load
  typename MaskImType::Pointer mask = readIm<MaskImType>(CmdLineObj.InputIm);
  using IterType = typename itk::ImageRegionIterator<MaskImType>;

  IterType iter(mask, mask->GetLargestPossibleRegion());

  for (iter.GoToBegin(); !iter.IsAtEnd(); ++iter)
  {
    PixType Val = iter.Get();
    if (Val)
    {
      typename MaskImType::IndexType pos = iter.GetIndex();
      typename MaskImType::PointType wc;
      mask->TransformIndexToPhysicalPoint(pos, wc);
      std::cout << Val;
      for (unsigned i = 0; i < dim; i++)
      {
        std::cout << "," << std::setprecision(10) << wc[i];
      }
      std::cout << std::endl;
    }
  }
}

int
main(int argc, char * argv[])
{
  int         dim1;
  CmdLineType CmdLineObj;

  ParseCmdLine(argc, argv, CmdLineObj);

  itk::ImageIOBase::IOComponentType ComponentType;
  itk::MultiThreader::SetGlobalMaximumNumberOfThreads(1);

  if (!readImageInfo(CmdLineObj.InputIm, &ComponentType, &dim1))
  {
    std::cerr << "Failed to open " << CmdLineObj.InputIm << std::endl;
    return (EXIT_FAILURE);
  }

  switch (dim1)
  {
    case 2:
      doSearch<int, 2>(CmdLineObj);
      break;
    case 3:
      doSearch<int, 3>(CmdLineObj);
      break;
    default:
      std::cerr << "Unsupported dimension" << std::endl;
      return (EXIT_FAILURE);
      break;
  }
  return EXIT_SUCCESS;
}
