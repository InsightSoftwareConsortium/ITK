#include "tclap/CmdLine.h"
#include "ioutils.h"

#include <itkMaskImageFilter.h>
#include "itkLabelSetDilateImageFilter.h"
#include "itkinstance.h"

typedef class CmdLineType
{
public:
  std::string InputIm, OutputIm;
  float       radius;
  int         iterations;
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

    ValueArg<std::string> outArg("o", "output", "output image", true, "", "string");
    cmd.add(outArg);

    ValueArg<float> radArg("r", "radius", "erosion radius", true, -1.0, "float");
    cmd.add(radArg);
    ValueArg<int> iterArg("", "iterations", "loop iterations", false, 1, "integer");
    cmd.add(iterArg);

    // Parse the args.
    cmd.parse(argc, argv);

    CmdLineObj.InputIm = inArg.getValue();
    CmdLineObj.OutputIm = outArg.getValue();
    CmdLineObj.radius = radArg.getValue();
    CmdLineObj.iterations = iterArg.getValue();
  }
  catch (ArgException & e) // catch any exceptions
  {
    std::cerr << "error: " << e.error() << " for arg " << e.argId() << std::endl;
  }
}

template <class MaskPixType, int dim>
void
doDilate(const CmdLineType & CmdLineObj)
{
  // test behaviour in a loop
  using MaskImType = typename itk::Image<MaskPixType, dim>;

  // load
  typename MaskImType::Pointer mask = readIm<MaskImType>(CmdLineObj.InputIm);

  // Label dilation
  itk::Instance<itk::LabelSetDilateImageFilter<MaskImType, MaskImType>> Dilate;
  Dilate->SetRadius(CmdLineObj.radius);
  Dilate->SetUseImageSpacing(true);

  for (int it = 0; it < CmdLineObj.iterations; it++)
  {
    Dilate->SetInput(mask);
    Dilate->Update();
    mask = Dilate->GetOutput();
    mask->DisconnectPipeline();
  }

  writeIm<MaskImType>(mask, CmdLineObj.OutputIm);
  // Dilate->writeDist("/tmp/pdist.nii.gz");
}

/////////////////////////////////

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
      doDilate<unsigned char, 2>(CmdLineObj);
      break;
    case 3:
      doDilate<unsigned char, 3>(CmdLineObj);
      break;
    default:
      std::cerr << "Unsupported dimension" << std::endl;
      return (EXIT_FAILURE);
      break;
  }
  return EXIT_SUCCESS;
}
