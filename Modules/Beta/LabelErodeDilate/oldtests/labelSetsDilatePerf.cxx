#include "tclap/CmdLine.h"
#include "ioutils.h"

#include <itkMaskImageFilter.h>
#include "itkLabelSetDilateImageFilter.h"
#include "itkTimeProbe.h"

// Aidan's trick
#include <itkSmartPointer.h>
namespace itk
{
template <typename T>
class Instance : public T::Pointer
{
public:
  Instance()
    : SmartPointer<T>(T::New())
  {}
};
} // namespace itk

typedef class CmdLineType
{
public:
  std::string InputIm, OutputIm;
  float       radius;
  int         repetitions, threads;
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

    ValueArg<int> threadArg("", "threads", "number of threads", false, 1, "integer");
    cmd.add(threadArg);

    ValueArg<int> repArg("", "repetitions", "number of repeats", false, 1, "integer");
    cmd.add(repArg);

    // Parse the args.
    cmd.parse(argc, argv);

    CmdLineObj.InputIm = inArg.getValue();
    CmdLineObj.OutputIm = outArg.getValue();
    CmdLineObj.radius = radArg.getValue();
    CmdLineObj.threads = threadArg.getValue();
    CmdLineObj.repetitions = repArg.getValue();
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
  using MaskImType = typename itk::Image<MaskPixType, dim>;
  itk::MultiThreader::SetGlobalMaximumNumberOfThreads(CmdLineObj.threads);
  itk::TimeProbe timer;

  // load
  typename MaskImType::Pointer mask = readIm<MaskImType>(CmdLineObj.InputIm);

  // apply mask to size

  // Label erosion
  itk::Instance<itk::LabelSetDilateImageFilter<MaskImType, MaskImType>> Dilate;
  Dilate->SetInput(mask);
  Dilate->SetRadius(CmdLineObj.radius);
  Dilate->SetUseImageSpacing(true);

  std::cout << "Iterations,lab_dilate_timed,radius,threads" << std::endl;
  for (int r = 0; r < CmdLineObj.repetitions; r++)
  {
    Dilate->Modified();
    timer.Start();
    Dilate->Update();
    timer.Stop();
  }
  std::cout << std::setprecision(3) << CmdLineObj.repetitions << "," << timer.GetMean() << "," << CmdLineObj.radius
            << "," << CmdLineObj.threads << std::endl;
  writeIm<MaskImType>(Dilate->GetOutput(), CmdLineObj.OutputIm);
  // Erode->writeDist("/tmp/dist.mhd");
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
