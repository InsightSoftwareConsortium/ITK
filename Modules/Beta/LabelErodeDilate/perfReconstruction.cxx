#include "tclap/CmdLine.h"
#include "ioutils.h"

#include "itkReconstructionByDilationImageFilter.h"
#include "itkTimeProbe.h"

// Aidan's trick
#include <itkSmartPointer.h>
namespace itk
{
    template <typename T>
    class Instance : public T::Pointer {
    public:
        Instance() : SmartPointer<T>( T::New() ) {}
    };
}



typedef class CmdLineType
{
public:
  std::string InputIm, MarkerIm, OutputIm;
  int repetitions, threads;

};

void ParseCmdLine(int argc, char* argv[],
                  CmdLineType &CmdLineObj
                  )
{
  using namespace TCLAP;
  try
    {
    // Define the command line object.
    CmdLine cmd("recon ", ' ', "0.9");

    ValueArg<std::string> inArg("i","input","input image (label mask)",true,"result","string");
    cmd.add( inArg );

    ValueArg<std::string> mArg("m","marker","marker image (label mask)",true,"result","string");
    cmd.add( mArg );


    ValueArg<std::string> outArg("o","output","output image", true,"","string");
    cmd.add( outArg );

    ValueArg<int> threadArg("", "threads", "number of threads", false, 1, "integer");
    cmd.add(threadArg);

    ValueArg<int> repArg("", "repetitions", "number of repeats", false, 1, "integer");
    cmd.add(repArg);

    // Parse the args.
    cmd.parse( argc, argv );

    CmdLineObj.InputIm = inArg.getValue();
    CmdLineObj.MarkerIm = mArg.getValue();
    CmdLineObj.OutputIm = outArg.getValue();
    CmdLineObj.threads = threadArg.getValue();
    CmdLineObj.repetitions = repArg.getValue();

    }
  catch (ArgException &e)  // catch any exceptions
    {
    std::cerr << "error: " << e.error() << " for arg " << e.argId() << std::endl;
    }
}

template <class MaskPixType, int dim>
void doDilate(const CmdLineType &CmdLineObj)
{
  typedef typename itk::Image<MaskPixType, dim> MaskImType;
  itk::MultiThreader::SetGlobalMaximumNumberOfThreads(CmdLineObj.threads);
  itk::TimeProbe timer;

  // load
  typename MaskImType::Pointer mask = readIm<MaskImType>(CmdLineObj.InputIm);
  typename MaskImType::Pointer marker = readIm<MaskImType>(CmdLineObj.MarkerIm);

  // apply mask to size

  // Label erosion
  itk::Instance< itk::ReconstructionByDilationImageFilter<MaskImType, MaskImType> > Dilate;
  Dilate->SetInput(marker);
  Dilate->SetMaskImage(mask);
  Dilate->UseInternalCopyOff();

  std::cout << "Iterations,lab_dilate_timed" << std::endl;
  for (int r = 0; r < CmdLineObj.repetitions; r++)
    {
    Dilate->Modified();
    timer.Start();
    Dilate->Update();
    timer.Stop();
    }
  std::cout << std::setprecision(3) << CmdLineObj.repetitions << "," << timer.GetMeanTime() << std::endl;
  writeIm<MaskImType>(Dilate->GetOutput(), CmdLineObj.OutputIm);
  //Erode->writeDist("/tmp/dist.mhd");

}

/////////////////////////////////

int main(int argc, char * argv[])
{

  int dim1;
  CmdLineType CmdLineObj;
  ParseCmdLine(argc, argv, CmdLineObj);

  itk::ImageIOBase::IOComponentType ComponentType;

  if (!readImageInfo(CmdLineObj.InputIm, &ComponentType, &dim1))
    {
    std::cerr << "Failed to open " << CmdLineObj.InputIm << std::endl;
    return(EXIT_FAILURE);
    }

  switch (dim1)
    {
    case 2:
      doDilate<short, 2>(CmdLineObj);
      break;
    case 3:
      doDilate<short, 3>(CmdLineObj);
      break;
    default:
      std::cerr << "Unsupported dimension" << std::endl;
      return (EXIT_FAILURE);
      break;
    }
  return EXIT_SUCCESS;
}
