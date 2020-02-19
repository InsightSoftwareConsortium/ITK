#include "tclap/CmdLine.h"
#include "ioutils.h"
#include <itkMGHImageIOFactory.h>
#include "itkMGHImageIO.h"

#include <itkMaskImageFilter.h>
#include "itkLabelSetDilateImageFilter.h"
#include <itkChangeLabelImageFilter.h>
#include "itkinstance.h"

using CmdLineType = class CmdLineType
{
public:
  std::string      InputIm, OutputIm;
  std::vector<int> Remove;
  float            radius;
};

void
ParseCmdLine(int argc, char * argv[], CmdLineType & CmdLineObj)
{
  using namespace TCLAP;
  try
  {
    // Define the command line object.
    CmdLine cmd("labelSetsDilateSelected ", ' ', "0.9");

    ValueArg<std::string> inArg("i", "input", "input image (label mask)", true, "result", "string");
    cmd.add(inArg);

    ValueArg<std::string> outArg("o", "output", "output image", true, "", "string");
    cmd.add(outArg);

    ValueArg<float> radArg("r", "radius", "erosion radius", true, -1.0, "float");
    cmd.add(radArg);

    UnlabeledMultiArg<int> labels(
      std::string("labels"), std::string("labels to be removed"), true, std::string("integers"));
    cmd.add(labels);
    // Parse the args.
    cmd.parse(argc, argv);

    CmdLineObj.InputIm = inArg.getValue();
    CmdLineObj.OutputIm = outArg.getValue();
    CmdLineObj.radius = radArg.getValue();
    CmdLineObj.Remove = labels.getValue();
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

  // load
  typename MaskImType::Pointer mask = readIm<MaskImType>(CmdLineObj.InputIm);
  // filter out labels to be discarded
  itk::Instance<itk::ChangeLabelImageFilter<MaskImType, MaskImType>> Deleter;
  Deleter->SetInput(mask);
  for (unsigned i = 0; i < CmdLineObj.Remove.size(); i++)
  {
    Deleter->SetChange(CmdLineObj.Remove[i], 0);
  }
  // Label dilation
  itk::Instance<itk::LabelSetDilateImageFilter<MaskImType, MaskImType>> Dilate;
  Dilate->SetInput(Deleter->GetOutput());
  Dilate->SetRadius(CmdLineObj.radius);
  Dilate->SetUseImageSpacing(true);
  writeIm<MaskImType>(Dilate->GetOutput(), CmdLineObj.OutputIm);
}

/////////////////////////////////

int
main(int argc, char * argv[])
{
  // itk::ObjectFactoryBase::UnRegisterAllFactories();
  itk::MGHImageIOFactory::RegisterOneFactory();
  itk::ObjectFactoryBase::RegisterFactory(itk::MGHImageIOFactory::New());

  int         dim1 = 3;
  CmdLineType CmdLineObj;
  ParseCmdLine(argc, argv, CmdLineObj);
  itk::ImageIOBase::IOComponentType ComponentType;

  if (!readImageInfo(CmdLineObj.InputIm, &ComponentType, &dim1))
  {
    std::cerr << "Failed to open " << CmdLineObj.InputIm << std::endl;
    return (EXIT_FAILURE);
  }
  switch (dim1)
  {
    case 2:
      doDilate<int, 2>(CmdLineObj);
      break;
    case 3:
      doDilate<int, 3>(CmdLineObj);
      break;
    default:
      std::cerr << "Unsupported dimension" << std::endl;
      return (EXIT_FAILURE);
      break;
  }

  return EXIT_SUCCESS;
}
