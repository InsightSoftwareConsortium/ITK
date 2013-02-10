#include "tclap/CmdLine.h"
#include "ioutils.h"

#include <itkMaskImageFilter.h>
#include <itkSquareImageFilter.h>
#include <itkBinaryThresholdImageFilter.h>
#include <itkDivideByConstantImageFilter.h>

#include <itkParabolicDilateLabelPropImageFilter.h>

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
  std::string InputIm, OutputIm, SizeIm;
} CmdLineType;

void ParseCmdLine(int argc, char* argv[],
                  CmdLineType &CmdLineObj
                  )
{
  using namespace TCLAP;
  try
    {
    // Define the command line object.
    CmdLine cmd("varSize ", ' ', "0.9");

    ValueArg<std::string> inArg("i","input","input image (binary mask)",true,"result","string");
    cmd.add( inArg );

    ValueArg<std::string> sizeArg("s","sizefield","size image image",true,"result","string");
    cmd.add( sizeArg );

    ValueArg<std::string> outArg("o","output","output image", true,"","string");
    cmd.add( outArg );

    // Parse the args.
    cmd.parse( argc, argv );

    CmdLineObj.InputIm = inArg.getValue();
    CmdLineObj.OutputIm = outArg.getValue();
    CmdLineObj.SizeIm = sizeArg.getValue();

    }
  catch (ArgException &e)  // catch any exceptions
    {
    std::cerr << "error: " << e.error() << " for arg " << e.argId() << std::endl;
    }
}

/////////////////////////////////
template <class MaskPixType, class SizePixType, int dim>
void doDilate(const CmdLineType &CmdLineObj)
{
  typedef typename itk::Image<MaskPixType, dim> MaskImType;
  typedef typename itk::Image<SizePixType, dim> SizeImType;

  typedef typename itk::Image<float, dim> DistImType;

  // load
  typename MaskImType::Pointer mask = readIm<MaskImType>(CmdLineObj.InputIm);
  typename SizeImType::Pointer dilsize = readIm<SizeImType>(CmdLineObj.SizeIm);



  // square this to give distances in the right form
  itk::Instance< itk::SquareImageFilter <MaskImType, DistImType> > Square;
  Square->SetInput(dilsize);

  writeIm<DistImType>(Square->GetOutput(), "square.mha");
  // dilate this with parabolic SE
  itk::Instance< itk::ParabolicDilateLabelPropImageFilter<DistImType, MaskImType, true, MaskImType> > Dilate;
  Dilate->SetInput(Square->GetOutput());
  Dilate->SetMarkerImage(mask);
  Dilate->SetScale(0.5);

  writeIm<MaskImType>(Dilate->GetOutput(), CmdLineObj.OutputIm);
  Dilate->writeDist("/tmp/dist.mhd");
  Dilate->writeCone("/tmp/cone.mhd");

}

/////////////////////////////////

int main(int argc, char * argv[])
{

  int dim1, dim2 = 0;
  CmdLineType CmdLineObj;
  ParseCmdLine(argc, argv, CmdLineObj);

  itk::MultiThreader::SetGlobalMaximumNumberOfThreads(1);

  itk::ImageIOBase::IOComponentType ComponentType, SizeComponentType;

  if (!readImageInfo(CmdLineObj.InputIm, &ComponentType, &dim1))
    {
    std::cerr << "Failed to open " << CmdLineObj.InputIm << std::endl;
    return(EXIT_FAILURE);
    }
  if (!readImageInfo(CmdLineObj.SizeIm, &SizeComponentType, &dim2))
    {
    std::cerr << "Failed to open " << CmdLineObj.SizeIm << std::endl;
    return(EXIT_FAILURE);
    }

  if (dim1 != dim2)
    {
    std::cerr << "Image dimensions must match " << dim1 << " " << dim2 << std::endl;
    return(EXIT_FAILURE);

    }

  switch (dim1)
    {
    case 2:
      doDilate<unsigned char, unsigned char, 2>(CmdLineObj);
      break;
    case 3:
      doDilate<unsigned char, unsigned char, 3>(CmdLineObj);
      break;
    default:
      std::cerr << "Unsupported dimension" << std::endl;
      return (EXIT_FAILURE);
      break;
    }
  return EXIT_SUCCESS;
}
