/**
 *  Example on the use of the MeanImageFilter
 *
 */

import InsightToolkit.*;


public class MeanImageFilter
{
  public static void main( String argv[] )
  {
    System.out.println("MeanImageFilter Example");

    itkImageFileReaderUS2_Pointer reader = itkImageFileReaderUS2.itkImageFileReaderUS2_New();
    itkImageFileWriterUS2_Pointer writer = itkImageFileWriterUS2.itkImageFileWriterUS2_New();

    itkMeanImageFilterUS2US2_Pointer filter = itkMeanImageFilterUS2US2.itkMeanImageFilterUS2US2_New();

    filter.SetInput( reader.GetOutput() );
    writer.SetInput( filter.GetOutput() );

    System.out.println("Reading input image"+argv[0]);
    reader.SetFileName( argv[0] );
    reader.Update();

    System.out.println("Running Mean filter");
    filter.Update();

    System.out.println("Writin output image"+argv[1]);
    writer.SetFileName( argv[1] );

    writer.Update();
  }

}


