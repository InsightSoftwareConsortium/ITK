/**
 *  Example on the use of the ThresholdImageFilter
 *
 */

import InsightToolkit.*;

public class ThresholdImageFilter
{
  public static void main( String argv[] )
  {
    System.out.println("ThresholdImageFilter Example");

    itkImageFileReaderUS2_Pointer reader = itkImageFileReaderUS2.itkImageFileReaderUS2_New();
    itkImageFileWriterUS2_Pointer writer = itkImageFileWriterUS2.itkImageFileWriterUS2_New();

    itkThresholdImageFilterUS2_Pointer filter = itkThresholdImageFilterUS2.itkThresholdImageFilterUS2_New();

    filter.SetInput( reader.GetOutput() );
    writer.SetInput( filter.GetOutput() );

    reader.SetFileName( argv[0] );
    writer.SetFileName( argv[1] );

    filter.SetOutsideValue( Integer.parseInt( argv[2] )  );
    filter.ThresholdAbove(  Integer.parseInt( argv[3] )  );

    writer.Update();
  }

}


