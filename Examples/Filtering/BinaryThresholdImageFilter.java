/**
 *  Example on the use of the BinaryThresholdImageFilter
 *
 */

import InsightToolkit.*;

public class BinaryThresholdImageFilter
{
  public static void main( String argv[] )
  {
    System.out.println("BinaryThresholdImageFilter Example");

    itkImageFileReaderUS2_Pointer reader = itkImageFileReaderUS2.itkImageFileReaderUS2_New();
    itkImageFileWriterUS2_Pointer writer = itkImageFileWriterUS2.itkImageFileWriterUS2_New();

    itkBinaryThresholdImageFilterUS2US2_Pointer filter = itkBinaryThresholdImageFilterUS2US2.itkBinaryThresholdImageFilterUS2US2_New();

    filter.SetInput( reader.GetOutput() );
    writer.SetInput( filter.GetOutput() );

    reader.SetFileName( argv[0] );
    writer.SetFileName( argv[1] );

    filter.SetLowerThreshold(  Integer.parseInt( argv[2] )  );
    filter.SetUpperThreshold(  Integer.parseInt( argv[3] )  );

    filter.SetOutsideValue( Integer.parseInt( argv[4] )  );
    filter.SetInsideValue(  Integer.parseInt( argv[5] )  );

    writer.Update();
  }

}


