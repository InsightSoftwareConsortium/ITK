
#     INPUTS: {BrainProtonDensitySlice.png}
#     OUTPUTS: {ResampleImageFilterOutput1.png}
#     0

#     INPUTS: {BrainProtonDensitySlice.png}
#     OUTPUTS: {ResampleImageFilterOutput2.png}
#     1

#     INPUTS: {BrainProtonDensitySlice.png}
#     OUTPUTS: {ResampleImageFilterOutput3.png}
#     2

#     INPUTS: {BrainProtonDensitySlice.png}
#     OUTPUTS: {ResampleImageFilterOutput4.png}
#     3

import itk
from sys import argv, stderr, exit

itk.auto_progress(2)

# if( len(argv) < 3 ):
#   print >> stderr, """Missing Parameters
# Usage: ResampleImageFilter.py inputImageFile  outputImageFile [exampleAction={0,1,2,3}]"""
#   exit(1)

dim = 2
SOType = itk.SpatialObject[dim]
InternalImageType = itk.Image[itk.F, dim]
OutputPixelType = itk.UC
OutputImageType = itk.Image[OutputPixelType, dim]

ellipse = itk.EllipseSpatialObject[dim].New( Radius=[10,5] )
ellipse.GetObjectToParentTransform().SetOffset( [20,20] )
ellipse.ComputeObjectToWorldTransform()

box = itk.BoxSpatialObject[dim].New( Size=20 )
box.GetObjectToParentTransform().SetOffset( [20,40] )
box.ComputeObjectToWorldTransform()

gaussian = itk.GaussianSpatialObject[dim].New( Radius=100 )
gaussian.GetObjectToParentTransform().SetOffset( [60,60] )
gaussian.GetObjectToParentTransform().SetScale( 10 )
gaussian.ComputeObjectToWorldTransform()

group = itk.GroupSpatialObject[dim].New()
group.AddSpatialObject( ellipse.GetPointer() )
group.AddSpatialObject( box.GetPointer() )
group.AddSpatialObject( gaussian.GetPointer() )

filter = itk.SpatialObjectToImageFilter[SOType, InternalImageType].New( group, Size=[100,100], UseObjectValue=True )
filter.Update() # required ?!

rescale = itk.RescaleIntensityImageFilter[InternalImageType, OutputImageType].New( filter, OutputMinimum=itk.NumericTraits[OutputPixelType].NonpositiveMin(), OutputMaximum=itk.NumericTraits[OutputPixelType].max() )

itk.write(rescale, argv[1])
