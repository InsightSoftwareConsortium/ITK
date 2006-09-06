
import itk

itk.MultiThreader.SetGlobalDefaultNumberOfThreads(3)
cast = itk.CastImageFilter.IUC2IUC2.New()

assert itk.MultiThreader.GetGlobalDefaultNumberOfThreads() == 3
assert cast.GetNumberOfThreads() == 3



itk.MultiThreader.SetGlobalDefaultNumberOfThreads(20)
cast = itk.CastImageFilter.IUC2IUC2.New()

assert itk.MultiThreader.GetGlobalDefaultNumberOfThreads() == 20
assert cast.GetNumberOfThreads() == 20
