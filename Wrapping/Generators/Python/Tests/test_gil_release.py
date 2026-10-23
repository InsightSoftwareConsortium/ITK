import sys
import threading

import itk


def test_gil_release():
    image_type = itk.Image[itk.F, 2]
    image = image_type.New()
    image.SetRegions([512, 512])
    image.Allocate()
    image.FillBuffer(1.0)
    median = itk.MedianImageFilter[image_type, image_type].New(Input=image, Radius=5)
    median.SetNumberOfWorkUnits(1)

    for operation in (median.Update, median.UpdateLargestPossibleRegion):
        median.Modified()
        ready = threading.Event()
        start = threading.Event()
        progressed = threading.Event()

        def worker():
            ready.set()
            start.wait()
            progressed.set()

        thread = threading.Thread(target=worker)
        thread.start()
        ready.wait()
        switch_interval = sys.getswitchinterval()
        try:
            # Prevent Python bytecode scheduling from masquerading as native GIL release.
            sys.setswitchinterval(100)
            start.set()
            operation()
            released = progressed.is_set()
        finally:
            sys.setswitchinterval(switch_interval)
            start.set()
            thread.join()

        assert released, f"{operation.__name__} did not release the GIL"


if __name__ == "__main__":
    test_gil_release()
