from concurrent.futures import ThreadPoolExecutor
import sys
import threading
import time

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


def test_concurrent_filters():
    image_type = itk.Image[itk.F, 2]
    filters = []
    for _ in range(4):
        image = image_type.New()
        image.SetRegions([512, 512])
        image.Allocate()
        image.FillBuffer(1.0)
        median = itk.MedianImageFilter[image_type, image_type].New(
            Input=image, Radius=5
        )
        median.SetNumberOfWorkUnits(1)
        filters.append(median)

    operations = [median.Update for median in filters]
    ready = threading.Barrier(len(operations), timeout=30)

    def run_filter(operation):
        ready.wait()
        start = time.perf_counter()
        operation()
        end = time.perf_counter()
        return start, end

    switch_interval = sys.getswitchinterval()
    try:
        # Exclude Python thread switching between timestamps and serialized native calls.
        sys.setswitchinterval(100)
        with ThreadPoolExecutor(max_workers=len(operations)) as executor:
            intervals = sorted(executor.map(run_filter, operations))
    finally:
        sys.setswitchinterval(switch_interval)

    assert any(
        current[0] < previous[1] for previous, current in zip(intervals, intervals[1:])
    ), "Independent filter updates did not overlap"


if __name__ == "__main__":
    test_gil_release()
    test_concurrent_filters()
