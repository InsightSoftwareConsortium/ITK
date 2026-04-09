"""
Comprehensive safety tests for Python GIL release during ITK operations.

When SWIG's -threads flag is enabled (ITK_PYTHON_RELEASE_GIL=ON), the GIL is
released before entering C++ code and re-acquired after return.  This enables
true concurrent execution but introduces risks that these tests verify against.

Each test targets a specific known gotcha from the SWIG -threads literature:

  Test 1 - Callback safety
    SWIG director methods do not automatically re-acquire the GIL before
    calling back into Python.  ITK handles this via PyGILState_Ensure in
    itkPyCommand.cxx, but any new callback path must do the same.
    References:
      - https://github.com/swig/swig/issues/3091 (director segfault 3.12+)
      - https://github.com/swig/swig/issues/2670 (director GIL not acquired)
      - SimpleITK sitkPyCommand.cxx GIL handling pattern

  Test 2 - Concurrent callbacks
    Multiple threads executing filters with Python observers simultaneously.
    Verifies that PyGILState_Ensure/Release is re-entrant and that the
    callback mechanism is thread-safe.
    Reference:
      - https://docs.python.org/3/c-api/init.html#non-python-created-threads

  Test 3 - Exception propagation across GIL boundary
    When -threads is enabled, SWIG wraps C++ calls in SaveThread/RestoreThread.
    If C++ throws, the thread state must be restored correctly before the
    Python exception is set.  Older SWIG versions crashed here.
    Reference:
      - https://swig-devel.narkive.com/PBt1fCLN (exception + threads segfault)

  Test 4 - Object destruction under concurrent access
    ITK SmartPointers prevent premature C++ object deletion, but Python
    reference counting must be correct when the GIL is released.  Releasing
    the GIL during destructor calls (delete arg1) is particularly risky.
    Reference:
      - https://docs.python.org/3/c-api/refcounting.html

  Test 5 - Concurrent image processing (data race detection)
    Multiple threads each operating on independent images should produce
    correct results without data races in ITK's internal state.
    Reference:
      - ITK's MultiThreaderBase ensures thread safety for filter internals

  Test 6 - Signal handling while GIL is released
    Python signal handlers (including Ctrl-C) are only delivered when the
    GIL is held.  Long C++ operations make the process appear unresponsive.
    This test verifies signals are eventually delivered.
    Reference:
      - https://docs.python.org/3/library/signal.html#signals-and-threads

  Test 7 - GIL reacquisition deadlock detection
    A Python callback invoked from C++ (which released the GIL) must
    re-acquire the GIL via PyGILState_Ensure.  If the implementation uses
    a different GIL acquisition path, or if there is a lock ordering issue,
    this can deadlock.
    Reference:
      - https://docs.python.org/3/c-api/init.html#PyGILState_Ensure

  Test 8 - Threading stress test
    Rapid create/filter/destroy cycles across multiple threads to detect
    intermittent crashes from reference counting races, memory corruption,
    or thread-unsafe SWIG runtime state.
    Reference:
      - https://github.com/swig/swig/issues/2396 (crash with -threads)
      - https://github.com/swig/swig/issues/3121 (free-threaded Python)
"""

import gc
import os
import signal
import sys
import threading
import time
import weakref

import itk
import numpy as np

passed = 0
failed = 0
skipped = 0


def check(name, condition, detail=""):
    global passed, failed
    if condition:
        passed += 1
    else:
        failed += 1
        print(f"  FAIL: {name}" + (f" ({detail})" if detail else ""))


def skip(name, reason):
    global skipped
    skipped += 1
    print(f"  SKIP: {name} ({reason})")


def make_image(size=None, fill=1.0, pixel_type=itk.F):
    if size is None:
        size = [64, 64]
    img = itk.Image[pixel_type, len(size)].New()
    img.SetRegions(size)
    img.Allocate()
    img.FillBuffer(fill)
    return img


# ==================================================================
#  Test 1: Callback safety during filter execution
#  Ref: swig/swig#3091, swig/swig#2670
# ==================================================================
print("=" * 60)
print("Test 1: Callback safety during filter execution")
print("=" * 60)

callback_called = []
callback_errors = []


def progress_callback():
    """Python callback invoked from C++ during filter.Update().

    itkPyCommand.cxx re-acquires the GIL via PyGILState_Ensure before
    calling this function.  If it didn't, this would segfault.
    """
    try:
        callback_called.append(threading.current_thread().name)
        # Exercise Python runtime to verify GIL is held
        _ = [i * 2 for i in range(100)]
    except Exception as e:
        callback_errors.append(str(e))


image = make_image([128, 128])
median = itk.MedianImageFilter.New(image, Radius=2)
median.AddObserver(itk.ProgressEvent(), lambda *a: progress_callback())
median.Update()
check(
    "callback invoked during filter",
    len(callback_called) > 0,
    f"called {len(callback_called)} times",
)
check("no callback errors", len(callback_errors) == 0, f"errors: {callback_errors}")
del median, image

# ==================================================================
#  Test 2: Concurrent callbacks from threaded filters
#  Ref: https://docs.python.org/3/c-api/init.html#non-python-created-threads
# ==================================================================
print()
print("=" * 60)
print("Test 2: Concurrent callbacks from threaded filters")
print("=" * 60)

concurrent_callback_calls = []
concurrent_lock = threading.Lock()
concurrent_errors = []


def threaded_progress_callback():
    try:
        with concurrent_lock:
            concurrent_callback_calls.append(threading.current_thread().name)
        # Verify Python objects are accessible under GIL
        result = sum(range(50))
    except Exception as e:
        with concurrent_lock:
            concurrent_errors.append(f"{threading.current_thread().name}: {e}")


def run_filter_with_callback():
    try:
        img = make_image([100, 100])
        filt = itk.MedianImageFilter.New(img, Radius=3)
        filt.SetNumberOfWorkUnits(1)
        filt.AddObserver(itk.ProgressEvent(), lambda *a: threaded_progress_callback())
        filt.Update()
    except Exception as e:
        with concurrent_lock:
            concurrent_errors.append(f"{threading.current_thread().name}: {e}")


threads = [
    threading.Thread(target=run_filter_with_callback, name=f"cb-{i}") for i in range(4)
]
for t in threads:
    t.start()
for t in threads:
    t.join(timeout=30)

check(
    "concurrent callbacks succeeded",
    len(concurrent_errors) == 0,
    f"errors: {concurrent_errors}",
)
check(
    "callbacks invoked from threads",
    len(concurrent_callback_calls) > 0,
    f"called {len(concurrent_callback_calls)} times",
)

# ==================================================================
#  Test 3: Exception propagation across GIL boundary
#  Ref: swig-devel thread on exception + threads segfault
# ==================================================================
print()
print("=" * 60)
print("Test 3: Exception propagation across GIL boundary")
print("=" * 60)

# itk.imread on a non-existent file throws RuntimeError through the
# SWIG wrapper.  With -threads, the GIL is released before the C++ call
# and must be re-acquired before setting the Python exception.
try:
    itk.imread("/nonexistent/path/that/does/not/exist.nii.gz")
    check("exception from imread", False, "should have raised")
except RuntimeError:
    check("C++ RuntimeError propagated correctly", True)
except Exception as e:
    check("exception propagation", True, f"got {type(e).__name__}")

# Exception from a threaded context
exception_results = []


def threaded_exception():
    try:
        itk.imread("/nonexistent/threaded.nii.gz")
        exception_results.append("no_exception")
    except RuntimeError:
        exception_results.append("ok")
    except Exception as e:
        exception_results.append(f"wrong: {type(e).__name__}")


threads = [threading.Thread(target=threaded_exception) for _ in range(4)]
for t in threads:
    t.start()
for t in threads:
    t.join(timeout=15)

check(
    "exceptions propagated in all 4 threads",
    all(r == "ok" for r in exception_results),
    f"results: {exception_results}",
)

# ==================================================================
#  Test 4: Object destruction under concurrent access
#  Ref: Python C API refcounting docs
# ==================================================================
print()
print("=" * 60)
print("Test 4: Object destruction under concurrent access")
print("=" * 60)

destruction_errors = []


def destroy_while_filtering():
    try:
        # Each thread creates and destroys its own objects
        def slow_filter():
            try:
                local_img = make_image([128, 128])
                filt = itk.MedianImageFilter.New(local_img, Radius=5)
                filt.SetNumberOfWorkUnits(1)
                filt.Update()
                del filt, local_img
                gc.collect()
            except Exception as e:
                destruction_errors.append(f"filter: {e}")

        threads = [threading.Thread(target=slow_filter) for _ in range(4)]
        for t in threads:
            t.start()
        for t in threads:
            t.join(timeout=30)
    except Exception as e:
        destruction_errors.append(f"main: {e}")


destroy_while_filtering()
check(
    "no crashes during concurrent destroy",
    len(destruction_errors) == 0,
    f"errors: {destruction_errors}",
)

# ==================================================================
#  Test 5: Concurrent image processing (data race detection)
#  Ref: ITK MultiThreaderBase thread safety
# ==================================================================
print()
print("=" * 60)
print("Test 5: Concurrent image processing (data races)")
print("=" * 60)

race_errors = []


def concurrent_read_write():
    try:
        images = [make_image([32, 32], fill=float(i)) for i in range(4)]
        results = [None] * 4

        def process(idx):
            try:
                filt = itk.MedianImageFilter.New(images[idx], Radius=1)
                filt.SetNumberOfWorkUnits(1)
                filt.Update()
                results[idx] = itk.array_from_image(filt.GetOutput())[0, 0]
            except Exception as e:
                race_errors.append(f"thread {idx}: {e}")

        threads = [threading.Thread(target=process, args=(i,)) for i in range(4)]
        for t in threads:
            t.start()
        for t in threads:
            t.join(timeout=30)

        for i, r in enumerate(results):
            if r is not None:
                check(
                    f"thread {i} result correct",
                    abs(r - float(i)) < 0.01,
                    f"expected {float(i)}, got {r}",
                )
    except Exception as e:
        race_errors.append(f"setup: {e}")


concurrent_read_write()
check(
    "no race condition errors",
    len(race_errors) == 0,
    f"errors: {race_errors}",
)

# ==================================================================
#  Test 6: Signal handling while GIL is released
#  Ref: https://docs.python.org/3/library/signal.html#signals-and-threads
# ==================================================================
print()
print("=" * 60)
print("Test 6: Signal handling while GIL is released")
print("=" * 60)

if sys.platform != "win32":
    signal_received = []

    def signal_handler(signum, frame):
        signal_received.append(signum)

    old_handler = signal.signal(signal.SIGUSR1, signal_handler)

    def run_and_signal():
        img = make_image([200, 200])
        filt = itk.MedianImageFilter.New(img, Radius=5)
        filt.SetNumberOfWorkUnits(1)

        def send_signal():
            time.sleep(0.01)
            os.kill(os.getpid(), signal.SIGUSR1)

        sig_thread = threading.Thread(target=send_signal)
        sig_thread.start()
        filt.Update()
        sig_thread.join(timeout=5)

    run_and_signal()
    check("signal received during filter execution", len(signal_received) > 0)

    signal.signal(signal.SIGUSR1, old_handler)
else:
    skip("signal handling", "SIGUSR1 not supported on Windows")

# ==================================================================
#  Test 7: GIL reacquisition deadlock detection
#  Ref: https://docs.python.org/3/c-api/init.html#PyGILState_Ensure
# ==================================================================
print()
print("=" * 60)
print("Test 7: GIL reacquisition deadlock detection")
print("=" * 60)

DEADLOCK_TIMEOUT = 10  # seconds
deadlock_result = [None]


def test_deadlock():
    """Test that callbacks re-acquiring GIL don't deadlock.

    During filter.Update(), the GIL is released (SWIG -threads).
    The ITK progress callback calls PyGILState_Ensure to re-acquire
    the GIL before invoking the Python callable.  If there is a lock
    ordering issue, this deadlocks and the test times out.
    """
    try:
        img = make_image([64, 64])
        filt = itk.MedianImageFilter.New(img, Radius=2)

        def callback_that_does_python_work():
            arr = np.zeros(10)
            _ = arr.sum()

        filt.AddObserver(
            itk.ProgressEvent(), lambda *a: callback_that_does_python_work()
        )
        filt.Update()
        deadlock_result[0] = "success"
    except Exception as e:
        deadlock_result[0] = f"error: {e}"


t = threading.Thread(target=test_deadlock)
t.start()
t.join(timeout=DEADLOCK_TIMEOUT)

if t.is_alive():
    check(
        "no deadlock in GIL reacquisition",
        False,
        f"test timed out after {DEADLOCK_TIMEOUT}s -- possible deadlock",
    )
else:
    check(
        "no deadlock in GIL reacquisition",
        deadlock_result[0] == "success",
        deadlock_result[0],
    )

# ==================================================================
#  Test 8: Threading stress test (rapid create/filter/destroy)
#  Ref: swig/swig#2396 (crash with -threads and multithreading)
#       swig/swig#3121 (free-threaded Python SWIG runtime races)
# ==================================================================
print()
print("=" * 60)
print("Test 8: Threading stress test (rapid create/destroy)")
print("=" * 60)

stress_errors = []


def stress_create_destroy():
    for _ in range(50):
        try:
            img = make_image([16, 16])
            arr = np.asarray(img)
            filt = itk.MedianImageFilter.New(img, Radius=1)
            filt.Update()
            out = filt.GetOutput()
            del filt, img, arr, out
        except Exception as e:
            stress_errors.append(str(e))


threads = [threading.Thread(target=stress_create_destroy) for _ in range(4)]
for t in threads:
    t.start()
for t in threads:
    t.join(timeout=60)

gc.collect()
check(
    "stress test: no errors in 200 create/destroy cycles",
    len(stress_errors) == 0,
    f"errors: {stress_errors[:3]}",
)

# ==================================================================
#  Summary
# ==================================================================
print()
print("=" * 60)
print(f"Python {sys.version_info[0]}.{sys.version_info[1]}.{sys.version_info[2]}")
print(f"NumPy {np.__version__}")
print(f"TOTAL: {passed} passed, {failed} failed, {skipped} skipped")
print("=" * 60)

if failed > 0:
    print("GIL RELEASE SAFETY TESTS FAILED!")
    sys.exit(1)
else:
    print("All GIL release safety tests passed.")
