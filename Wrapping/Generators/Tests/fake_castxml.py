"""Instrumented castxml stand-in driven by env vars (see conftest.FakeCastxml)."""

import os
import sys


def main():
    argv = sys.argv[1:]
    with open(os.environ["FAKE_LOG"], "a") as log:
        log.write(("PRE" if "-E" in argv else "FULL") + "\n")

    if os.environ.get("FAKE_FAIL") == "1":
        return 1

    out = None
    for i, a in enumerate(argv):
        if a == "-o" and i + 1 < len(argv):
            out = argv[i + 1]

    hdrs = [p for p in os.environ.get("FAKE_HDRS", "").split(os.pathsep) if p]
    ver = os.environ.get("FAKE_CASTXML_VERSION", "1")

    if "-E" in argv:
        with open(out, "wb") as f:
            marker_root = os.environ.get("FAKE_MARKER_ROOT", "")
            f.write(b'# 1 "' + (marker_root + "/module.cxx").encode() + b'"\n')
            body = b""
            for h in hdrs:
                f.write(f'# 1 "{h}"\n'.encode())
                with open(h, "rb") as hf:
                    body += hf.read()
            f.write(body)
            f.write(b"int payload;\n")
    else:
        with open(out, "wb") as f:
            content = b"".join(open(h, "rb").read() for h in hdrs)
            f.write(b"<xml castxml-version='" + ver.encode() + b"' body='")
            f.write(content.replace(b"\n", b";"))
            f.write(b"'/>\n")
    return 0


if __name__ == "__main__":
    sys.exit(main())
