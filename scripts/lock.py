import subprocess
import os
import mss
import sys

from PIL import Image
from screeninfo import get_monitors

monitors = get_monitors()

with mss.mss() as sct:

    root_w = sum(map(lambda x: x.width, monitors))
    root_h = max(map(lambda x: x.height, monitors))

    # take screenshot
    monitor = {"top": 0, "left": 0, "width": root_w, "height": root_h}
    sct_img = sct.grab(monitor)
    img = Image.frombytes("RGB", sct_img.size, sct_img.bgra, "raw", "BGRX")

    # pixelate
    imgSmall = img.resize(
        (int(img.size[0] / 10), int(img.size[1] / 10)), resample=Image.BILINEAR
    )

    result = imgSmall.resize(img.size, Image.NEAREST)
    icon = Image.open(sys.argv[1])

    # calc lock icon position on primary display
    primary = monitors[0]
    area = (
        int(((primary.width / 2) + primary.x) - icon.size[0] / 2),
        int(((primary.height / 2) + primary.y) - icon.size[1] / 2),
    )

    # add lock icon to bg image
    result.paste(icon, area, icon)

    subprocess.run(
        [
            "i3lock",
            "--raw",
            str(result.width) + "x" + str(result.height) + ":rgb",
            "-i",
            "/dev/stdin",
        ],
        stdout=subprocess.PIPE,
        input=result.tobytes(),
    )
