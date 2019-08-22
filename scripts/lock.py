import subprocess
import os
import mss
import sys

from PIL import Image
from screeninfo import get_monitors

monitors = get_monitors()

with mss.mss() as sct:

    # calc x11 root window dimensions
    max_x = max(map(lambda x: x.x, monitors))
    mon_max_x = list(filter(lambda x: x.x == max_x, monitors))
    mon_max_width = max(map(lambda x: x.width, mon_max_x))
    root_w = max_x + mon_max_width

    max_y = max(map(lambda y: y.y, monitors))
    mon_max_y = list(filter(lambda y: y.y == max_y, monitors))
    mon_max_height = max(map(lambda y: y.height, mon_max_y))
    root_h = max_y + mon_max_height

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
