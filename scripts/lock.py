import subprocess
import mss
import sys

from PIL import Image

sct = mss.mss()

# take screenshot
sct_img = sct.grab(sct.monitors[0])
img = Image.frombytes("RGB", sct_img.size, sct_img.bgra, "raw", "BGRX")

# pixelate
result = img.resize(
    (int(img.size[0] / 10), int(img.size[1] / 10)), resample=Image.BILINEAR
).resize(img.size, Image.NEAREST)

# open lock icon
icon = Image.open(sys.argv[1])

# calc lock icon position on primary display
primary = sct.monitors[1]
area = (
    int(((primary['width'] / 2) + primary['left']) - icon.size[0] / 2),
    int(((primary['height'] / 2) + primary['top']) - icon.size[1] / 2),
)

# add lock icon to bg image
result.paste(icon, area, icon)

# pipe raw rgb result to i3lock
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
