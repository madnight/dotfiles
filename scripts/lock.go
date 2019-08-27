package main

import (
    "github.com/nfnt/resize"
    "github.com/vova616/screenshot"
    "image/png"
    "image/draw"
    "image"
    "os"
    "bytes"
    "os/exec"
)

func main() {
    // read lock icon file
    fdLock, _ := os.Open("/home/x/scripts/lock.png")
    pngLock, _ := png.Decode(fdLock)
    lock := image.Image(pngLock)

    // make screenshot
    raw_root, _ := screenshot.CaptureScreen()
    screenshot := image.Image(raw_root)

    // pixelate
    screenshot = resize.Resize(1920/10, 0, screenshot, resize.NearestNeighbor)
    screenshot = resize.Resize(1920, 0, screenshot, resize.NearestNeighbor)

    // paste lock onto screenshot
    bg := image.NewRGBA(screenshot.Bounds())
    draw.Draw(bg, screenshot.Bounds(), screenshot, image.ZP, draw.Src)
    draw.Draw(bg, lock.Bounds().Add(image.Pt(600, 400)), lock, image.ZP, draw.Over)

    // convert image to byte buffer
    var b bytes.Buffer
    b.Write([]byte(bg.Pix))

    // os.Exit(0)
    // run i3lock with raw byte buffer as stdin
    i3 := exec.Command("i3lock", "--raw", "1920x1080:rgbx", "-i", "/dev/stdin")
    i3.Stdin = &b
    i3.Run()
}
