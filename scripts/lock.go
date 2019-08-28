package main

import (
    "github.com/vova616/screenshot"
    "github.com/disintegration/gift"
    "image/png"
    "image"
    "os"
    "bytes"
    "os/exec"
)

func main() {

    lockChan := make(chan image.Image)

    // read lock icon file
    go func() {
        fdLock, _ := os.Open("/home/x/scripts/lock.png")
        pngLock, _ := png.Decode(fdLock)
        lockChan <- image.Image(pngLock)
    }()

    // make screenshot
    raw_root, _ := screenshot.CaptureScreen()

    // pixelate
    gift.New(gift.Pixelate(10)).Draw(raw_root, raw_root)

    // paste lock onto screenshot
    gift.New().DrawAt(raw_root, <-lockChan, image.Pt(600, 400), gift.OverOperator)

    os.Exit(0)
    // run i3lock with raw byte buffer as stdin
    i3 := exec.Command("i3lock", "--raw", "1920x1080:rgbx", "-i", "/dev/stdin")
    i3.Stdin = bytes.NewBuffer([]byte(raw_root.Pix))
    i3.Run()
}
